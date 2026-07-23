library(here)
library(dplyr)
library(data.table)

# LOAD DATA ------------------------------------------------------------------
clean <- readRDS(here("2_clean_panel", "output", "clean.rds"))
data <- clean


# RESHAPE DATA FROM INDIVIDUAL-YEARS TO FAMILY-YEARS --------------------------
data <- data %>%
  mutate(
    role_tag = case_when(
      relation == 1 ~ "head",
      relation == 2 ~ "spouse",
      relation == 3 ~ "child",
      relation == 4 ~ "other",
      relation == 0 ~ "unknown",
      TRUE ~ "unknown"
    )
  ) %>%
  group_by(fam_id, year, role_tag) %>%
  mutate(
    role_num = row_number(),
    role_id = paste0(role_tag, "_", role_num)
  ) %>%
  ungroup() %>%
  filter(relation != 0) %>%
  select(-role_tag, -role_num, -pn, -sequence)


# CREATE HOUSEHOLD-SUMMARY VARIABLES ------------------------------------------
data <- data %>%
  group_by(year, fam_id) %>%
  mutate(
    # Household counts of children and other family members
    hh_children = sum(relation == 3, na.rm = TRUE),
    hh_other = sum(relation == 4, na.rm = TRUE),

    # Number of household members classified as head or spouse
    hh_headspouse = sum(relation %in% c(1, 2), na.rm = TRUE),

    # Equals 1 when either a head or spouse is present, but not both
    hh_single = as.integer(
      xor(
        any(relation == 1, na.rm = TRUE),
        any(relation == 2, na.rm = TRUE)
      )
    ),

    # Household age totals used to construct the person-weighted clan mean age
    hh_age_sum = sum(age, na.rm = TRUE),
    hh_age_n = sum(!is.na(age))
  ) %>%
  ungroup()


# FORMAT AS DATA.TABLE AND SEPARATE FAMILY AND INDIVIDUAL DATA ----------------
dt <- as.data.table(data)

id_vars <- c("year", "fam_id")

fam_vars <- c(
  "ind_top_inc_all", "ind_top_inc_tax_hs", "ind_top_inc_tax_o",
  "ind_top_inc_trans_hs", "ind_top_inc_trans_o1", "ind_top_inc_trans_o2",
  "ind_top_wealth_nohouse", "ind_top_wealth", "ind_top_wealth_home",
  "inc_all", "inc_tax_hs", "inc_tax_o",
  "inc_trans_hs", "inc_trans_o1", "inc_trans_o2",
  "wealth_nohouse", "wealth", "wealth_home",
  "release", "numfu", "fam_weight",
  "hh_children", "hh_other", "hh_headspouse", "hh_single",
  "hh_age_sum", "hh_age_n"
)

ind_vars <- c(
  "id1968", "sample", "stratum", "cluster", "relation",
  "race_year", "race", "black"
)


# CREATE FAMILY-LEVEL DATA ----------------------------------------------------
dt_fam <- dt[, c("year", "fam_id", "pid", fam_vars), with = FALSE]

fam_wide <- dt_fam %>%
  group_by(year, fam_id) %>%
  slice(1) %>%
  ungroup()

rm(dt_fam)


# CREATE INDIVIDUAL-LEVEL DATA ------------------------------------------------
long_dt <- melt(
  dt,
  id.vars = c(id_vars, "pid", "role_id"),
  measure.vars = ind_vars,
  variable.name = "var"
)

# Create role tag for pivot to wide
long_dt[, var_role := paste0(var, "_", role_id)]

# Cast to wide
ind_wide <- dcast(
  long_dt,
  year + fam_id ~ var_role,
  value.var = "value"
)


# CLEAN HOUSEHOLD FILE --------------------------------------------------------
# Remove columns with the same values for all roles within a family
# (i.e. adopt the value for the head)

ind_wide <- ind_wide %>%
  select(
    -starts_with("id1968_child"),
    -starts_with("id1968_other"),
    -starts_with("stratum_spouse"),
    -starts_with("stratum_child"),
    -starts_with("stratum_other"),
    -starts_with("cluster_spouse"),
    -starts_with("cluster_child"),
    -starts_with("cluster_other"),
    -starts_with("sample_child"),
    -starts_with("sample_other"),
    -starts_with("race_year_child"),
    -starts_with("race_year_other"),
    -starts_with("race_child"),
    -starts_with("race_other"),
    -starts_with("black_child"),
    -starts_with("black_other")
  ) %>%
  rename(
    id1968 = id1968_head_1,
    id1968_spouse = id1968_spouse_1,
    stratum = stratum_head_1,
    cluster = cluster_head_1,
    sample_head = sample_head_1,
    sample_spouse = sample_spouse_1,
    race_year_head = race_year_head_1,
    race_year_spouse = race_year_spouse_1,
    race_head = race_head_1,
    race_spouse = race_spouse_1,
    black_head = black_head_1,
    black_spouse = black_spouse_1
  )


# MERGE FAMILY AND INDIVIDUAL DATA --------------------------------------------
households <- merge(
  fam_wide,
  ind_wide,
  by = c("year", "fam_id"),
  all.x = TRUE
)

# Remove columns that are not needed for now
households <- households %>%
  select(
    -starts_with("ind_top_"),
    -starts_with("race_year_")
  )


# DEAL WITH HOUSEHOLDS THAT BELONG TO MULTIPLE CLANS --------------------------
# Check that clan IDs are shared between heads and spouses
# Save mismatched cases

mismatched <- households %>%
  filter(
    !is.na(id1968) &
      !is.na(id1968_spouse) &
      id1968 != id1968_spouse
  )

# Duplicate mismatched cases to assign to both clans
households_keep <- households %>%
  filter(
    is.na(id1968) |
      is.na(id1968_spouse) |
      id1968 == id1968_spouse
  )

head <- mismatched

spouse <- head %>%
  mutate(
    id1968_mis = id1968,
    id1968 = id1968_spouse,
    id1968_spouse = id1968_mis
  ) %>%
  select(-id1968_mis)

households_dual_clan <- bind_rows(
  households_keep, # Unchanged: matched or missing
  head,            # Original rows: head's clan
  spouse           # Duplicated rows: spouse's clan
) %>%
  arrange(year, fam_id)

dup_counts <- households_dual_clan %>%
  count(year, fam_id, name = "n") %>%
  filter(n > 1)

if (nrow(dup_counts) != nrow(mismatched)) {
  warning(
    "Mismatch in duplication count: ",
    "duplicated fam_id-year = ", nrow(dup_counts),
    " vs mismatched rows = ", nrow(mismatched)
  )
}

over_duped <- dup_counts %>%
  filter(n > 2)

if (nrow(over_duped) > 0) {
  warning(
    "Some fam_id-year combinations have more than two rows:\n",
    paste0(capture.output(print(over_duped)), collapse = "\n")
  )
}

households <- households_dual_clan

rm(
  head,
  spouse,
  households_dual_clan,
  households_keep,
  dup_counts,
  over_duped
)


# ADD CLAN DATA TO HOUSEHOLDS -------------------------------------------------
households <- households %>%
  group_by(year, id1968) %>%
  arrange(fam_id, .by_group = TRUE) %>%
  mutate(
    # Household number within the clan-year
    hh_number = row_number(),

    # Number of households and total people in the clan
    numclan = n(),
    num_clan_people = sum(numfu, na.rm = TRUE),

    # Average age of clan members with an observed age
    clan_age_mean = if_else(
      sum(hh_age_n, na.rm = TRUE) > 0,
      sum(hh_age_sum, na.rm = TRUE) /
        sum(hh_age_n, na.rm = TRUE),
      NA_real_
    ),

    # Average number of children per household within the clan
    clan_children_hh_mean = mean(
      hh_children,
      na.rm = TRUE
    ),

    # Average number of other family members per household within the clan
    clan_other_hh_mean = mean(
      hh_other,
      na.rm = TRUE
    ),

    # Proportion of all clan members classified as other family members
    clan_other_prop = if_else(
      num_clan_people > 0,
      sum(hh_other, na.rm = TRUE) / num_clan_people,
      NA_real_
    ),

    # Proportion of all clan members classified as head or spouse
    clan_headspouse_prop = if_else(
      num_clan_people > 0,
      sum(hh_headspouse, na.rm = TRUE) / num_clan_people,
      NA_real_
    )
  ) %>%
  ungroup()


# PREP WEIGHTS ---------------------------------------------------------------
# The sum of all weights is saved as sum_all_weights.
# In the clan file, sum_all_weights is divided by the number of households
# in the clan to get the average weight per household.

households <- households %>%
  mutate(fam_weight = as.numeric(fam_weight)) %>%
  ungroup() %>%
  mutate(sum_all_weights = efficient_sum(fam_weight))


# ORGANIZE FILE ---------------------------------------------------------------
households <- households %>%
  select(
    year,
    fam_id,
    id1968,
    id1968_spouse,
    pid,
    release,
    numfu,
    numclan,
    num_clan_people,
    hh_number,
    fam_weight,
    sum_all_weights,
    stratum,
    cluster,
    sample_head,
    sample_spouse,
    black_head,

    # Household structure variables
    hh_children,  # Number of children in the household
    hh_other,     # Number of other family members in the household
    hh_single,    # Indicator for a single-headed household
    hh_age_sum,   # Sum of member ages in the household (NEW)
    hh_age_n,     # Count of members with observed age in the household (NEW)

    # Clan demographic and structure variables
    clan_age_mean,             # Average age of clan members
    clan_children_hh_mean,     # Average children per household in the clan
    clan_other_hh_mean,        # Average other family members per household
    clan_other_prop,           # Share of clan members classified as other
    clan_headspouse_prop,      # Share of clan members who are heads or spouses

    # Income variables
    starts_with("inc_"),

    # Wealth variables
    starts_with("wealth")
  )


# LIMIT SAMPLE ---------------------------------------------------------------
# Strata values 33-56 are for the Latino sample.
# After filtering, six individuals in stratum 43 remain, but only for cluster 1.
# Remove them because they cause errors in svydesign() later.

households <- households %>%
  mutate(stratum = as.numeric(as.character(stratum))) %>%
  filter(stratum != 43)


# SAVE HOUSEHOLDS WITH NEGATIVE INCOME AND WEALTH VALUES ----------------------
file.remove(
  list.files(
    here("3_households", "output"),
    pattern = "\\.rds$",
    full.names = TRUE
  )
)

saveRDS(
  households,
  here("3_households", "output", "neg_households.rds")
)


# BOUND NEGATIVE INCOME AND WEALTH VALUES AT ZERO -----------------------------
households <- households %>%
  mutate(
    wealth_nohouse = ifelse(wealth_nohouse < 0, 0, wealth_nohouse),
    wealth = ifelse(wealth < 0, 0, wealth),
    inc_all = ifelse(inc_all < 0, 0, inc_all)
  )


# SAVE HOUSEHOLDS WITH BOUNDED INCOME AND WEALTH ------------------------------
saveRDS(
  households,
  here("3_households", "output", "households.rds")
)

saveRDS(
  mismatched,
  here("3_households", "output", "mismatched.rds")
)


# CLEAN UP TEMPORARY FILES ----------------------------------------------------
rm(clean, data, dt, fam_wide, ind_wide, long_dt)

