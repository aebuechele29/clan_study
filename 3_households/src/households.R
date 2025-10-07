# INPUTS: 2_clean_panel/output/clean.rds
# OUTPUTS: 3_households/output/households.rds

# LOAD DATA ------------------------------------------------------------------
clean <- readRDS(here("2_clean_panel", "output", "clean.rds"))
data <- clean

# RESHAPE DATA FROM INDIVIDUAL-YEARS TO FAMILY-YEARS ------------------------------------------------------------------
data <- data %>%
  mutate(role_tag = case_when(
    relation == 1 ~ "head",
    relation == 2 ~ "spouse",
    relation == 3 ~ "child",
    relation == 4 ~ "other",
    relation == 0 ~ "unknown",
    TRUE ~ "unknown"
  )) %>%
  group_by(fam_id, year, role_tag) %>%
  mutate(role_num = row_number(),
         role_id = paste0(role_tag, "_", role_num)) %>%
  ungroup() %>%
  filter(relation != 0) %>%
  select(-role_tag, -role_num, -pn, -relation, -sequence) 

# Format as data.table and separate family and individual data
dt <- as.data.table(data)

id_vars <- c("year", "fam_id")  
fam_vars <- c(
  "ind_top_inc_all", "ind_top_inc_tax_hs", "ind_top_inc_tax_o",
  "ind_top_inc_trans_hs", "ind_top_inc_trans_o1", "ind_top_inc_trans_o2",
  "ind_top_wealth_nohouse", "ind_top_wealth", "ind_top_wealth_farmbus",
  "ind_top_wealth_checking", "ind_top_wealth_debt", "ind_top_wealth_re",
  "ind_top_wealth_stocks", "ind_top_wealth_vehicles", "ind_top_wealth_other",
  "ind_top_wealth_home", "ind_top_student_loans",
  "inc_all", "inc_tax_hs", "inc_tax_o", "inc_trans_hs", "inc_trans_o1", "inc_trans_o2",
  "wealth_nohouse", "wealth", "wealth_farmbus", "wealth_checking", "wealth_debt",
  "wealth_re", "wealth_stocks", "wealth_vehicles", "wealth_other", "wealth_home",
  "student_loans", "release", "numfu", "fam_weight"
)

ind_vars <- c(
  "id1968", "sample", "stratum", "cluster", 
  "race_year", "race", "black"
)

# Create family-level data
dt_fam <- data[, c("year", "fam_id", "pid", fam_vars), with = FALSE]

#dt_fam <- dt_fam %>%
  # select(-c(pid))

fam_wide <- dt_fam %>%
  group_by(year, fam_id) %>%
  slice(1) %>%
  ungroup()

rm(dt_fam)

# Create individual-level data
long_dt <- melt(dt, 
                id.vars = c(id_vars, "pid", "role_id"), 
                measure.vars = ind_vars,
                variable.name = "var")

# Create role tag for pivot to wide
long_dt[, var_role := paste0(var, "_", role_id)]

# Cast to wide
ind_wide <- dcast(
  long_dt,
  year + fam_id ~ var_role,
  value.var = "value"
)


# CLEAN HOUSEHOLD FILE -------------------------------------------------------------
# Remove columns with the same values for all roles within a family (i.e. adopt value for the head) 
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

# Merge family and individual data
households <- merge(
  fam_wide,
  ind_wide,
  by = c("year", "fam_id"),
  all.x = TRUE
)

# Remove columns that are not needed for now (topcodes and sample)
households <- households %>%
  select(
    -starts_with("ind_top_"),
    -starts_with("race_year_"))


# ADD CLAN DATA TO HOUSEHOLDS ------------------------------------------------------------------
# Add number of households within each clan by year
households <- households %>%
  group_by(year, id1968) %>%
  mutate(numclan = n()) %>%
  ungroup()

# Assign a clan number to each household per year
households <- households %>%
  group_by(id1968, year) %>%
  arrange(fam_id) %>%
  mutate(hh_number = row_number()) %>%
  ungroup() 

# Add number of family members in each clan
households <- households %>%
  group_by(year, id1968) %>%
  mutate(num_clan_people = sum(numfu, na.rm = TRUE)) %>%
  ungroup()

# Check that clan ids are shared between heads and spouses
# Save mismatched cases
  mismatched <- households %>%
    filter(
      !is.na(id1968) & 
      !is.na(id1968_spouse) & 
      id1968 != id1968_spouse
    ) 
  
  saveRDS(mismatched, here("3_households", "output", "mismatched.rds"))

# Remove mismatched from the main dataset
  households <- households %>%
    filter(
      is.na(id1968) | 
       is.na(id1968_spouse) | 
       id1968 == id1968_spouse
   )


# ADD WEIGHTED VALUES   ---------------------------------------------------
  # These values are added to households so that they can be used in the clan file
  # Raw values are not meaningful, each represents (xi x wi) where xi is the value and wi is the weight
  # The sum of all weights is saved as sum_all_weights. Dividing the sum of weighted values by this gives the weighted mean
  # The survey package replicates the same weighted mean calculation and reports on variation based on strata and clusters

households <- households %>%
  mutate(fam_weight = as.numeric(fam_weight))

# Calculate weighted values for income and wealth
vars <- c(
  "inc_all", "inc_tax_hs", "inc_tax_o", "inc_trans_hs", "inc_trans_o1", "inc_trans_o2",
  "wealth_nohouse", "wealth", "wealth_farmbus", "wealth_checking", "wealth_debt",
  "wealth_re", "wealth_stocks", "wealth_vehicles", "wealth_other", "wealth_home",
  "student_loans"
)

for (var in vars) {
  households[[paste0(var, "_w")]] <- households[[var]] * households$fam_weight
}


# Add sum of all weights as a variable
households <- households %>%
  ungroup() %>%
  mutate(sum_all_weights = efficient_sum(fam_weight))
  

# Organize file
households <- households %>%
  select(
    year, fam_id, id1968, id1968_spouse, pid, release, numfu,   
    hh_number, numclan, num_clan_people,
    fam_weight, sum_all_weights, stratum, cluster, sample_head, sample_spouse, black_head,
    
    # Income variables
    starts_with("inc_"),
    
    # Wealth variables
    starts_with("wealth"),
    starts_with("student_loans")
  )

# LIMIT SAMPLE -----------------------------------------------------------------
# Strata values 33-56 are for the Latino sample. After filtering, 6 individuals in strata 43 remain but only for cluster 1
# Removing because these individuals cause errors in the svydesign function later
households <- households %>%
  mutate(stratum = as.numeric(as.character(stratum))) %>%
  filter(stratum != 43)

# # Set negative values for wealth and income to 0 - i.e. bound Gini calculations
# households <- households %>%
#    mutate(
#      wealth_nohouse = ifelse(wealth_nohouse < 0, 0, wealth_nohouse),
#      inc_all        = ifelse(inc_all < 0, 0, inc_all)
#    )


# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("3_households", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(households, here("3_households", "output", "households.rds"))

# Clean Up Temporary Files --------------------------------------------------
rm(clean, data, dt, fam_wide, ind_wide, long_dt, mismatched)




