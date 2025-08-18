# INPUTS: 3_households/output/households.rds
# OUTPUTS: 4_clans/output/clans.rds, 4_clans/output/robust_clans.rds, 3_households/output/robust_households.rds

households <- readRDS(here("3_households", "output", "households.rds"))

# CREATE CLAN-LEVEL DATAFRAME ---------------------------------------------------
  # This will summarize household data into clan-level data 
clans <- households %>%
  group_by(year, id1968) %>%
  summarise(
    numclan = n(),

    # Keep variables that are the same for all households within a clan
    sum_all_weights = first(sum_all_weights), # Sum of all survey weights 
    fam_id          = first(fam_id),          # To remove single-household clans later
    num_clan_people = first(num_clan_people), 
    stratum         = first(stratum),
    cluster         = first(cluster),

    # Income & wealth (unweighted): sum / mean / median / sd
    across(
      .cols = c(inc_all, wealth_nohouse),
      .fns  = list(
        clan   = ~ sum(.x, na.rm = TRUE),
        mean   = ~ sum(.x, na.rm = TRUE) / numclan,
        median = ~ median(.x, na.rm = TRUE),
        n      = ~ sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    ),

    # Race counts (unweighted)
    black_households = sum(black_head, na.rm = TRUE),
    black_clan       = ifelse(black_households / numclan > 0.50, 1, 0),

    # Sum of weights in the clan-year
    clan_weight = sum(fam_weight, na.rm = TRUE) / numclan,

    .groups = "drop"
  )

# Rename columns for clarity
clans <- clans %>%
  rename(inc_all = inc_all_clan,
         wealth_nohouse = wealth_nohouse_clan)


# VALIDATE CLAN IDENTIFIERS ---------------------------------------------------
# id1968: Individual identifier anchored in 1968
if (nrow(distinct(households, id1968, year)) != nrow(clans)) {
  stop("Different number of distinct clans in household and clan file. Check merge.")
}

# year: Year of observation
if (nrow(clans) != nrow(distinct(clans, id1968, year))) {
  stop("Duplicate (id1968, year) combinations found in clans. Check merge.")
}

# ADJUST KEY OUTCOMES FOR CLAN SIZE ---------------------------------------------------
clans <- clans %>%
  mutate(
    inc_all = inc_all / numclan,  # Average income per household in clan
    wealth_nohouse = wealth_nohouse / numclan  # Average wealth per household in clan
  )
  

# CREATE ROBUST CLANS - CLANS WITH MORE THAN ONE HOUSEHOLD ---------------------------------
# Filter clans with more than one household
robust_clans <- clans %>%
  filter(numclan > 1)

remove <- clans %>%
  filter(numclan == 1) %>%
  select(year, fam_id)

# Remove households with fam_id in remove list
robust_households <- households %>%
  anti_join(remove, by = c("year", "fam_id"))

# Remove fam_id from clans
clans <- clans %>%
  select(-fam_id)

robust_clans <- robust_clans %>%
  select(-fam_id)

# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("4_clans", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(clans, here("4_clans", "output", "clans.rds"))
saveRDS(robust_clans, here("4_clans", "output", "robust_clans.rds"))
saveRDS(robust_households, here("3_households", "output", "robust_households.rds"))

rm(list = ls())