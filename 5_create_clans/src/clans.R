# INPUTS: 4_clean_households/output/clean_hs.rds
# OUTPUTS: 5_clans/output/clans.rds

households <- readRDS(here("4_clean_households", "output", "clean_hs.rds"))

# CREATE CLANS
cols_to_pivot <- setdiff(names(households), c("id1968", "year", "hh_number", "numclan"))

clans <- households %>%
  pivot_wider(
    id_cols = c(id1968, year, numclan),
    names_from = hh_number,
    values_from = setdiff(names(households), c("id1968", "year", "numclan", "hh_number"))
  )

# CLEAN/VALIDATE CLAN IDENTIFIERS
# id1968: Individual identifier anchored in 1968
if (nrow(distinct(households, id1968, year)) != nrow(clans)) {
  stop("Different number of distinct clans in household and clan file. Check merge.")
}

# year: Year of observation
if (nrow(clans) != nrow(distinct(clans, id1968, year))) {
  stop("Duplicate (id1968, year) combinations found in clans. Check merge.")
}

# SAVE ---------------------------------------------------------------------------
file.remove(list.files(here("5_create_clans", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(clans, here("5_create_clans", "output", "clans.rds"))








