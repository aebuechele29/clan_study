# INPUTS: 3_households/output/robust_households.rds
# OUTPUTS: 9_diagnostics/output/icc_results.csv


# DATA ---------------------------------------------------------------------
r_hh     <- readRDS(here("3_households", "output", "robust_households.rds"))


# Model for ICC Calculation ------------------------------------------------
model_null <- lmer(
  inc_all ~ 1 + (1 | id1968) + (1 | id1968:fam_id), # Nests households within clans
  data = r_hh
)

# Extract ICCs
icc_results <- performance::icc(model_null, by_group = TRUE)

# Combine and export
icc_tbl <- as.data.frame(icc_results) %>%
  tibble::rownames_to_column("group")

write_csv(icc_tbl, here::here("9_diagnostics", "output", "icc_results.csv"))

model_null <- lmer(
wealth_nohouse ~ 1 + (1 | id1968) + (1 | id1968:fam_id), # Nests households within clans
  data = r_hh
)

# Extract ICCs
icc_results <- performance::icc(model_null, by_group = TRUE)

# Combine and export
icc_tbl <- as.data.frame(icc_results) %>%
  tibble::rownames_to_column("group")

write_csv(icc_tbl, here::here("9_diagnostics", "output", "icc_results_wealth.csv"))

