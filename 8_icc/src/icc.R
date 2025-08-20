# INPUTS: 3_households/output/robust_households.rds
# OUTPUTS: 8_icc/output/icc_results.csv


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

write_csv(icc_tbl, here::here("8_icc", "output", "icc_results.csv"))



# NOTES FOR DORON / JULIA
- GINI'S NUCLEAR FAMILY IS ABOUT CHOICE OF MEASURE
- WE ALSO HAVE TO THINK ABOUT HOW THE GINI IS ESTIMATED (CURRENTLY USING SYVGINI WHICH USES TRAPEZIUM RULE)
  - THIS DOESN'T MATTER AS MUCH AS LONG AS WE POSITION OUR COMPARISON BETWEEN HOUSEHOLDS AND CLANS ACCORDINGLY
  - WE SHOULD BE USING AN ALGEBRAEIC METHOD INSTEAD OF A GEOMETRIC ONE


