# LOAD DATA ----------------------------------------------------------------
hh     <- readRDS(here("3_households", "output", "households.rds"))
hh_wealth    <- readRDS(here("3_households", "output", "households_wealth.rds"))
r_hh     <- readRDS(here("3_households", "output", "robust_households.rds"))
r_hh_wealth    <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))



# ALL DATA -----------------------------------------------------------------
# Income
m_inc <- lmer(inc_all ~ 1 + (1 | id1968) + (1 | id1968:fam_id), data = hh)
icc_inc <- performance::icc(m_inc, by_group = TRUE) |>
  as.data.frame() |>
  rownames_to_column("group") |>
  mutate(outcome = "inc_all", robust = FALSE)

# Wealth excluding home equity
m_wealth <- lmer(wealth_nohouse ~ 1 + (1 | id1968) + (1 | id1968:fam_id), data = hh_wealth)
icc_wealth_nohouse <- performance::icc(m_wealth, by_group = TRUE) |>
  as.data.frame() |>
  rownames_to_column("group") |>
  mutate(outcome = "wealth_nohouse", robust = FALSE)

# Wealth including home equity
m_wealth_home <- lmer(wealth ~ 1 + (1 | id1968) + (1 | id1968:fam_id), data = hh_wealth)
icc_wealth_home <- performance::icc(m_wealth_home, by_group = TRUE) |>
  as.data.frame() |>
  rownames_to_column("group") |>
  mutate(outcome = "wealth", robust = FALSE)



# ROBUST DATA -----------------------------------------------------------------
# Income
m_inc <- lmer(inc_all ~ 1 + (1 | id1968) + (1 | id1968:fam_id), data = r_hh)
r_icc_inc <- performance::icc(m_inc, by_group = TRUE) |>
  as.data.frame() |>
  rownames_to_column("group") |>
  mutate(outcome = "inc_all", robust = TRUE)

# Wealth excluding home equity
m_wealth <- lmer(wealth_nohouse ~ 1 + (1 | id1968) + (1 | id1968:fam_id), data = r_hh_wealth)
r_icc_wealth_nohouse <- performance::icc(m_wealth, by_group = TRUE) |>
  as.data.frame() |>
  rownames_to_column("group") |>
  mutate(outcome = "wealth_nohouse", robust = TRUE)

# Wealth including home equity
m_wealth_home <- lmer(wealth ~ 1 + (1 | id1968) + (1 | id1968:fam_id), data = r_hh_wealth)
r_icc_wealth_home <- performance::icc(m_wealth_home, by_group = TRUE) |>
  as.data.frame() |>
  rownames_to_column("group") |>
  mutate(outcome = "wealth", robust = TRUE)

# Combine and export
icc_all <- bind_rows(
  icc_inc, icc_wealth_nohouse, icc_wealth_home,
  r_icc_inc, r_icc_wealth_nohouse, r_icc_wealth_home
) |>
  relocate(outcome, robust, group)

icc_all <- icc_all %>% select(-group)

write_csv(icc_all, here("5_summary", "output", "icc.csv"))
