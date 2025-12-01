library(here)
library(dplyr)
library(purrr)

# LOAD DATA ------------------------------------------------------------------
hh <- readRDS(here("3_households", "output", "households.rds"))
clans <- readRDS(here("4_clans", "output", "clans.rds"))
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

hh_wealth <- readRDS(here("3_households", "output", "households_wealth.rds"))
clans_wealth <- readRDS(here("4_clans", "output", "clans_wealth.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

# Adjust weighted designs for lonely PSUs
options(survey.lonely.psu = "adjust")

# CALCULATE GINIS ---------------------------------------------------------------

# Income
inc_dfs <- list(
    run_gini(hh,      "inc_all", "fam_weight", FALSE, TRUE, "hh_w_inc"),
    run_gini(r_hh,    "inc_all", "fam_weight", FALSE, TRUE, "r_hh_w_inc"),
    run_gini(clans,   "inc_all", "clan_weight", FALSE, TRUE, "cl_w_inc"),
    run_gini(r_clans, "inc_all", "clan_weight", FALSE, TRUE, "r_cl_w_inc")
)

inc_by_year <- reduce(inc_dfs, full_join, by = "year") %>% arrange(year)
inc_by_year <- append_mean_row(inc_by_year)
write.csv(inc_by_year, here("6_calculate_gini", "output", "income.csv"), row.names = FALSE)

# Wealth (excluding home equity)
wealth_nohouse_dfs <- list(
    run_gini(hh_wealth,   "wealth_nohouse", "fam_weight", FALSE, TRUE, "hh_w_wealth"),
    run_gini(r_hh_wealth, "wealth_nohouse", "fam_weight", FALSE, TRUE, "r_hh_w_wealth"),
    run_gini(clans_wealth,   "wealth_nohouse", "clan_weight", FALSE, TRUE, "cl_w_wealth"),
    run_gini(r_clans_wealth, "wealth_nohouse", "clan_weight", FALSE, TRUE, "r_cl_w_wealth")
)

wealth_by_year <- reduce(wealth_nohouse_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year_nohouse <- append_mean_row(wealth_by_year)
write.csv(wealth_by_year_nohouse, here("6_calculate_gini", "output", "wealth_nohouse.csv"), row.names = FALSE)

# Wealth (including home equity)
wealth_dfs <- list(
    run_gini(hh_wealth,   "wealth", "fam_weight", FALSE, TRUE, "hh_w_wealth"),
    run_gini(r_hh_wealth, "wealth", "fam_weight", FALSE, TRUE, "r_hh_w_wealth"),
    run_gini(clans_wealth,   "wealth", "clan_weight", FALSE, TRUE, "cl_w_wealth"),
    run_gini(r_clans_wealth, "wealth", "clan_weight", FALSE, TRUE, "r_cl_w_wealth")
)

wealth_by_year <- reduce(wealth_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year <- append_mean_row(wealth_by_year)
write.csv(wealth_by_year, here("6_calculate_gini", "output", "wealth_withhome.csv"), row.names = FALSE)




# Compare wealth measures
compare_wealth <- read.csv(here("6_calculate_gini", "output", "wealth_nohouse.csv")) %>%
  select(year,
         r_hh_w_wealth_nohouse = r_hh_w_wealth,
         r_cl_w_wealth_nohouse = r_cl_w_wealth) %>%
  full_join(
    read.csv(here("6_calculate_gini", "output", "wealth_withhome.csv")) %>%
      select(year,
             r_hh_w_wealth_withhome = r_hh_w_wealth,
             r_cl_w_wealth_withhome = r_cl_w_wealth),
    by = "year"
  ) %>%
  select(year,
         r_hh_w_wealth_nohouse,
         r_cl_w_wealth_nohouse,
         r_hh_w_wealth_withhome,
         r_cl_w_wealth_withhome) %>%
  mutate(
    diff_nohouse = r_hh_w_wealth_nohouse - r_cl_w_wealth_nohouse,
    diff_withhome = r_hh_w_wealth_withhome - r_cl_w_wealth_withhome,
    diff_wealth = diff_nohouse - diff_withhome
  ) %>%
  write.csv(here("6_calculate_gini", "output", "compare_wealth", "wealth_ginis_comparison.csv"),
            row.names = FALSE)


# Compare samples
income <- read.csv(here("6_calculate_gini", "output", "income.csv")) %>%
  select(year,
         hh_w_inc,
         r_hh_w_inc,
         cl_w_inc,
         r_cl_w_inc)  %>%
  filter(year != "ALL") %>%
  arrange(as.integer(year)) %>%
  mutate(
    all_hh_diff = hh_w_inc - cl_w_inc,
    robust_hh_diff = r_hh_w_inc - r_cl_w_inc,
    diff_samples = all_hh_diff - robust_hh_diff
  ) %>%
  write.csv(here("6_calculate_gini", "output", "compare_r", "income.csv"),
            row.names = FALSE)

wealth_nohouse <- read.csv(here("6_calculate_gini", "output","wealth_nohouse.csv")) %>%
  select(year,
         hh_w_wealth,
         r_hh_w_wealth,
         cl_w_wealth,
         r_cl_w_wealth) %>%
  filter(year != "ALL") %>%
  arrange(as.integer(year)) %>%
  mutate(
    all_hh_diff = hh_w_wealth - cl_w_wealth,
    robust_hh_diff = r_hh_w_wealth - r_cl_w_wealth,
    diff_samples = all_hh_diff - robust_hh_diff
  ) %>%
  write.csv(here("6_calculate_gini", "output", "compare_r", "wealth_nohouse.csv"),
            row.names = FALSE)

wealth_nohouse <- read.csv(here("6_calculate_gini", "output", "wealth_withhome.csv")) %>%
  select(year,
         hh_w_wealth,
         r_hh_w_wealth,
         cl_w_wealth,
         r_cl_w_wealth) %>%
  filter(year != "ALL") %>%
  arrange(as.integer(year)) %>%
  mutate(
    all_hh_diff = hh_w_wealth - cl_w_wealth,
    robust_hh_diff = r_hh_w_wealth - r_cl_w_wealth,
    diff_samples = all_hh_diff - robust_hh_diff
  ) %>%
  write.csv(here("6_calculate_gini", "output", "compare_r", "wealth_withhome.csv"),
            row.names = FALSE)


