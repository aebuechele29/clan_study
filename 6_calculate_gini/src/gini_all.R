library(here)
library(dplyr)
library(purrr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(rlang)
library(flextable)
library(officer)
library(survey)
library(convey)


# LOAD DATA ------------------------------------------------------------------
# Main data
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

# Including households in single-HH clans
hh <- readRDS(here("3_households", "output", "households.rds"))
clans <- readRDS(here("4_clans", "output", "clans.rds"))
hh_wealth <- readRDS(here("3_households", "output", "households_wealth.rds"))
clans_wealth <- readRDS(here("4_clans", "output", "clans_wealth.rds"))

# Including negative values for income and wealth
neg_r_hh <- readRDS("3_households/output/neg_robust_households.rds")
neg_r_clans <- readRDS("4_clans/output/neg_robust_clans.rds")
neg_r_hh_wealth <- readRDS("3_households/output/neg_robust_households_wealth.rds")
neg_r_clans_wealth <- readRDS("4_clans/output/neg_robust_clans_wealth.rds")

# Adjust weighted designs for lonely PSUs
options(survey.lonely.psu = "adjust")

# SIZE STANDARDIZATION -------------------------------------------------------
# Households: divide by number of people in household (numfu)
# Clans: divide by number of households in clan (numclan)

r_hh          <- r_hh          %>% mutate(inc_all   = inc_all   / numfu)
r_clans        <- r_clans       %>% mutate(inc_all   = inc_all   / numclan)
r_hh_wealth    <- r_hh_wealth   %>% mutate(wealth_nohouse = wealth_nohouse / numfu,
                                            wealth         = wealth         / numfu)
r_clans_wealth <- r_clans_wealth %>% mutate(wealth_nohouse = wealth_nohouse / numclan,
                                             wealth         = wealth         / numclan)

hh          <- hh          %>% mutate(inc_all   = inc_all   / numfu)
clans        <- clans       %>% mutate(inc_all   = inc_all   / numclan)
hh_wealth    <- hh_wealth   %>% mutate(wealth_nohouse = wealth_nohouse / numfu,
                                        wealth         = wealth         / numfu)
clans_wealth <- clans_wealth %>% mutate(wealth_nohouse = wealth_nohouse / numclan,
                                         wealth         = wealth         / numclan)

neg_r_hh          <- neg_r_hh          %>% mutate(inc_all   = inc_all   / numfu)
neg_r_clans        <- neg_r_clans       %>% mutate(inc_all   = inc_all   / numclan)
neg_r_hh_wealth    <- neg_r_hh_wealth   %>% mutate(wealth_nohouse = wealth_nohouse / numfu,
                                                     wealth         = wealth         / numfu)
neg_r_clans_wealth <- neg_r_clans_wealth %>% mutate(wealth_nohouse = wealth_nohouse / numclan,
                                                      wealth         = wealth         / numclan)

# CALCULATE GINIS ---------------------------------------------------------------

# Income
inc_dfs <- list(
    run_gini(hh,      "inc_all", "fam_weight",  FALSE, TRUE,  "hh_w_inc"),
    run_gini(r_hh,    "inc_all", "fam_weight",  FALSE, TRUE,  "r_hh_w_inc"),
    run_gini(clans,   "inc_all", "clan_weight", FALSE, TRUE,  "cl_w_inc"),
    run_gini(r_clans, "inc_all", "clan_weight", FALSE, TRUE,  "r_cl_w_inc"),
    run_gini(r_hh,    "inc_all", NULL,          FALSE, FALSE, "r_hh_unw_inc"),
    run_gini(r_clans, "inc_all", NULL,          FALSE, FALSE, "r_cl_unw_inc"),
    run_gini(neg_r_hh,    "inc_all", "fam_weight",  FALSE, TRUE, "neg_r_hh_inc"),
    run_gini(neg_r_clans, "inc_all", "clan_weight", FALSE, TRUE, "neg_r_cl_inc")
)

inc_by_year <- reduce(inc_dfs, full_join, by = "year") %>% arrange(year)
inc_by_year <- append_mean_row(inc_by_year)
write.csv(inc_by_year, here("6_calculate_gini", "output", "income.csv"), row.names = FALSE)

# Wealth (excluding home equity)
wealth_nohouse_dfs <- list(
    run_gini(hh_wealth,      "wealth_nohouse", "fam_weight",  FALSE, TRUE,  "hh_w_wealth"),
    run_gini(r_hh_wealth,    "wealth_nohouse", "fam_weight",  FALSE, TRUE,  "r_hh_w_wealth"),
    run_gini(clans_wealth,   "wealth_nohouse", "clan_weight", FALSE, TRUE,  "cl_w_wealth"),
    run_gini(r_clans_wealth, "wealth_nohouse", "clan_weight", FALSE, TRUE,  "r_cl_w_wealth"),
    run_gini(r_hh_wealth,    "wealth_nohouse", NULL,          FALSE, FALSE, "r_hh_unw_wealth"),
    run_gini(r_clans_wealth, "wealth_nohouse", NULL,          FALSE, FALSE, "r_cl_unw_wealth"),
    run_gini(neg_r_hh_wealth,    "wealth_nohouse", "fam_weight",  FALSE, TRUE, "neg_r_hh_wealth"),
    run_gini(neg_r_clans_wealth, "wealth_nohouse", "clan_weight", FALSE, TRUE, "neg_r_cl_wealth")
)

wealth_by_year <- reduce(wealth_nohouse_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year_nohouse <- append_mean_row(wealth_by_year)
write.csv(wealth_by_year_nohouse, here("6_calculate_gini", "output", "wealth_nohouse.csv"), row.names = FALSE)

# Wealth (including home equity)
wealth_dfs <- list(
    run_gini(hh_wealth,      "wealth", "fam_weight",  FALSE, TRUE,  "hh_w_wealth"),
    run_gini(r_hh_wealth,    "wealth", "fam_weight",  FALSE, TRUE,  "r_hh_w_wealth"),
    run_gini(clans_wealth,   "wealth", "clan_weight", FALSE, TRUE,  "cl_w_wealth"),
    run_gini(r_clans_wealth, "wealth", "clan_weight", FALSE, TRUE,  "r_cl_w_wealth"),
    run_gini(r_hh_wealth,    "wealth", NULL,          FALSE, FALSE, "r_hh_unw_wealth"),
    run_gini(r_clans_wealth, "wealth", NULL,          FALSE, FALSE, "r_cl_unw_wealth"),
    run_gini(neg_r_hh_wealth,    "wealth", "fam_weight",  FALSE, TRUE, "neg_r_hh_wealth"),
    run_gini(neg_r_clans_wealth, "wealth", "clan_weight", FALSE, TRUE, "neg_r_cl_wealth")
)

wealth_by_year <- reduce(wealth_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year <- append_mean_row(wealth_by_year)
write.csv(wealth_by_year, here("6_calculate_gini", "output", "wealth_withhome.csv"), row.names = FALSE)
