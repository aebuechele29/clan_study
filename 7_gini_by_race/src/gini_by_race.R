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

# CALCULATE GINIS BY RACE ---------------------------------------------------------
# There are four versions for each variable (income, wealth_nohouse, wealth) by Households, then clans:
    # 1) unweighted, all data
    # 2) weighted, all data
    # 3) unweighted, robust data
    # 4) weighted, robust data

# INCOME

# Households
hh_u_inc      <- run_gini_race(
  hh %>% filter(black_head == 1),
  hh %>% filter(black_head != 1 & !is.na(black_head)),
  "inc_all", NULL, FALSE, FALSE, "hh_u_inc"
)

hh_w_inc      <- run_gini_race(
  r_hh %>% filter(black_head == 1),
  r_hh %>% filter(black_head != 1 & !is.na(black_head)),
  "inc_all", "fam_weight", FALSE, TRUE, "hh_w_inc"
)

r_hh_u_inc      <- run_gini_race(
  r_hh %>% filter(black_head == 1),
  r_hh %>% filter(black_head != 1 & !is.na(black_head)),
  "inc_all", NULL, FALSE, FALSE, "r_hh_u_inc"
)

r_hh_w_inc      <- run_gini_race(
  r_hh %>% filter(black_head == 1),
  r_hh %>% filter(black_head != 1 & !is.na(black_head)),
  "inc_all", "fam_weight", FALSE, TRUE, "r_hh_w_inc"
)

# Clans - means
cl_u_inc_mean   <- run_gini_race(
  clans %>% filter(black_clan == 1),
  clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all_mean", NULL, TRUE, FALSE, "cl_u_inc_mean"
)

cl_inc_mean_w   <- run_gini_race(
  clans %>% filter(black_clan == 1),
  clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all_mean", "clan_weight", FALSE, TRUE, "cl_w_inc_mean"
)

r_cl_u_inc_mean   <- run_gini_race(
  r_clans %>% filter(black_clan == 1),
  r_clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all_mean", NULL, TRUE, FALSE, "r_cl_u_inc_mean"
)

r_cl_inc_mean_w   <- run_gini_race(
  r_clans %>% filter(black_clan == 1),
  r_clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all_mean", "clan_weight", FALSE, TRUE, "r_cl_w_inc_mean"
)

# Clans - totals
cl_u_inc   <- run_gini_race(
  clans %>% filter(black_clan == 1),
  clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all", NULL, TRUE, FALSE, "cl_u_inc"
)

cl_w_inc   <- run_gini_race(
  clans %>% filter(black_clan == 1),
  clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all", "clan_weight", FALSE, TRUE, "cl_w_inc"
)

r_cl_u_inc   <- run_gini_race(
  r_clans %>% filter(black_clan == 1),
  r_clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all", NULL, TRUE, FALSE, "r_cl_u_inc"
)

r_cl_w_inc   <- run_gini_race(
  r_clans %>% filter(black_clan == 1),
  r_clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all", "clan_weight", FALSE, TRUE, "r_cl_w_inc"
)

inc_by_year_race <- list(r_hh_w_inc, r_cl_w_inc) %>%
  reduce(full_join, by = "year") %>%
  arrange(year)
inc_by_year_race <- append_mean_row(inc_by_year_race)
write.csv(inc_by_year_race, here("7_gini_by_race", "output", "income_race.csv"), row.names = FALSE)



# WEALTH (excluding home equity)
# Households
hh_u_wealth      <- run_gini_race(
  hh_wealth %>% filter(black_head == 1),
  hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth_nohouse", NULL, FALSE, FALSE, "hh_u_wealth"
)
hh_w_wealth      <- run_gini_race(
  hh_wealth %>% filter(black_head == 1),
  hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth_nohouse", "fam_weight", FALSE, TRUE, "hh_w_wealth"
)

r_hh_u_wealth      <- run_gini_race(
  r_hh_wealth %>% filter(black_head == 1),
  r_hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth_nohouse", NULL, FALSE, FALSE, "r_hh_u_wealth"
)
r_hh_w_wealth      <- run_gini_race(
  r_hh_wealth %>% filter(black_head == 1),
  r_hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth_nohouse", "fam_weight", FALSE, TRUE, "r_hh_w_wealth"
)


# Clan - means
cl_u_wealth_mean   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse_mean", NULL, TRUE, FALSE, "cl_u_wealth_mean"
)

cl_w_wealth_mean   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse_mean", "clan_weight", FALSE, TRUE, "cl_w_wealth_mean"
)

r_cl_u_wealth_mean   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse_mean", NULL, TRUE, FALSE, "r_cl_u_wealth_mean"
)

r_cl_w_wealth_mean   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse_mean", "clan_weight", FALSE, TRUE, "r_cl_w_wealth_mean"
)

# Clan - totals
cl_u_wealth   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse", NULL, TRUE, FALSE, "cl_u_wealth"
)

cl_w_wealth   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse", "clan_weight", FALSE, TRUE, "cl_w_wealth"
)

r_cl_u_wealth   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse", NULL, TRUE, FALSE, "r_cl_u_wealth"
)

r_cl_w_wealth   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse", "clan_weight", FALSE, TRUE, "r_cl_w_wealth"
)

wealth_nohouse_by_year_race <- list(r_hh_w_wealth, r_cl_w_wealth) %>%
  reduce(full_join, by = "year") %>%
  arrange(year)
wealth_nohouse_by_year_race <- append_mean_row(wealth_nohouse_by_year_race)
write.csv(wealth_nohouse_by_year_race, here("7_gini_by_race", "output", "wealth_nohouse_race.csv"), row.names = FALSE)



# WEALTH (including home equity)
# Households
hh_u_wealth      <- run_gini_race(
  hh_wealth %>% filter(black_head == 1),
  hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth", NULL, FALSE, FALSE, "hh_u_wealth"
)
hh_w_wealth      <- run_gini_race(
  hh_wealth %>% filter(black_head == 1),
  hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth", "fam_weight", FALSE, TRUE, "hh_w_wealth"
)

r_hh_u_wealth      <- run_gini_race(
  r_hh_wealth %>% filter(black_head == 1),
  r_hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth", NULL, FALSE, FALSE, "r_hh_u_wealth"
)
r_hh_w_wealth      <- run_gini_race(
  r_hh_wealth %>% filter(black_head == 1),
  r_hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth", "fam_weight", FALSE, TRUE, "r_hh_w_wealth"
)


# Clan - means
cl_u_wealth_mean   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_mean", NULL, TRUE, FALSE, "cl_u_wealth_mean"
)

cl_w_wealth_mean   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_mean", "clan_weight", FALSE, TRUE, "cl_w_wealth_mean"
)

r_cl_u_wealth_mean   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_mean", NULL, TRUE, FALSE, "r_cl_u_wealth_mean"
)

r_cl_w_wealth_mean   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_mean", "clan_weight", FALSE, TRUE, "r_cl_w_wealth_mean"
)

# Clan - totals
cl_u_wealth   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth", NULL, TRUE, FALSE, "cl_u_wealth"
)

cl_w_wealth   <- run_gini_race(
  clans_wealth %>% filter(black_clan == 1),
  clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth", "clan_weight", FALSE, TRUE, "cl_w_wealth"
)

r_cl_u_wealth   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth", NULL, TRUE, FALSE, "r_cl_u_wealth"
)

r_cl_w_wealth  <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth", "clan_weight", FALSE, TRUE, "r_cl_w_wealth"
)

wealth_by_year_race <- list(r_hh_w_wealth, r_cl_w_wealth) %>%
  reduce(full_join, by = "year") %>%
  arrange(year)
wealth_by_year_race <- append_mean_row(wealth_by_year_race)
write.csv(wealth_by_year_race, here("7_gini_by_race", "output", "wealth_withhome_race.csv"), row.names = FALSE)



