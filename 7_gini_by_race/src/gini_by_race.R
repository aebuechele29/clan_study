library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(rlang)
library(survey)
library(convey)

source(here::here("functions", "all_functions.R"))

# LOAD DATA ------------------------------------------------------------------
hh             <- readRDS(here("3_households", "output", "households.rds"))
clans          <- readRDS(here("4_clans",      "output", "clans.rds"))
r_hh           <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans        <- readRDS(here("4_clans",      "output", "robust_clans.rds"))

hh_wealth      <- readRDS(here("3_households", "output", "households_wealth.rds"))
clans_wealth   <- readRDS(here("4_clans",      "output", "clans_wealth.rds"))
r_hh_wealth    <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans_wealth <- readRDS(here("4_clans",      "output", "robust_clans_wealth.rds"))

neg_r_hh           <- readRDS(here("3_households", "output", "neg_robust_households.rds"))
neg_r_clans        <- readRDS(here("4_clans",      "output", "neg_robust_clans.rds"))
neg_r_hh_wealth    <- readRDS(here("3_households", "output", "neg_robust_households_wealth.rds"))
neg_r_clans_wealth <- readRDS(here("4_clans",      "output", "neg_robust_clans_wealth.rds"))

options(survey.lonely.psu = "adjust")

out_dir <- here("7_gini_by_race", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# SIZE STANDARDIZATION -------------------------------------------------------
hh                 <- hh                 %>% mutate(inc_all        = inc_all        / numfu)
r_hh               <- r_hh               %>% mutate(inc_all        = inc_all        / numfu)
neg_r_hh           <- neg_r_hh           %>% mutate(inc_all        = inc_all        / numfu)
hh_wealth          <- hh_wealth          %>% mutate(wealth_nohouse = wealth_nohouse / numfu,
                                                     wealth         = wealth         / numfu)
r_hh_wealth        <- r_hh_wealth        %>% mutate(wealth_nohouse = wealth_nohouse / numfu,
                                                     wealth         = wealth         / numfu)
neg_r_hh_wealth    <- neg_r_hh_wealth    %>% mutate(wealth_nohouse = wealth_nohouse / numfu,
                                                     wealth         = wealth         / numfu)
clans              <- clans              %>% mutate(inc_all        = inc_all        / numclan)
r_clans            <- r_clans            %>% mutate(inc_all        = inc_all        / numclan)
neg_r_clans        <- neg_r_clans        %>% mutate(inc_all        = inc_all        / numclan)
clans_wealth       <- clans_wealth       %>% mutate(wealth_nohouse = wealth_nohouse / numclan,
                                                     wealth         = wealth         / numclan)
r_clans_wealth     <- r_clans_wealth     %>% mutate(wealth_nohouse = wealth_nohouse / numclan,
                                                     wealth         = wealth         / numclan)
neg_r_clans_wealth <- neg_r_clans_wealth %>% mutate(wealth_nohouse = wealth_nohouse / numclan,
                                                     wealth         = wealth         / numclan)


# PART 1: GINI COEFFICIENTS BY RACE

# Income ───────────────────────────────────────────────────────────────────
hh_u_inc   <- run_gini_race(hh    %>% filter(black_head == 1),
                             hh    %>% filter(black_head != 1, !is.na(black_head)),
                             "inc_all", NULL, FALSE, FALSE, "hh_u_inc")
hh_w_inc   <- run_gini_race(hh    %>% filter(black_head == 1),
                             hh    %>% filter(black_head != 1, !is.na(black_head)),
                             "inc_all", "fam_weight", FALSE, TRUE, "hh_w_inc")
r_hh_u_inc <- run_gini_race(r_hh  %>% filter(black_head == 1),
                             r_hh  %>% filter(black_head != 1, !is.na(black_head)),
                             "inc_all", NULL, FALSE, FALSE, "r_hh_u_inc")
r_hh_w_inc <- run_gini_race(r_hh  %>% filter(black_head == 1),
                             r_hh  %>% filter(black_head != 1, !is.na(black_head)),
                             "inc_all", "fam_weight", FALSE, TRUE, "r_hh_w_inc")
cl_u_inc   <- run_gini_race(clans   %>% filter(black_clan == 1),
                             clans   %>% filter(black_clan != 1, !is.na(black_clan)),
                             "inc_all", NULL, TRUE, FALSE, "cl_u_inc")
cl_w_inc   <- run_gini_race(clans   %>% filter(black_clan == 1),
                             clans   %>% filter(black_clan != 1, !is.na(black_clan)),
                             "inc_all", "clan_weight", FALSE, TRUE, "cl_w_inc")
r_cl_u_inc <- run_gini_race(r_clans %>% filter(black_clan == 1),
                             r_clans %>% filter(black_clan != 1, !is.na(black_clan)),
                             "inc_all", NULL, TRUE, FALSE, "r_cl_u_inc")
r_cl_w_inc <- run_gini_race(r_clans %>% filter(black_clan == 1),
                             r_clans %>% filter(black_clan != 1, !is.na(black_clan)),
                             "inc_all", "clan_weight", FALSE, TRUE, "r_cl_w_inc")

inc_by_year_race <- list(r_hh_w_inc, r_cl_w_inc) %>%
  reduce(full_join, by = "year") %>%
  arrange(year) %>%
  append_mean_row()
write.csv(inc_by_year_race, file.path(out_dir, "income_race.csv"), row.names = FALSE)

# Wealth (excl. home equity) ───────────────────────────────────────────────
hh_u_wnh   <- run_gini_race(hh_wealth    %>% filter(black_head == 1),
                             hh_wealth    %>% filter(black_head != 1, !is.na(black_head)),
                             "wealth_nohouse", NULL, FALSE, FALSE, "hh_u_wealth")
hh_w_wnh   <- run_gini_race(hh_wealth    %>% filter(black_head == 1),
                             hh_wealth    %>% filter(black_head != 1, !is.na(black_head)),
                             "wealth_nohouse", "fam_weight", FALSE, TRUE, "hh_w_wealth")
r_hh_u_wnh <- run_gini_race(r_hh_wealth  %>% filter(black_head == 1),
                             r_hh_wealth  %>% filter(black_head != 1, !is.na(black_head)),
                             "wealth_nohouse", NULL, FALSE, FALSE, "r_hh_u_wealth")
r_hh_w_wnh <- run_gini_race(r_hh_wealth  %>% filter(black_head == 1),
                             r_hh_wealth  %>% filter(black_head != 1, !is.na(black_head)),
                             "wealth_nohouse", "fam_weight", FALSE, TRUE, "r_hh_w_wealth")
cl_u_wnh   <- run_gini_race(clans_wealth   %>% filter(black_clan == 1),
                             clans_wealth   %>% filter(black_clan != 1, !is.na(black_clan)),
                             "wealth_nohouse", NULL, TRUE, FALSE, "cl_u_wealth")
cl_w_wnh   <- run_gini_race(clans_wealth   %>% filter(black_clan == 1),
                             clans_wealth   %>% filter(black_clan != 1, !is.na(black_clan)),
                             "wealth_nohouse", "clan_weight", FALSE, TRUE, "cl_w_wealth")
r_cl_u_wnh <- run_gini_race(r_clans_wealth %>% filter(black_clan == 1),
                             r_clans_wealth %>% filter(black_clan != 1, !is.na(black_clan)),
                             "wealth_nohouse", NULL, TRUE, FALSE, "r_cl_u_wealth")
r_cl_w_wnh <- run_gini_race(r_clans_wealth %>% filter(black_clan == 1),
                             r_clans_wealth %>% filter(black_clan != 1, !is.na(black_clan)),
                             "wealth_nohouse", "clan_weight", FALSE, TRUE, "r_cl_w_wealth")

wnh_by_year_race <- list(r_hh_w_wnh, r_cl_w_wnh) %>%
  reduce(full_join, by = "year") %>%
  arrange(year) %>%
  append_mean_row()
write.csv(wnh_by_year_race, file.path(out_dir, "wealth_nohouse_race.csv"), row.names = FALSE)

# Wealth (incl. home equity) ───────────────────────────────────────────────
hh_u_wh   <- run_gini_race(hh_wealth    %>% filter(black_head == 1),
                            hh_wealth    %>% filter(black_head != 1, !is.na(black_head)),
                            "wealth", NULL, FALSE, FALSE, "hh_u_wealth")
hh_w_wh   <- run_gini_race(hh_wealth    %>% filter(black_head == 1),
                            hh_wealth    %>% filter(black_head != 1, !is.na(black_head)),
                            "wealth", "fam_weight", FALSE, TRUE, "hh_w_wealth")
r_hh_u_wh <- run_gini_race(r_hh_wealth  %>% filter(black_head == 1),
                            r_hh_wealth  %>% filter(black_head != 1, !is.na(black_head)),
                            "wealth", NULL, FALSE, FALSE, "r_hh_u_wealth")
r_hh_w_wh <- run_gini_race(r_hh_wealth  %>% filter(black_head == 1),
                            r_hh_wealth  %>% filter(black_head != 1, !is.na(black_head)),
                            "wealth", "fam_weight", FALSE, TRUE, "r_hh_w_wealth")
cl_u_wh   <- run_gini_race(clans_wealth   %>% filter(black_clan == 1),
                            clans_wealth   %>% filter(black_clan != 1, !is.na(black_clan)),
                            "wealth", NULL, TRUE, FALSE, "cl_u_wealth")
cl_w_wh   <- run_gini_race(clans_wealth   %>% filter(black_clan == 1),
                            clans_wealth   %>% filter(black_clan != 1, !is.na(black_clan)),
                            "wealth", "clan_weight", FALSE, TRUE, "cl_w_wealth")
r_cl_u_wh <- run_gini_race(r_clans_wealth %>% filter(black_clan == 1),
                            r_clans_wealth %>% filter(black_clan != 1, !is.na(black_clan)),
                            "wealth", NULL, TRUE, FALSE, "r_cl_u_wealth")
r_cl_w_wh <- run_gini_race(r_clans_wealth %>% filter(black_clan == 1),
                            r_clans_wealth %>% filter(black_clan != 1, !is.na(black_clan)),
                            "wealth", "clan_weight", FALSE, TRUE, "r_cl_w_wealth")

wh_by_year_race <- list(r_hh_w_wh, r_cl_w_wh) %>%
  reduce(full_join, by = "year") %>%
  arrange(year) %>%
  append_mean_row()
write.csv(wh_by_year_race, file.path(out_dir, "wealth_withhome_race.csv"), row.names = FALSE)



# PART 2: RACE RATIOS
# For each variable, compute weighted mean by year for Black and Non-Black,
# then ratio = Black mean / Non-Black mean.
# Specs: hh_u, hh_w, r_hh_u, r_hh_w, cl_u, cl_w, r_cl_u, r_cl_w,
#        neg_r_hh_w, neg_r_cl_w
# Saved as wide CSVs: one row per year, one column per spec

wtd_mean_yr <- function(df, value_var, weight_var = NULL) {
  df %>%
    group_by(year) %>%
    summarise(
      m = if (is.null(weight_var)) {
        mean(.data[[value_var]], na.rm = TRUE)
      } else {
        w <- .data[[weight_var]]
        x <- .data[[value_var]]
        keep <- is.finite(x) & is.finite(w) & w > 0
        sum(x[keep] * w[keep]) / sum(w[keep])
      },
      .groups = "drop"
    )
}

make_ratio_wide <- function(specs) {
  purrr::map(specs, function(s) {
    b  <- wtd_mean_yr(s$black_df,    s$value_var, s$weight_var) %>% rename(!!paste0(s$name, "_black_mean")    := m)
    nb <- wtd_mean_yr(s$nonblack_df, s$value_var, s$weight_var) %>% rename(!!paste0(s$name, "_nonblack_mean") := m)
    full_join(b, nb, by = "year") %>%
      mutate(!!paste0(s$name, "_ratio") := .data[[paste0(s$name, "_black_mean")]] /
                                           .data[[paste0(s$name, "_nonblack_mean")]])
  }) %>%
    reduce(full_join, by = "year") %>%
    arrange(year)
}

# Income ratios ─────────────────────────────────────────────────────────────
inc_ratio_specs <- list(
  list(name = "hh_u",       black_df = hh        %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = hh        %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "inc_all", weight_var = NULL),
  list(name = "hh_w",       black_df = hh        %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = hh        %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "inc_all", weight_var = "fam_weight"),
  list(name = "r_hh_u",     black_df = r_hh      %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = r_hh      %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "inc_all", weight_var = NULL),
  list(name = "r_hh_w",     black_df = r_hh      %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = r_hh      %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "inc_all", weight_var = "fam_weight"),
  list(name = "cl_u",       black_df = clans     %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = clans     %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "inc_all", weight_var = NULL),
  list(name = "cl_w",       black_df = clans     %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = clans     %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "inc_all", weight_var = "clan_weight"),
  list(name = "r_cl_u",     black_df = r_clans   %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = r_clans   %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "inc_all", weight_var = NULL),
  list(name = "r_cl_w",     black_df = r_clans   %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = r_clans   %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "inc_all", weight_var = "clan_weight"),
  list(name = "neg_r_hh_w", black_df = neg_r_hh  %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = neg_r_hh  %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "inc_all", weight_var = "fam_weight"),
  list(name = "neg_r_cl_w", black_df = neg_r_clans %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = neg_r_clans %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "inc_all", weight_var = "clan_weight")
)
inc_ratios <- make_ratio_wide(inc_ratio_specs)
write.csv(inc_ratios, file.path(out_dir, "income_race_ratios.csv"), row.names = FALSE)

# Wealth (excl. home equity) ratios ─────────────────────────────────────────
wnh_ratio_specs <- list(
  list(name = "hh_u",       black_df = hh_wealth        %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = hh_wealth        %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth_nohouse", weight_var = NULL),
  list(name = "hh_w",       black_df = hh_wealth        %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = hh_wealth        %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth_nohouse", weight_var = "fam_weight"),
  list(name = "r_hh_u",     black_df = r_hh_wealth      %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = r_hh_wealth      %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth_nohouse", weight_var = NULL),
  list(name = "r_hh_w",     black_df = r_hh_wealth      %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = r_hh_wealth      %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth_nohouse", weight_var = "fam_weight"),
  list(name = "cl_u",       black_df = clans_wealth     %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = clans_wealth     %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth_nohouse", weight_var = NULL),
  list(name = "cl_w",       black_df = clans_wealth     %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = clans_wealth     %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth_nohouse", weight_var = "clan_weight"),
  list(name = "r_cl_u",     black_df = r_clans_wealth   %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = r_clans_wealth   %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth_nohouse", weight_var = NULL),
  list(name = "r_cl_w",     black_df = r_clans_wealth   %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = r_clans_wealth   %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth_nohouse", weight_var = "clan_weight"),
  list(name = "neg_r_hh_w", black_df = neg_r_hh_wealth  %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = neg_r_hh_wealth  %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth_nohouse", weight_var = "fam_weight"),
  list(name = "neg_r_cl_w", black_df = neg_r_clans_wealth %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = neg_r_clans_wealth %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth_nohouse", weight_var = "clan_weight")
)
wnh_ratios <- make_ratio_wide(wnh_ratio_specs)
write.csv(wnh_ratios, file.path(out_dir, "wealth_nohouse_race_ratios.csv"), row.names = FALSE)

# Wealth (incl. home equity) ratios ─────────────────────────────────────────
wh_ratio_specs <- list(
  list(name = "hh_u",       black_df = hh_wealth        %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = hh_wealth        %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth", weight_var = NULL),
  list(name = "hh_w",       black_df = hh_wealth        %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = hh_wealth        %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth", weight_var = "fam_weight"),
  list(name = "r_hh_u",     black_df = r_hh_wealth      %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = r_hh_wealth      %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth", weight_var = NULL),
  list(name = "r_hh_w",     black_df = r_hh_wealth      %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = r_hh_wealth      %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth", weight_var = "fam_weight"),
  list(name = "cl_u",       black_df = clans_wealth     %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = clans_wealth     %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth", weight_var = NULL),
  list(name = "cl_w",       black_df = clans_wealth     %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = clans_wealth     %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth", weight_var = "clan_weight"),
  list(name = "r_cl_u",     black_df = r_clans_wealth   %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = r_clans_wealth   %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth", weight_var = NULL),
  list(name = "r_cl_w",     black_df = r_clans_wealth   %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = r_clans_wealth   %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth", weight_var = "clan_weight"),
  list(name = "neg_r_hh_w", black_df = neg_r_hh_wealth  %>% filter(black_head == 1, !is.na(black_head)),
                         nonblack_df = neg_r_hh_wealth  %>% filter(black_head == 0, !is.na(black_head)),
       value_var = "wealth", weight_var = "fam_weight"),
  list(name = "neg_r_cl_w", black_df = neg_r_clans_wealth %>% filter(black_clan == 1, !is.na(black_clan)),
                         nonblack_df = neg_r_clans_wealth %>% filter(black_clan == 0, !is.na(black_clan)),
       value_var = "wealth", weight_var = "clan_weight")
)
wh_ratios <- make_ratio_wide(wh_ratio_specs)
write.csv(wh_ratios, file.path(out_dir, "wealth_withhome_race_ratios.csv"), row.names = FALSE)
