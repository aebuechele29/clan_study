# INPUTS: 3_households/output/households.rds, 3_robust_households/output/robust_households.rds
# INPUTS: 4_clans/output/clans.rds, 4_robust_clans/output/robust_clans.rds
# OUTPUTS: 5_calculate_gini/output/inc_by_year.csv, 5_calculate_gini/output/wealth_by_year.csv

# LOAD DATA ------------------------------------------------------------------
hh <- readRDS(here("3_households", "output", "households.rds"))
clans <- readRDS(here("4_clans", "output", "clans.rds"))

# Robust versions: limited to households that belong to clans with more than one household
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

# Wealth data: limited to years when wealth data is available 
    # There are two measures of wealth - welath including home equity (wealth) and wealth excluding home equity (wealth_nohouse)
    # Wealth excluding home equity has less missing data, as this question began being collected by the PSID when the wealth supplement began
    # The wealth supplement was fielded in 1984, 1989, 1994, and every other year from 1999 to 2021

hh_wealth <- hh %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

r_hh_wealth <- r_hh %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

clans_wealth <- clans %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

r_clans_wealth <- r_clans %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 



# GINI CALCULATION FUNCTION ------------------------------------------
gini_by_year_svy <- function(df, value_var, weight_var = NULL, simple_design = FALSE, with_se = FALSE) {
  v <- as_name(enquo(value_var))
  
  # Ensure sorted and cleaned data per year
  df <- df %>%
    filter(is.finite(!!sym(v))) %>%
    arrange(year, !!sym(v))

  if (simple_design) {
    # Unweighted design
    des <- svydesign(ids = ~1, weights = ~1, data = df) |> convey_prep()
    vt <- if (with_se) c("se") else NULL
  } else {
    # Weighted design
    if (is.null(weight_var)) {
      df <- df %>% mutate(.one = 1)
      w_formula <- ~.one
    } else {
      w_formula <- as.formula(paste0("~", weight_var))
    }
    des <- svydesign(ids = ~cluster, strata = ~stratum, weights = w_formula, 
                     data = df, nest = TRUE) |> convey_prep()
    vt <- if (with_se) c("se") else NULL
  }

  out <- svyby(
    as.formula(paste0("~", v)),
    ~year,
    design = des,
    FUN = svygini,
    na.rm = TRUE,
    vartype = vt,
    keep.names = FALSE
  ) %>%
    rename(gini = !!v) %>%
    mutate(
      gini = round(as.numeric(gini), 3),
      se   = if ("se" %in% names(.)) round(as.numeric(se), 3) else NULL
    ) %>%
    arrange(year)

  # Include SE for weighted versions
  if (!with_se) {
    out <- select(out, year, gini)
  } else {
    out <- select(out, year, gini, se)
  }

  out
}

append_mean_row <- function(df) {
  # Compute means
  mean_row <- df %>%
    summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 3)))
  
  if ("year" %in% names(df)) {
    df <- df %>% mutate(year = as.character(year))
    mean_row <- mean_row %>%
      mutate(year = "ALL") %>%
      relocate(year, .before = 1)
  }
  
  # Bind and move ALL row to top
  df <- bind_rows(mean_row, df)
  
  df
}



# HELPER FUNCTION TO RUN GINI CALCULATIONS ------------------------------------------
run_gini <- function(df, var, weight = NULL, simple = FALSE, se = FALSE, name) {
  out <- gini_by_year_svy(df, !!sym(var), weight_var = weight, 
                          simple_design = simple, with_se = se)
  if (se) {
    out <- rename(out, !!name := gini, !!paste0(name, "_se") := se)
  } else {
    out <- rename(out, !!name := gini)
  }
  out
}


# CALL INCOME AND WEALTH GINI CALCULATIONS ------------------------------------------
# Income
inc_dfs <- list(
  # Households
  # run_gini(hh,      "inc_all",        NULL,         FALSE, FALSE, "hh_u_inc"),
  # run_gini(hh,      "inc_all",        "fam_weight", FALSE, TRUE,  "hh_w_inc"),
  run_gini(r_hh,    "inc_all",        NULL,         FALSE, FALSE, "r_hh_u_inc"),
  run_gini(r_hh,    "inc_all",        "fam_weight", FALSE, TRUE,  "r_hh_w_inc"),
  
  # Clans - medians
  # run_gini(clans,   "inc_all_median", NULL,         TRUE,  FALSE, "cl_u_inc_median"),
  # run_gini(r_clans, "inc_all_median", NULL,         TRUE,  FALSE, "r_cl_u_inc_median"),
  
  # Clans - means
  # run_gini(clans,   "inc_all_mean",   NULL,         TRUE,  FALSE, "cl_u_inc_mean"),
  run_gini(r_clans, "inc_all_mean",   NULL,         TRUE,  FALSE, "r_cl_u_inc_mean"),
  run_gini(r_clans, "inc_all",   "clan_weight",         FALSE,  TRUE, "r_cl_w_inc_mean")
)

inc_by_year <- reduce(inc_dfs, full_join, by = "year") %>% arrange(year)
inc_by_year <- append_mean_row(inc_by_year)
write.csv(inc_by_year, here("5_calculate_gini", "output", "inc_by_year.csv"), row.names = FALSE)



# Wealth (excluding home equity)
wealth_dfs <- list(
  # Households
  # run_gini(hh_wealth,   "wealth_nohouse",        NULL,         FALSE, FALSE, "hh_u_wealth"),
  # run_gini(hh_wealth,   "wealth_nohouse",        "fam_weight", FALSE, TRUE,  "hh_w_wealth"),
  run_gini(r_hh_wealth, "wealth_nohouse",        NULL,         FALSE, FALSE, "r_hh_u_wealth"),
  run_gini(r_hh_wealth, "wealth_nohouse",        "fam_weight", FALSE, TRUE,  "r_hh_w_wealth"),
  
  # Clans - medians
  # run_gini(clans_wealth,   "wealth_nohouse_median", NULL,         TRUE,  FALSE, "cl_u_wealth_median"),
  # run_gini(r_clans_wealth, "wealth_nohouse_median", NULL,         TRUE,  FALSE, "r_cl_u_wealth_median"),
  
  # Clans - means
  # run_gini(clans_wealth,   "wealth_nohouse_mean",   NULL,         TRUE,  FALSE, "cl_u_wealth_mean"),
  run_gini(r_clans_wealth, "wealth_nohouse_mean",   NULL,         TRUE,  FALSE, "r_cl_u_wealth_mean"),
  run_gini(r_clans_wealth, "wealth_nohouse",   "clan_weight",         FALSE,  TRUE, "r_cl_w_wealth_mean")
)

wealth_by_year <- reduce(wealth_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year <- append_mean_row(wealth_by_year)
write.csv(wealth_by_year, here("5_calculate_gini", "output", "wealth_by_year.csv"), row.names = FALSE)



# HELPER FUNCTION TO RUN GINI CALCULATIONS BY RACE ------------------------------------------
run_gini_race <- function(df_black, df_nonblack, var, weight = NULL, simple = FALSE, se = FALSE, name) {
  black_df     <- run_gini(df_black,    var, weight, simple, se, paste0(name, "_black"))
  nonblack_df  <- run_gini(df_nonblack, var, weight, simple, se, paste0(name, "_nonblack"))
  full_join(black_df, nonblack_df, by = "year")
}

# CALL INCOME AND WEALTH FOR GINI BY RACE CALCULATIONS ------------------------------------------
# Income
# Households
hh_u_inc      <- run_gini_race(
  r_hh %>% filter(black_head == 1),
  r_hh %>% filter(black_head != 1 & !is.na(black_head)),
  "inc_all", NULL, FALSE, FALSE, "r_hh_u_inc"
)
hh_w_inc      <- run_gini_race(
  r_hh %>% filter(black_head == 1),
  r_hh %>% filter(black_head != 1 & !is.na(black_head)),
  "inc_all", "fam_weight", FALSE, TRUE, "r_hh_w_inc"
)

# Clans - median
cl_inc_median <- run_gini_race(
  r_clans %>% filter(black_clan == 1),
  r_clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all_median", NULL, TRUE, FALSE, "r_cl_u_inc_median"
)

# Clans - mean
cl_inc_mean   <- run_gini_race(
  r_clans %>% filter(black_clan == 1),
  r_clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all_mean", NULL, TRUE, FALSE, "r_cl_u_inc_mean"
)

cl_inc_mean_w   <- run_gini_race(
  r_clans %>% filter(black_clan == 1),
  r_clans %>% filter(black_clan != 1 & !is.na(black_clan)),
  "inc_all_mean", "clan_weight", FALSE, TRUE, "r_cl_w_inc_mean"
)

inc_by_year_race <- list(hh_u_inc, hh_w_inc, cl_inc_median, cl_inc_mean, cl_inc_mean_w) %>%
  reduce(full_join, by = "year") %>%
  arrange(year)
inc_by_year_race <- append_mean_row(inc_by_year_race)
write.csv(inc_by_year_race, here("5_calculate_gini", "output", "inc_by_year_race.csv"), row.names = FALSE)

# Wealth
# Households
hh_u_wealth      <- run_gini_race(
  r_hh_wealth %>% filter(black_head == 1),
  r_hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth_nohouse", NULL, FALSE, FALSE, "r_hh_u_wealth"
)
hh_w_wealth      <- run_gini_race(
  r_hh_wealth %>% filter(black_head == 1),
  r_hh_wealth %>% filter(black_head != 1 & !is.na(black_head)),
  "wealth_nohouse", "fam_weight", FALSE, TRUE, "r_hh_w_wealth"
)

# Clans - median
cl_wealth_median <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse_median", NULL, TRUE, FALSE, "r_cl_u_wealth_median"
)

# Clans - mean
cl_wealth_mean   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse_mean", NULL, TRUE, FALSE, "r_cl_u_wealth_mean"
)

cl_wealth_mean_w   <- run_gini_race(
  r_clans_wealth %>% filter(black_clan == 1),
  r_clans_wealth %>% filter(black_clan != 1 & !is.na(black_clan)),
  "wealth_nohouse_mean", "clan_weight", FALSE, TRUE, "r_cl_w_wealth_mean"
)

wealth_by_year_race <- list(hh_u_wealth, hh_w_wealth, cl_wealth_median, cl_wealth_mean, cl_wealth_mean_w) %>%
  reduce(full_join, by = "year") %>%
  arrange(year)
wealth_by_year_race <- append_mean_row(wealth_by_year_race)
write.csv(wealth_by_year_race, here("5_calculate_gini", "output", "wealth_by_year_race.csv"), row.names = FALSE)


rm(list = ls())