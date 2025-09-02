# INPUTS: 3_households/output/households.rds, 3_robust_households/output/robust_households.rds
# INPUTS: 4_clans/output/clans.rds, 4_robust_clans/output/robust_clans.rds
# OUTPUTS: 5_calculate_gini/output/inc_by_year_sgini.csv, 5_calculate_gini/output/wealth_by_year_sgini.csv


# LOAD DATA ------------------------------------------------------------------
hh     <- readRDS(here("3_households", "output", "households.rds"))
clans  <- readRDS(here("4_clans",      "output", "clans.rds"))

# Robust versions
r_hh    <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans",      "output", "robust_clans.rds"))

# Wealth data subset
hh_wealth     <- hh     %>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))
r_hh_wealth   <- r_hh   %>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))
clans_wealth  <- clans  %>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))
r_clans_wealth<- r_clans%>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))

# Generalized Gini per year ------------------------------------------
gini_by_year_sgini <- function(df, value_var, name, nu = 2) {
  v <- as_name(enquo(value_var))
  
  out <- df %>%
    filter(is.finite(!!sym(v))) %>%
    group_by(year) %>%
    summarise(
      gini = sgini(!!sym(v), nu = nu)$Gini,
      bcGini = sgini(!!sym(v), nu = nu)$bcGini,
      .groups = "drop"
    ) %>%
    rename(!!name := gini,
           !!paste0(name, "_bc") := bcGini)
  
  out
}

# Append row with ALL-year means
append_mean_row <- function(df) {
  mean_row <- df %>%
    summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 3)))
  
  if ("year" %in% names(df)) {
    df <- df %>% mutate(year = as.character(year))
    mean_row <- mean_row %>% mutate(year = "ALL") %>% relocate(year, .before = 1)
  }
  bind_rows(mean_row, df)
}

# INCOME ---------------------------------------------------------------------
inc_dfs <- list(
  gini_by_year_sgini(r_hh,    inc_all, "r_hh_sgini"),
  gini_by_year_sgini(r_clans, inc_all_mean, "r_cl_sgini_mean"),
  gini_by_year_sgini(r_clans, inc_all, "r_cl_sgini")
)

inc_by_year_sgini <- reduce(inc_dfs, full_join, by = "year") %>% arrange(year)
inc_by_year_sgini <- append_mean_row(inc_by_year_sgini)
write.csv(inc_by_year_sgini, here("5_calculate_gini", "output", "inc_by_year_sgini.csv"), row.names = FALSE)

# WEALTH ---------------------------------------------------------------------
wealth_dfs <- list(
  gini_by_year_sgini(r_hh_wealth,    wealth_nohouse, "r_hh_sgini_wealth"),
  gini_by_year_sgini(r_clans_wealth, wealth_nohouse_mean, "r_cl_sgini_wealth_mean"),
  gini_by_year_sgini(r_clans_wealth, wealth_nohouse, "r_cl_sgini_wealth")
)

wealth_by_year_sgini <- reduce(wealth_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year_sgini <- append_mean_row(wealth_by_year_sgini)
write.csv(wealth_by_year_sgini, here("5_calculate_gini", "output", "wealth_by_year_sgini.csv"), row.names = FALSE)

# CLEANUP --------------------------------------------------------------------
rm(list = ls())
