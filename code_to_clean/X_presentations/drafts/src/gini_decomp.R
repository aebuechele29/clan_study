library(dineq)   # gini_decomp() for within/between/overlap by subgroup

# INPUTS ------------------------------------------------------------------
r_hh    <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

# Wealth years 
wealth_years <- c(1984, 1989, 1994, seq(1999, 2021, by = 2))

r_hh_wealth <- r_hh %>% filter(year %in% wealth_years)

# Functions
decomp_one_year <- function(df, value_var, group_var, weight_var = NULL) {
  # keep finite values only
  df <- df %>% filter(is.finite(.data[[value_var]]), !is.na(.data[[group_var]]))
  if (nrow(df) == 0 || dplyr::n_distinct(df[[group_var]]) < 2) {
    return(tibble(
      gini_total = NA_real_, gini_within = NA_real_,
      gini_between = NA_real_, gini_overlap = NA_real_
    ))
  }
  w <- if (!is.null(weight_var)) df[[weight_var]] else NULL

  gd <- gini_decomp(x = df[[value_var]], z = df[[group_var]], weights = w)  # returns a list
  tibble(
    gini_total   = unname(gd$gini_decomp$gini_total),
    gini_within  = unname(gd$gini_decomp$gini_within),
    gini_between = unname(gd$gini_decomp$gini_between),
    gini_overlap = unname(gd$gini_decomp$gini_overlap)
  )
}

decomp_by_year <- function(df, value_var, group_var, weight_var = NULL, label) {
  df %>%
    group_by(year) %>%
    group_split() %>%
    map_df(function(d) {
      y <- unique(d$year)
      res <- decomp_one_year(d, value_var = value_var, group_var = group_var, weight_var = weight_var)
      res %>% mutate(year = y)
    }) %>%
    select(year, everything()) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    rename_with(~ paste0(label, "_", .x), .cols = -year)
}


# Income decomposition
# Weighted (fam_weight) decomposition of household income by clan
inc_hh_w <- decomp_by_year(
  df = r_hh,
  value_var = "inc_all",
  group_var = "id1968",
  weight_var = "fam_weight",
  label = "r_hh_w_inc"
)

write_csv(inc_hh_w, here("5_calculate_gini", "output", "income_decomp.csv"))


# Wealth decomposition
# Weighted wealth decomposition
w_hh_w <- decomp_by_year(
  df = r_hh_wealth,
  value_var = "wealth_nohouse",
  group_var = "id1968",
  weight_var = "fam_weight",
  label = "r_hh_w_wealth"
)

write_csv(w_hh_w, here("5_calculate_gini", "output", "wealth_decomp.csv"))

