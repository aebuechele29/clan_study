# LOAD DATA ------------------------------------------------------------------
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

r_hh_wealth <- r_hh %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

r_clans_wealth <- r_clans %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

# Get all available years
years_income <- sort(unique(r_hh$year))
years_wealth <- sort(unique(r_hh$year))  # or r_hh_wealth$year if wealth is subset

# FUNCTIONS ------------------------------------------------------------------

# Lorenz curve table
lorenz_tbl <- function(x, w = NULL) {
  L <- if (is.null(w)) Lc(x) else Lc(x, n = w)  # Apply weights
  tibble(p = L$p, L = L$L)
}

# Build Lorenz curves (weighted + unweighted)
get_lorenz_both <- function(df, value_var, weight_var, years, unit_label) {
  base <- df %>%
    filter(year %in% years, is.finite(.data[[value_var]])) %>%
    transmute(year, value = .data[[value_var]], w = .data[[weight_var]])

  # Unweighted
  unw <- base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      lorenz_tbl(d$value) %>% mutate(year = unique(d$year), weighting = "Unweighted")
    })

  # Weighted
  wtd <- base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      lorenz_tbl(d$value, d$w) %>% mutate(year = unique(d$year), weighting = "Weighted")
    })

  bind_rows(unw, wtd) %>%
    mutate(Unit = factor(unit_label, levels = c("Household", "Clan")))
}

# Numerical integration (trapezoidal rule)
trapz <- function(x, y) {
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# Aaberge’s D1, D2, D3 (first three moments of the Lorenz curve)
calc_D123 <- function(p, L) {
  D1 <- trapz(p, L)                 # ∫ L(u) du
  D2 <- 2 * trapz(p, (1 - p) * L)   # 2∫ (1-u)L(u) du
  D3 <- 3 * trapz(p, (1 - p)^2 * L) # 3∫ (1-u)^2L(u) du
  tibble(D1 = D1, D2 = D2, D3 = D3)
}

# Gini from Lorenz curve
calc_gini_from_lorenz <- function(p, L) {
  1 - 2 * trapz(p, L)
}

# INCOME ---------------------------------------------------------------------
inc_hh   <- get_lorenz_both(r_hh,    "inc_all",     "fam_weight",  years_income, "Household")
inc_clan <- get_lorenz_both(r_clans, "inc_all",     "clan_weight", years_income, "Clan")
inc_all  <- bind_rows(inc_hh, inc_clan)

inc_unw <- inc_all %>%
  filter(weighting == "Unweighted") %>%
  group_by(Unit, year) %>%
  summarise(
    calc_D123(p, L),
    gini = calc_gini_from_lorenz(p, L),
    .groups = "drop"
  ) %>%
  mutate(valid_d1 = 1 - 2*D1)

write_csv(inc_unw, here("9_scratch", "output", "income_D123.csv"))


# WEALTH ---------------------------------------------------------------------
w_hh   <- get_lorenz_both(r_hh_wealth,    "wealth_nohouse",     "fam_weight",  years_wealth, "Household")
w_clan <- get_lorenz_both(r_clans_wealth, "wealth_nohouse",     "clan_weight", years_wealth, "Clan")
w_all  <- bind_rows(w_hh, w_clan)

w_unw <- w_all %>%
  filter(weighting == "Unweighted") %>%
  group_by(Unit, year) %>%
  summarise(
    calc_D123(p, L),
    gini = calc_gini_from_lorenz(p, L),
    .groups = "drop"
  ) %>%
  mutate(valid_d1 = 1 - 2*D1)

write_csv(inc_unw, here("9_scratch", "output", "wealth_D123.csv"))





