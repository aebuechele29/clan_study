# INPUTS: 3_households/output/households.rds, 3_robust_households/output/robust_households.rds
# INPUTS: 4_clans/output/clans.rds, 4_robust_clans/output/robust_clans.rds
# OUTPUTS: 6_lorenz/output/income_lorenz.pdf, 6_lorenz/output/wealth_lorenz.pdf

# LOAD DATA ------------------------------------------------------------------
# hh <- readRDS(here("3_households", "output", "households.rds"))
# clans <- readRDS(here("4_clans", "output", "clans.rds"))

# Robust versions: limited to households that belong to clans with more than one household
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

# Wealth data: limited to years when wealth data is available 
    # There are two measures of wealth - welath including home equity (wealth) and wealth excluding home equity (wealth_nohouse)
    # Wealth excluding home equity has less missing data, as this question began being collected by the PSID when the wealth supplement began
    # The wealth supplement was fielded in 1984, 1989, 1994, and every other year from 1999 to 2021

r_hh_wealth <- r_hh %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

r_clans_wealth <- r_clans %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

# Choose years to plot Lorenz curves for
years_income <- c(2019) 
years_wealth <- c(2019)

# LORENZ FUNCTIONS ------------------------------------------------------------------------------------
# Calculate Lorenz curve
lorenz_tbl <- function(x, w = NULL) {
  L <- if (is.null(w)) Lc(x) else Lc(x, n = w)  # Apply weights
  tibble(p = L$p, L = L$L)
}

# Build Lorenz plots for multiple df/variable combinations over multiple years (returns weighted & unweighted)
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


# INCOME 
inc_hh   <- get_lorenz_both(r_hh,    "inc_all",     "fam_weight",  years_income, "Household")
inc_clan <- get_lorenz_both(r_clans, "inc_all","clan_weight", years_income, "Clan")
inc_all  <- bind_rows(inc_hh, inc_clan)

# Plot
p_inc <- ggplot(
  inc_all,
  aes(x = p, y = L,
      color = factor(year),
      linetype = weighting,
      group = interaction(year, weighting, Unit))
) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_linetype_manual(values = c(Unweighted = "dotted", Weighted = "solid")) +
  labs(
    title = paste0("Lorenz Curves — Income (", paste(years_income, collapse = ", "), ")"),
    x = "Cumulative Proportion of Units",
    y = "Cumulative Proportion of Income",
    color = "Year",
    linetype = "Weighting"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  facet_wrap(~ Unit, nrow = 1)

showtext_auto()

p_inc <- p_inc + theme(text = element_text(family = "Times New Roman")) 

ggsave(
  filename = here("6_lorenz", "output", "income_lorenz.pdf"),
  plot = p_inc,
  width = 12,
  height = 5
)

# WEALTH
w_hh   <- get_lorenz_both(r_hh_wealth,    "wealth_nohouse",          "fam_weight",  years_wealth, "Household")
w_clan <- get_lorenz_both(r_clans_wealth, "wealth_nohouse","clan_weight", years_wealth, "Clan")
w_all  <- bind_rows(w_hh, w_clan)

# Plot
p_w <- ggplot(
  w_all,
  aes(x = p, y = L,
      color = factor(year),
      linetype = weighting,
      group = interaction(year, weighting, Unit))
) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_linetype_manual(values = c(Unweighted = "dotted", Weighted = "solid")) +
  labs(
    title = paste0("Lorenz Curves - Wealth (", paste(years_wealth, collapse = ", "), ")"),
    x = "Cumulative Proportion of Units",
    y = "Cumulative Proportion of Wealth",
    color = "Year",
    linetype = "Weighting"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  facet_wrap(~ Unit, nrow = 1)

showtext_auto()

p_w <- p_w + theme(text = element_text(family = "Times New Roman")) 

ggsave(
  filename = here("6_lorenz", "output", "wealth_lorenz.pdf"),
  plot = p_w,
  width = 12,
  height = 5
)

rm(list = ls())


