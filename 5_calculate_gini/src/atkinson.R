
# LOAD DATA ------------------------------------------------------------------
hh <- readRDS(here("3_households", "output", "households.rds"))
clans <- readRDS(here("4_clans", "output", "clans.rds"))

# Robust versions: limited to households that belong to clans with more than one household
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

# Functions
dedup_households <- function(df) {
  df %>%
    filter(!is.na(id1968), !is.na(fam_id)) %>%
    arrange(year, id1968, fam_id) %>%
    distinct(year, id1968, fam_id, .keep_all = TRUE)
}

# Unweighted Atkinson (drop negatives always; if epsilon = 1, drop zeros too_
atkinson_unweighted <- function(x, epsilon = 0.5) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  x <- x[x >= 0]                 # drop negatives
  if (epsilon == 1) x <- x[x > 0]  # drop zeros for log case
  if (!length(x)) return(NA_real_)

  mu <- mean(x)
  if (!is.finite(mu) || mu <= 0) return(NA_real_)

  if (epsilon == 1) {
    mu_e <- exp(mean(log(x)))
  } else {
    base <- mean(x^(1 - epsilon))
    mu_e <- base^(1 / (1 - epsilon))
  }
  1 - (mu_e / mu)
}

# Compute unweighted Atkinson by year for a df/value & epsilon vector
atkinson_by_year_unweighted <- function(df, value_var, eps = c(0.5, 1, 2)) {
  v <- rlang::ensym(value_var)
  df %>%
    select(year, !!v) %>%
    group_by(year) %>%
    group_split() %>%
    map_dfr(function(d) {
      y <- unique(d$year)
      tibble(
        year = y,
        epsilon = eps,
        atkinson = map_dbl(eps, ~ atkinson_unweighted(d[[rlang::as_name(v)]], epsilon = .x))
      )
    }) %>%
    arrange(year, epsilon)
}

# Households

atk_hh <- r_hh %>%
  filter(is.finite(inc_all)) %>%
  dedup_households() %>%
  atkinson_by_year_unweighted(value_var = "inc_all", eps = c(0.5, 1, 2)) %>%
  mutate(group = "Households")

# Clans

atk_clan <- r_clans %>%
  filter(is.finite(inc_all), !is.na(id1968)) %>%
  arrange(year, id1968) %>%
  distinct(year, id1968, .keep_all = TRUE) %>%
  atkinson_by_year_unweighted(value_var = "inc_all", eps = c(0.3, 0.5, 1, 2)) %>%
  mutate(group = "Clans")


atk_income_wide <- bind_rows(atk_hh, atk_clan) %>%
  select(year, epsilon, group, atkinson) %>%
  pivot_wider(
    names_from = group,
    values_from = atkinson
  ) %>%
  arrange(year, epsilon) %>%
  mutate(across(where(is.numeric) & !matches("^year$"), ~ round(.x, 4)))

write_csv(atk_income_wide,
          here("5_calculate_gini", "output", "atkinson.csv"))



# Plot
ggplot(atk_income_unw, aes(x = year, y = atkinson, color = factor(epsilon), group = epsilon)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +
  scale_color_discrete(name = expression(epsilon)) +
  labs(
    title = "Unweighted Atkinson Index of Income (Households vs. Clans)",
    x = "Year",
    y = "Atkinson index"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())
