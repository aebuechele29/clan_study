# INPUTS: 6_clean_clans/output/clean_clans.rds, 4_clean_households/output/clean_hs.rds
# OUTPUTS: 7_analysis/output

clans <- readRDS(here("6_clean_clans", "output", "clean_clans.rds"))
households <- readRDS(here("4_clean_households", "output", "clean_hs.rds"))
library(ineq)

# Variables to compute Gini for
clan_vars <- c("inc_all_median_clan", "wealth_nohouse_median_clan", "wealth_home_median_clan")
hh_vars   <- c("inc_all", "wealth_nohouse", "wealth_home")


gini_by_var <- function(df, varname) {
  df %>%
    group_by(year) %>%
    summarise(
      gini = if (sum(!is.na(.data[[varname]])) > 1) {
        ineq::Gini(na.omit(.data[[varname]]))
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(variable = varname)
}

# Calculate Gini for clan-level 
# Clan-level Gini calculations
gini_inc_all_median_clan <- gini_by_var(clans, "inc_all_median_clan") %>% 
  mutate(level = "clan", type = "median")
gini_inc_all_mean_clan <- gini_by_var(clans, "inc_all_mean_clan") %>% 
  mutate(level = "clan", type = "mean")
gini_wealth_home_clan <- gini_by_var(clans, "wealth_home_median_clan") %>% 
  mutate(level = "clan", type = "median")
gini_wealth_nohouse_clan <- gini_by_var(clans, "wealth_nohouse_median_clan") %>% 
  mutate(level = "clan", type = "median")
gini_wealth_home_mean_clan <- gini_by_var(clans, "wealth_home_mean_clan") %>% 
  mutate(level = "clan", type = "mean")
gini_wealth_nohouse_mean_clan <- gini_by_var(clans, "wealth_nohouse_mean_clan") %>% 
  mutate(level = "clan", type = "mean")

gini_clans <- bind_rows(
  gini_inc_all_median_clan,
  gini_inc_all_mean_clan,
  gini_wealth_home_clan,
  gini_wealth_nohouse_clan,
  gini_wealth_home_mean_clan,
  gini_wealth_nohouse_mean_clan
)

# Household-level Gini calculations
gini_inc_all_hh <- gini_by_var(households, "inc_all") %>% 
  mutate(level = "household", type = "raw")
gini_wealth_home_hh <- gini_by_var(households, "wealth_home") %>% 
  mutate(level = "household", type = "raw")
gini_wealth_nohouse_hh <- gini_by_var(households, "wealth_nohouse") %>% 
  mutate(level = "household", type = "raw")

gini_households <- bind_rows(
  gini_inc_all_hh,
  gini_wealth_home_hh,
  gini_wealth_nohouse_hh
)

# Combine all
gini_combined <- bind_rows(gini_clans, gini_households) %>%
  select(year, variable, level, type, gini) %>%
  arrange(year, variable, level, type)

gini_wide <- gini_combined %>%
  mutate(
    var_level_type = case_when(
      level == "household" ~ paste0(variable, "_household"),
      level == "clan" ~ paste0(variable, "_", type, "_clan")
    )
  ) %>%
  select(year, var_level_type, gini) %>%
  pivot_wider(
    names_from = var_level_type,
    values_from = gini
  ) %>%
  arrange(year)

# Summarize across years
gini_summary <- gini_combined %>%
  group_by(variable, level, type) %>%
  summarise(
    mean_gini = mean(gini, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(var_label = case_when(
    level == "household" ~ paste0(variable, "_household"),
    level == "clan" ~ paste0(variable, "_", type, "_clan")
  )) %>%
  select(var_label, mean_gini) %>%
  pivot_wider(
    names_from = var_label,
    values_from = mean_gini
  )

# Save to CSV
write.csv(gini_wide, here("7_analysis", "output", "gini_by_year.csv"), row.names = FALSE)
write.csv(gini_summary, here("7_analysis", "output", "gini_all.csv"), row.names = FALSE)


# Line plot over time 
gini_combined <- gini_combined %>%
  mutate(
    kind = case_when(
      grepl("^inc_", variable) ~ "Income",
      grepl("^wealth_", variable) ~ "Wealth",
      TRUE ~ "other"
    )
  )

pdf(here("7_analysis", "output", "gini_plot.pdf"), width = 8, height = 5) 

ggplot(gini_combined, aes(x = year, y = gini, color = level, linetype = level)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1) +
  facet_wrap(~ kind, scales = "fixed") +
  scale_y_continuous(limits = c(.3, 1)) +
  theme_minimal() +
  labs(
    title = "Smoothed Gini Coefficient Over Time",
    y = "Gini Coefficient",
    x = "Year",
    color = "Level",
    linetype = "Level"
  )

dev.off()


# Calculate Gini by race -----------------------------------------------------------
clans <- clans %>%
  mutate(
    race_group = case_when(
      prop_race_black_ > 0.5 ~ "Majority Black",
      prop_race_white_ > 0.5 ~ "Majority White",
      prop_race_other_ > 0.5 ~ "Majority Other Non-White",
      TRUE ~ "Mixed/No Majority"
    )
  )

households <- households %>%
  mutate(
    race_group = case_when(
      prop_race_black > 0.5 ~ "Majority Black",
      prop_race_white > 0.5 ~ "Majority White",
      prop_race_other > 0.5 ~ "Majority Other Non-White",
      TRUE ~ "Mixed/No Majority"
    )
  )


gini_by_race_group <- function(df, varname, level_label) {
  df %>%
    group_by(year, race_group) %>%
    summarise(
      gini = if (sum(!is.na(.data[[varname]])) > 1) {
        ineq::Gini(na.omit(.data[[varname]]))
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(
      variable = varname,
      level = level_label
    )
}

gini_race_clan <- bind_rows(
  gini_by_race_group(clans, "inc_all_median_clan", "clan"),
  gini_by_race_group(clans, "wealth_nohouse_median_clan", "clan"),
  gini_by_race_group(clans, "wealth_home_median_clan", "clan")
)

gini_race_hh <- bind_rows(
  gini_by_race_group(households, "inc_all", "household"),
  gini_by_race_group(households, "wealth_nohouse", "household"),
  gini_by_race_group(households, "wealth_home", "household")
)

gini_race_all <- bind_rows(gini_race_clan, gini_race_hh) %>%
  mutate(
    kind = case_when(
      grepl("^inc_", variable) ~ "Income",
      grepl("^wealth_", variable) ~ "Wealth"
    )
  )

gini_race_filtered <- gini_race_all %>%
  filter(race_group %in% c("Majority Black", "Majority White"))

pdf(here("7_analysis", "output", "gini_majority_black_white.pdf"), width = 9, height = 5)

ggplot(gini_race_filtered, aes(x = year, y = gini, color = race_group, linetype = level)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1) +
  facet_wrap(~ kind, scales = "fixed") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  labs(
    title = "Gini Coefficient Over Time: Majority Black vs. Majority White",
    y = "Gini Coefficient",
    x = "Year",
    color = "Race Group",
    linetype = "Level"
  )

dev.off()

