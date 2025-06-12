# INPUTS: 6_clean_clans/output/clean_clans.rds, 4_clean_households/output/clean_hs.rds
# OUTPUTS: 7_analysis/output

# This file calculates the Gini coefficient for income and wealth for:
  # All clans and households
  # Clans and households where clans have more than one household
  # Clans and households by race (either Black or Non-Black)

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

# CALCULATE GINI FOR ALL OBSERVATIONS ------------------------------------------------------
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
write.csv(gini_wide, here("7_analysis", "output", "gini_all_by_year.csv"), row.names = FALSE)
write.csv(gini_summary, here("7_analysis", "output", "gini_all_across.csv"), row.names = FALSE)


# Line plot over time 
gini_combined <- gini_combined %>%
  mutate(
    kind = case_when(
      grepl("^inc_", variable) ~ "Income",
      grepl("^wealth_", variable) ~ "Wealth",
      TRUE ~ "other"
    )
  )

pdf(here("7_analysis", "output", "gini_all_plot.pdf"), width = 8, height = 5) 

ggplot(gini_combined, aes(x = year, y = gini, color = level, linetype = level)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1) +
  facet_wrap(~ kind, scales = "fixed") +
  scale_y_continuous(limits = c(.3, 1)) +
  theme_minimal() +
  labs(
    title = "Gini Coefficient Over Time - All Clans and Households",
    y = "Gini Coefficient",
    x = "year",
    color = "Level",
    linetype = "Level"
  )

dev.off()


# REMOVE CLANS WITH ONLY ONE HOUSEHOLD AND RECALCULATE GINI --------------------------------------
robust_clans <- clans %>%
  filter(numclan > 1)

# Save clans with only one household to a separate file
single_households <- clans %>%
  filter(numclan == 1) %>%
  rename(fam_id = fam_id_1) 

# Remove households that are in the single_household file from households
robust_households <- anti_join(households, single_households, by = c("year", "fam_id"))

# Calculate Gini for clan-level using robust_clans
gini_inc_all_median_clan <- gini_by_var(robust_clans, "inc_all_median_clan") %>% 
  mutate(level = "clan", type = "median")
gini_inc_all_mean_clan <- gini_by_var(robust_clans, "inc_all_mean_clan") %>% 
  mutate(level = "clan", type = "mean")
gini_wealth_home_clan <- gini_by_var(robust_clans, "wealth_home_median_clan") %>% 
  mutate(level = "clan", type = "median")
gini_wealth_nohouse_clan <- gini_by_var(robust_clans, "wealth_nohouse_median_clan") %>% 
  mutate(level = "clan", type = "median")
gini_wealth_home_mean_clan <- gini_by_var(robust_clans, "wealth_home_mean_clan") %>% 
  mutate(level = "clan", type = "mean")
gini_wealth_nohouse_mean_clan <- gini_by_var(robust_clans, "wealth_nohouse_mean_clan") %>% 
  mutate(level = "clan", type = "mean")

gini_clans <- bind_rows(
  gini_inc_all_median_clan,
  gini_inc_all_mean_clan,
  gini_wealth_home_clan,
  gini_wealth_nohouse_clan,
  gini_wealth_home_mean_clan,
  gini_wealth_nohouse_mean_clan
)

# Household-level Gini calculations using robust_households
gini_inc_all_hh <- gini_by_var(robust_households, "inc_all") %>% 
  mutate(level = "household", type = "raw")
gini_wealth_home_hh <- gini_by_var(robust_households, "wealth_home") %>% 
  mutate(level = "household", type = "raw")
gini_wealth_nohouse_hh <- gini_by_var(robust_households, "wealth_nohouse") %>% 
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
write.csv(gini_wide, here("7_analysis", "output", "gini_multiple_by_year.csv"), row.names = FALSE)
write.csv(gini_summary, here("7_analysis", "output", "gini_multiple_across.csv"), row.names = FALSE)

# Line plot over time 
gini_combined <- gini_combined %>%
  mutate(
    kind = case_when(
      grepl("^inc_", variable) ~ "Income",
      grepl("^wealth_", variable) ~ "Wealth",
      TRUE ~ "other"
    )
  )

pdf(here("7_analysis", "output", "gini_multiple_plot.pdf"), width = 8, height = 5) 

ggplot(gini_combined, aes(x = year, y = gini, color = level, linetype = level)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1) +
  facet_wrap(~ kind, scales = "fixed") +
  scale_y_continuous(limits = c(.3, 1)) +
  theme_minimal() +
  labs(
    title = "Gini Coefficient Over Time - Clans with Multiple Households",
    y = "Gini Coefficient",
    x = "year",
    color = "Level",
    linetype = "Level"
  )

dev.off()


# CALCULATE GINI BY RACE GROUPS ------------------------------------------------------------
clans <- clans %>%
  mutate(
    race_group = case_when(
      prop_race_black_ >= 0.5 ~ "Black",
      prop_race_black_ < 0.5 ~ "Non-Black"
    )
  )

households <- households %>%
  mutate(
    race_group = case_when(
      prop_race_black > 0.0 ~ "Black",
      prop_race_black == 0.0 ~ "Non-Black"
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
  ) %>%
  filter(!is.na(race_group))  

pdf(here("7_analysis", "output", "gini_race_plot.pdf"), width = 9, height = 5)

ggplot(gini_race_all, aes(x = year, y = gini, linetype = level)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1) +
  facet_grid(kind ~ race_group, scales = "fixed", space = "fixed") +
  scale_y_continuous(limits = c(.25, 1)) +
  theme_minimal(base_size = 12) +
  theme(panel.spacing = unit(1, "lines")) +
  labs(
    title = "Gini Coefficient Over Time: Black Households vs. Non-Black Households",
    y = "Gini Coefficient",
    x = "Year",
    color = "Measure & Race",
    linetype = "Level"
  )

dev.off()


# FIGURING OUT THE EXTREME INCOME IN 1994 AND 1995

# Filter to relevant years
clans_filtered <- clans %>% filter(year %in% 1990:1998)
households_filtered <- households %>% filter(year %in% 1990:1998)

# Summary function
summary_stats <- function(data, var) {
  data %>%
    group_by(year) %>%
    summarise(
      n = sum(!is.na(.data[[var]])),
      mean = mean(.data[[var]], na.rm = TRUE),
      median = median(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE),
      min = min(.data[[var]], na.rm = TRUE),
      max = max(.data[[var]], na.rm = TRUE),
      gini = ineq::Gini(.data[[var]][!is.na(.data[[var]])])
    ) %>%
    mutate(variable = var)
}

# Run summaries
clan_income_summary <- summary_stats(clans_filtered, "inc_all_median_clan")
hh_income_summary   <- summary_stats(households_filtered, "inc_all")
build_income_summary   <- summary_stats(build, "inc_all")

# Combine for inspection
income_summaries <- bind_rows(clan_income_summary, hh_income_summary)

# View
print(income_summaries)
