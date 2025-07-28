# INPUTS: 6_clean_clans/output/clean_clans.rds, 4_clean_households/output/clean_hs.rds
# OUTPUTS: 7_analysis/output

# This file calculates the Gini coefficient for income and wealth for two different samples:
  # SAMPLE 1: ALL CLANS AND HOUSEHOLDS (using clans and households)
  # SAMPLE 2: MULTIPLE - CLANS AND HOUSEHOLDS WITH MORE THAN ONE HOUSEHOLD (using robust_clans and robust_households)

# Then, each sample is broken down by race:
  # SAMPLE 1: RACE - ALL CLANS AND HOUSEHOLDS (either Black or Non-Black)
  # SAMPLE 2: RACE - CLANS AND HOUSEHOLDS WITH MORE THAN ONE HOUSEHOLD BY RACE (either Black or Non-Black)

# Each section:
  # Calculates the Gini coefficient for income and wealth at the clan and household level
  # Summarizes the Gini across time (by_year.csv, plot.pdf)
  # Plots the Gini over time (across.csv)
  # Plots the Lorenz curves (lorenz.pdf)

# Finally, there are XXX descriptive plots:
  # 1. Describing observations within each sample (desc_sample.pdf)
  # 2. Describing income and wealth measures for each sample (desc_inc_wealth.pdf)
  # 3. Calculating the ICC for income and wealth measures for each sample (desc_icc.pdf)
  # 4. Plotting income versus wealth for households and clans (desc_inc_v_wealth.pdf)
  # 5. Plotting within-clan variation for income and wealth (desc_variation.pdf)
  # 6. Plotting within-clan variation for income and wealth by clan size (desc_variation_by_size.pdf)

clans <- readRDS(here("6_clean_clans", "output", "clean_clans.rds"))
households <- readRDS(here("4_clean_households", "output", "clean_hs.rds"))

# DEFINE INPUTS ----------------------------------------------------------------------------------------
# Fonts
font_add_google(name = "Tinos", family = "Times New Roman")
showtext_auto()

# Colors and linetypes for plots
gini_colors <- c(
  "household" = "#ff7f0e",
  "clan"      = "#ffbb78"
)

gini_linetypes <- c(
  "household" = "dashed",
  "clan"      = "solid"
)

lorenz_colors <- c(
  "Household" = "#ff7f0e",
  "Clan"      = "#ffbb78"
)

lorenz_linetypes <- c(
  "Household" = "dashed",
  "Clan"      = "solid"
)

# Income and wealth variables
clan_vars <- c("inc_all_median_clan", "wealth_nohouse_median_clan", "wealth_home_median_clan")
hh_vars   <- c("inc_all", "wealth_nohouse", "wealth_home")

# DEFINE FUNCTIONS ----------------------------------------------------------------------------------------
# Function: Calculate Gini coefficient 
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

# Function: Calculate Gini coefficient by group
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

# Function: Calculate Lorenz curve data by group
lorenz_data_by_race_group <- function(df, value_var, group_var, level_label, race_var, year_filter) {
  df %>%
    filter(year == year_filter, is.finite(.data[[value_var]]), !is.na(.data[[race_var]])) %>%
    group_split(!!sym(race_var)) %>%
    purrr::map_dfr(~ {
      race_label <- unique(.x[[race_var]])
      values <- .x[[value_var]]
      lorenz <- ineq::Lc(values)
      tibble(
        p = lorenz$p,
        L = lorenz$L,
        level = level_label,
        race_group = race_label
      )
    })
}



# CALCULATE GINI: ALL HOUSEHOLDS ------------------------------------------------------------
# Clan Gini calculations
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

# Household Gini calculations
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
write.csv(gini_wide, here("7_analysis", "output", "all_by_year.csv"), row.names = FALSE)
write.csv(gini_summary, here("7_analysis", "output", "all_across.csv"), row.names = FALSE)


# Plot over time 
gini_combined <- gini_combined %>%
  mutate(
    kind = case_when(
      grepl("^inc_", variable) ~ "Income",
      grepl("^wealth_", variable) ~ "Wealth",
      TRUE ~ "other"
    )
  )

pdf(here("7_analysis", "output", "all_plot.pdf"), width = 8, height = 5)

ggplot(gini_combined, aes(x = year, y = gini, color = level, linetype = level)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, linewidth = 0.6) +
  facet_wrap(~ kind, scales = "fixed") +
  scale_y_continuous(limits = c(0, 1)) +
scale_color_manual(values = gini_colors) +
scale_linetype_manual(values = gini_linetypes) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Supplementary Figure 1: Comparing Inequality Over Time (Full Sample)",
    y = "Gini Coefficient",
    x = "Year",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE."
  )

dev.off()



# Calculations for Lorenz curves
latest_year <- max(households$year, na.rm = TRUE)

L_hh_income <- Lc(households %>%
                    filter(year == latest_year, is.finite(inc_all)) %>%
                    pull(inc_all))

L_clan_income <- Lc(clans %>%
                      filter(year == latest_year, is.finite(inc_all_median_clan)) %>%
                      pull(inc_all_median_clan))

L_hh_wealth <- Lc(households %>%
                    filter(year == latest_year, is.finite(wealth_nohouse)) %>%
                    pull(wealth_nohouse))

L_clan_wealth <- Lc(clans %>%
                      filter(year == latest_year, is.finite(wealth_nohouse_median_clan)) %>%
                      pull(wealth_nohouse_median_clan))

lorenz_income <- bind_rows(
  data.frame(p = L_hh_income$p, L = L_hh_income$L, group = "Household"),
  data.frame(p = L_clan_income$p, L = L_clan_income$L, group = "Clan")
) %>%
  mutate(group_label = factor(group, levels = c("Household", "Clan")))

lorenz_wealth <- bind_rows(
  data.frame(p = L_hh_wealth$p, L = L_hh_wealth$L, group = "Household"),
  data.frame(p = L_clan_wealth$p, L = L_clan_wealth$L, group = "Clan")
) %>%
  mutate(group_label = factor(group, levels = c("Household", "Clan")))

# Lorenz income plot
p_income <- ggplot(lorenz_income, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Income",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Income",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"), legend.position = "bottom")

# Lorenz wealth plot
p_wealth <- ggplot(lorenz_wealth, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Wealth",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Wealth",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"), legend.position = "bottom")

# Combine and export
p_combined <- p_income + p_wealth + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = here("7_analysis", "output", "all_lorenz.pdf"),
  plot = p_combined,
  width = 12,
  height = 5
)



# CALCULATE GINI: MULTIPLE - CLANS WITH MORE THAN ONE HOUSEHOLD ------------------------------------------------
# Filter clans with only one household
robust_clans <- clans %>%
  filter(numclan > 1)

single_households <- clans %>%
  filter(numclan == 1) %>%
  rename(fam_id = fam_id_1) 

robust_households <- anti_join(households, single_households, by = c("year", "fam_id"))

# Clan Gini calculations
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

# Household Gini calculations
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
write.csv(gini_wide, here("7_analysis", "output", "multiple_by_year.csv"), row.names = FALSE)
write.csv(gini_summary, here("7_analysis", "output", "multiple_across.csv"), row.names = FALSE)

# Plot over time 
gini_combined <- gini_combined %>%
  mutate(
    kind = case_when(
      grepl("^inc_", variable) ~ "Income",
      grepl("^wealth_", variable) ~ "Wealth",
      TRUE ~ "other"
    )
  )

pdf(here("7_analysis", "output", "multiple_plot.pdf"), width = 8, height = 5)

ggplot(gini_combined, aes(x = year, y = gini, color = level, linetype = level)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, linewidth = 0.6) +
  facet_wrap(~ kind, scales = "fixed") +
  scale_y_continuous(limits = c(0, 1)) +
scale_color_manual(values = gini_colors) +
scale_linetype_manual(values = gini_linetypes) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Figure 1: Comparing Inequality Over Time",
    y = "Gini Coefficient",
    x = "Year",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE."
  )

dev.off()

# Calculations for Lorenz curves
latest_year <- max(robust_households$year, na.rm = TRUE)

L_hh_income <- Lc(robust_households %>%
                    filter(year == latest_year, is.finite(inc_all)) %>%
                    pull(inc_all))

L_clan_income <- Lc(robust_clans %>%
                      filter(year == latest_year, is.finite(inc_all_median_clan)) %>%
                      pull(inc_all_median_clan))

L_hh_wealth <- Lc(robust_households %>%
                    filter(year == latest_year, is.finite(wealth_nohouse)) %>%
                    pull(wealth_nohouse))

L_clan_wealth <- Lc(robust_clans %>%
                      filter(year == latest_year, is.finite(wealth_nohouse_median_clan)) %>%
                      pull(wealth_nohouse_median_clan))

lorenz_income <- bind_rows(
  data.frame(p = L_hh_income$p, L = L_hh_income$L, group = "Household"),
  data.frame(p = L_clan_income$p, L = L_clan_income$L, group = "Clan")
) %>%
  mutate(group_label = factor(group, levels = c("Household", "Clan")))

lorenz_wealth <- bind_rows(
  data.frame(p = L_hh_wealth$p, L = L_hh_wealth$L, group = "Household"),
  data.frame(p = L_clan_wealth$p, L = L_clan_wealth$L, group = "Clan")
) %>%
  mutate(group_label = factor(group, levels = c("Household", "Clan")))

# Lorenz income plot
p_income <- ggplot(lorenz_income, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Income",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Income",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"), legend.position = "bottom")

# Lorenz wealth plot
p_wealth <- ggplot(lorenz_wealth, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Wealth",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Wealth",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"), legend.position = "bottom")

# Combine and export
p_combined <- p_income + p_wealth + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = here("7_analysis", "output", "multiple_lorenz.pdf"),
  plot = p_combined,
  width = 12,
  height = 5
)

# CALCULATE GINI: RACE - ALL HOUSEHOLDS AND CLANS ------------------------------------------------
# Define Black and Non-Black groups
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

lorenz_colors <- c(
  "Household - Black"     = "#1f77b4",
  "Clan - Black"          = "#aec7e8",
  "Household - Non-Black" = "#ff7f0e",
  "Clan - Non-Black"      = "#ffbb78"
)

lorenz_linetypes <- c(
  "Household - Black"     = "dashed",
  "Clan - Black"          = "solid",
  "Household - Non-Black" = "dashed",
  "Clan - Non-Black"      = "solid"
)

# Clan Gini calculations
gini_race_clan <- bind_rows(
  gini_by_race_group(clans, "inc_all_median_clan", "clan"),
  gini_by_race_group(clans, "wealth_nohouse_median_clan", "clan"),
  gini_by_race_group(clans, "wealth_home_median_clan", "clan")
)

# Household Gini calculations
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

gini_race_all <- gini_race_all %>%
  mutate(
    group_label = paste(str_to_title(level), "-", race_group),
    group_label = factor(group_label, levels = c(
      "Household - Black", "Clan - Black",
      "Household - Non-Black", "Clan - Non-Black"
    ))
  )

gini_race_by_year <- gini_race_all %>%
  select(year, variable, level, race_group, gini) %>%
  arrange(year, variable, level, race_group)

# Summarize across years
gini_race_summary <- gini_race_all %>%
  group_by(variable, level, race_group) %>%
  summarise(mean_gini = mean(gini, na.rm = TRUE), .groups = "drop")

gini_race_across <- gini_race_summary %>%
  mutate(label = paste0(variable, "_", level, "_", race_group)) %>%
  select(label, mean_gini) %>%
  pivot_wider(names_from = label, values_from = mean_gini)

# Save to CSV
write.csv(gini_race_by_year, here("7_analysis", "output", "all_race_by_year.csv"), row.names = FALSE)
write.csv(gini_race_across, here("7_analysis", "output", "all_race_across.csv"), row.names = FALSE)


pdf(here("7_analysis", "output", "all_race_plot.pdf"), width = 9, height = 5)

# Plot over time
ggplot(gini_race_all, aes(x = year, y = gini, color = group_label, linetype = group_label)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, linewidth = 0.6) +
  facet_grid(race_group ~ kind, scales = "fixed", space = "fixed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Figure 2: Inequality in Black Households vs. Non-Black Households",
    y = "Gini Coefficient",
    x = "Year",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE."
  )

dev.off()


# Calculations for Lorenz curves
lorenz_income_hh <- lorenz_data_by_race_group(households, "inc_all", "level", "Household", "race_group", year_latest)
lorenz_income_clan <- lorenz_data_by_race_group(clans, "inc_all_median_clan", "level", "Clan", "race_group", year_latest)

lorenz_wealth_hh <- lorenz_data_by_race_group(households, "wealth_nohouse", "level", "Household", "race_group", year_latest)
lorenz_wealth_clan <- lorenz_data_by_race_group(clans, "wealth_nohouse_median_clan", "level", "Clan", "race_group", year_latest)

# Configure legend labels and order
desired_order <- c(
  "Household - Black",
  "Clan - Black",
  "Household - Non-Black",
  "Clan - Non-Black"
)

lorenz_income_all <- bind_rows(lorenz_income_hh, lorenz_income_clan) %>%
  mutate(
    group_label = paste(level, "-", race_group),
    group_label = factor(group_label, levels = desired_order)
  )

lorenz_wealth_all <- bind_rows(lorenz_wealth_hh, lorenz_wealth_clan) %>%
  mutate(
    group_label = paste(level, "-", race_group),
    group_label = factor(group_label, levels = desired_order)
  )

# Lorenz income
p_income <- ggplot(lorenz_income_all, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Income",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Income",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  )


# Lorenz wealth
p_wealth <- ggplot(lorenz_wealth_all, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Wealth",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Wealth",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  )

p_combined <- p_income + p_wealth + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = here("7_analysis", "output", "all_race_lorenz.pdf"),
  plot = p_combined,
  width = 12,
  height = 5
)



# CALCULATE GINI: RACE - CLANS WITH MORE THAN ONE HOUSEHOLD BY RACIAL GROUP ------------------------------------------------
# Define Black and Non-Black groups
robust_clans <- robust_clans %>%
  mutate(
    race_group = case_when(
      prop_race_black_ >= 0.5 ~ "Black",
      prop_race_black_ < 0.5 ~ "Non-Black"
    )
  )

robust_households <- robust_households %>%
  mutate(
    race_group = case_when(
      prop_race_black > 0.0 ~ "Black",
      prop_race_black == 0.0 ~ "Non-Black"
    )
  )

lorenz_colors <- c(
  "Household - Black"     = "#1f77b4",
  "Clan - Black"          = "#aec7e8",
  "Household - Non-Black" = "#ff7f0e",
  "Clan - Non-Black"      = "#ffbb78"
)

lorenz_linetypes <- c(
  "Household - Black"     = "dashed",
  "Clan - Black"          = "solid",
  "Household - Non-Black" = "dashed",
  "Clan - Non-Black"      = "solid"
)

# Clan Gini calculations
gini_race_clan <- bind_rows(
  gini_by_race_group(robust_clans, "inc_all_median_clan", "clan"),
  gini_by_race_group(robust_clans, "wealth_nohouse_median_clan", "clan"),
  gini_by_race_group(robust_clans, "wealth_home_median_clan", "clan")
)

# Household Gini calculations
gini_race_hh <- bind_rows(
  gini_by_race_group(robust_households, "inc_all", "household"),
  gini_by_race_group(robust_households, "wealth_nohouse", "household"),
  gini_by_race_group(robust_households, "wealth_home", "household")
)

gini_race_all <- bind_rows(gini_race_clan, gini_race_hh) %>%
  mutate(
    kind = case_when(
      grepl("^inc_", variable) ~ "Income",
      grepl("^wealth_", variable) ~ "Wealth"
    )
  ) %>%
  filter(!is.na(race_group))  

gini_race_all <- gini_race_all %>%
  mutate(
    group_label = paste(str_to_title(level), "-", race_group),
    group_label = factor(group_label, levels = c(
      "Household - Black", "Clan - Black",
      "Household - Non-Black", "Clan - Non-Black"
    ))
  )

gini_race_by_year <- gini_race_all %>%
  select(year, variable, level, race_group, gini) %>%
  arrange(year, variable, level, race_group)

# Summarize across years
gini_race_summary <- gini_race_all %>%
  group_by(variable, level, race_group) %>%
  summarise(mean_gini = mean(gini, na.rm = TRUE), .groups = "drop")

gini_race_across <- gini_race_summary %>%
  mutate(label = paste0(variable, "_", level, "_", race_group)) %>%
  select(label, mean_gini) %>%
  pivot_wider(names_from = label, values_from = mean_gini)

# Save to CSV
write.csv(gini_race_by_year, here("7_analysis", "output", "multiple_race_by_year.csv"), row.names = FALSE)
write.csv(gini_race_across, here("7_analysis", "output", "multiple_race_across.csv"), row.names = FALSE)


pdf(here("7_analysis", "output", "multiple_race_plot.pdf"), width = 9, height = 5)

# Plot over time
ggplot(gini_race_all, aes(x = year, y = gini, color = group_label, linetype = group_label)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, linewidth = 0.6) +
  facet_grid(race_group ~ kind, scales = "fixed", space = "fixed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom"
  ) +
  labs(
    title = "Figure 2: Inequality in Black Households vs. Non-Black Households",
    y = "Gini Coefficient",
    x = "Year",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE."
  )

dev.off()


# Calculations for Lorenz curves
lorenz_income_hh <- lorenz_data_by_race_group(robust_households, "inc_all", "level", "Household", "race_group", year_latest)
lorenz_income_clan <- lorenz_data_by_race_group(robust_clans, "inc_all_median_clan", "level", "Clan", "race_group", year_latest)

lorenz_wealth_hh <- lorenz_data_by_race_group(robust_households, "wealth_nohouse", "level", "Household", "race_group", year_latest)
lorenz_wealth_clan <- lorenz_data_by_race_group(robust_clans, "wealth_nohouse_median_clan", "level", "Clan", "race_group", year_latest)

# Configure legend labels and order
desired_order <- c(
  "Household - Black",
  "Clan - Black",
  "Household - Non-Black",
  "Clan - Non-Black"
)

lorenz_income_all <- bind_rows(lorenz_income_hh, lorenz_income_clan) %>%
  mutate(
    group_label = paste(level, "-", race_group),
    group_label = factor(group_label, levels = desired_order)
  )

lorenz_wealth_all <- bind_rows(lorenz_wealth_hh, lorenz_wealth_clan) %>%
  mutate(
    group_label = paste(level, "-", race_group),
    group_label = factor(group_label, levels = desired_order)
  )

# Lorenz income
p_income <- ggplot(lorenz_income_all, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Income",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Income",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  )


# Lorenz wealth
p_wealth <- ggplot(lorenz_wealth_all, aes(x = p, y = L, color = group_label, linetype = group_label)) +
  geom_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = lorenz_colors) +
  scale_linetype_manual(values = lorenz_linetypes) +
  labs(
    title = "Lorenz Curve: Wealth",
    x = "Cumulative Share of Units",
    y = "Cumulative Share of Wealth",
    color = "",
    linetype = "",
    caption = "Notes: PUT NOTES HERE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  )

p_combined <- p_income + p_wealth + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = here("7_analysis", "output", "multiple_race_lorenz.pdf"),
  plot = p_combined,
  width = 12,
  height = 5
)


# DESC: NUMBER OF OBSERVATION YEARS FOR BOTH SAMPLES ------------------------------------------------
summarize_dataset <- function(df, name) {
  total_obs <- nrow(df)
  unique_clans <- n_distinct(df$id1968)

  # Count number of unique clans and unique Black clans
  unique_black_clans <- df %>%
    distinct(id1968, .keep_all = TRUE) %>%
    filter(race_group == "Black") %>%
    nrow()

  race_counts <- df %>%
    filter(!is.na(race_group)) %>%
    count(race_group) %>%
    mutate(percent = n / sum(n)) %>%
    pivot_wider(
      names_from = race_group,
      values_from = c(n, percent),
      names_glue = "{race_group}_{.value}"
    )


  tibble(
    Dataset = name,
    N = total_obs,
    `Unique Clans` = unique_clans,
    `Black Clans` = unique_black_clans
  ) %>%
    bind_cols(race_counts)
}

summary_table <- bind_rows(
  summarize_dataset(robust_households, "Households"),
  summarize_dataset(robust_clans, "Clans"),
    summarize_dataset(households, "All Households"),
  summarize_dataset(clans, "All Clans")
)

# Format 
summary_table_formatted <- summary_table %>%
  mutate(
    across(c(N, `Unique Clans`, `Black Clans`), ~ comma(.x, accuracy = 1)),
    `Black N (%)` = paste0(comma(Black_n, accuracy = 1), " (", percent(Black_percent, accuracy = 0.1), ")"),
    `Non-Black N (%)` = paste0(comma(`Non-Black_n`, accuracy = 1), " (", percent(`Non-Black_percent`, accuracy = 0.1), ")"),
    
    # Format Black Clans (%) only for clan datasets
    `Black Clans (%)` = case_when(
      grepl("Clans", Dataset) ~ paste0(`Black Clans`, " (", percent(as.numeric(gsub(",", "", `Black Clans`)) / as.numeric(gsub(",", "", `Unique Clans`)), accuracy = 0.1), ")"),
      TRUE ~ "-"
    )
  ) %>%
  select(Dataset, N, `Black N (%)`, `Non-Black N (%)`, `Unique Clans`, `Black Clans (%)`)


# Export 
save_kable(
  summary_table_formatted %>%
    kable(format = "html", booktabs = TRUE, align = "lrrrrr", escape = FALSE) %>%
    kable_styling(full_width = FALSE, position = "center",
                  bootstrap_options = c("striped", "hover", "condensed")) %>%
    add_header_above(c("Sample Summary" = 6), bold = TRUE) %>%
    footnote(
  general = paste(
    "Notes:",
    "• N = total number of household- or clan-years.",
    "• Black households: ≥1 Black-identifying member.",
    "• Black clans: ≥50% of members identify as Black.",
    "• Unique households cannot be tracked over time in the PSID. See Materials and Methods for details.",
    "• 'All' datasets include households within a clan with only one household.",
    sep = "\n"
  ),
  general_title = "",
  footnote_as_chunk = TRUE
),
  file = here("7_analysis", "output", "desc_sample.pdf")
)



# DESC: SUMMARY STATISTICS FOR INCOME AND WEALTH -------------------------------------------------------------
summarize_income <- function(data, var, label) {
  data %>%
    summarize(
      Measure = label,
      Mean = mean({{ var }}, na.rm = TRUE),
      Median = median({{ var }}, na.rm = TRUE),
      SD = sd({{ var }}, na.rm = TRUE)
    )
}

# Robust sample (Multiple households per clan)
robust_summary <- bind_rows(
  summarize_income(robust_households, inc_all, "Household Income"),
  summarize_income(robust_clans, inc_all_median_clan, "Median Clan Income"),
  summarize_income(robust_clans, inc_all_mean_clan, "Mean Clan Income"),
  summarize_income(robust_households, wealth_nohouse, "Household Wealth"),
  summarize_income(robust_clans, wealth_nohouse_median_clan, "Median Clan Wealth"),
  summarize_income(robust_clans, wealth_nohouse_mean_clan, "Mean Clan Wealth")
)

# All sample (Including households in a single-household clan)
all_summary <- bind_rows(
  summarize_income(households, inc_all, "Household Income"),
  summarize_income(clans, inc_all_median_clan, "Median Clan Income"),
  summarize_income(clans, inc_all_mean_clan, "Mean Clan Income"),
  summarize_income(households, wealth_nohouse, "Household Wealth"),
  summarize_income(clans, wealth_nohouse_median_clan, "Median Clan Wealth"),
  summarize_income(clans, wealth_nohouse_mean_clan, "Mean Clan Wealth")
)
# Combine 
summary_table_combined <- robust_summary %>%
  left_join(all_summary, by = "Measure", suffix = c("_Robust", "_All")) %>%
  mutate(across(where(is.numeric), ~ scales::comma(.x, accuracy = 1))) %>%
  select(
    Measure,
    Mean_Robust, Median_Robust, SD_Robust,
    Mean_All, Median_All, SD_All
  ) %>%
  setNames(c("Measure", "Mean", "Median", "SD", "Mean", "Median", "SD"))

# Export 
save_kable(
  summary_table_combined %>%
    kable(format = "html", booktabs = TRUE, align = "lrrrrrr", escape = FALSE) %>%
    kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed")) %>%
    add_header_above(c(" " = 1, "Robust Sample" = 3, "Full Sample" = 3), bold = TRUE) %>%
    add_header_above(c("Summary Statistics for Income and Wealth" = 7), bold = TRUE) %>%
    footnote(
      general = "Note: Robust sample excludes households within single-household clans.",
      general_title = "",
      footnote_as_chunk = TRUE
    ),
  file = here("7_analysis", "output", "desc_inc_wealth.pdf")
)


# DESC: INTRACLASS CORRELATION (ICC) FOR HOUSEHOLDS WITHIN A CLAN -----------------------------------------------
icc_income_robust <- as.numeric(icc(lmer(inc_all ~ 1 + (1 | id1968), data = robust_households))[1])
icc_wealth_robust <- as.numeric(icc(lmer(wealth_home ~ 1 + (1 | id1968), data = robust_households))[1])
icc_income_all    <- as.numeric(icc(lmer(inc_all ~ 1 + (1 | id1968), data = households))[1])
icc_wealth_all    <- as.numeric(icc(lmer(wealth_home ~ 1 + (1 | id1968), data = households))[1])


icc_table <- tibble(
  Sample = c("Robust Sample", "All Households"),
  Income = c(icc_income_robust, icc_income_all),
  Wealth = c(icc_wealth_robust, icc_wealth_all)
)

save_kable(
  icc_table %>%
    mutate(across(Income:Wealth, ~ scales::percent(.x, accuracy = 0.1))) %>%
    kable(format = "html", booktabs = TRUE, align = "lrr", escape = FALSE) %>%
    kable_styling(full_width = FALSE, position = "center",
                  bootstrap_options = c("striped", "hover", "condensed")) %>%
    add_header_above(c("ICC: Variation in Household Outcomes Explained by Clan Membership" = 3), bold = TRUE) %>%
    footnote(
      general = "Note: ADD NOTES HERE.",
      general_title = "",
      footnote_as_chunk = TRUE
    ),
  file = here("7_analysis", "output", "desc_icc.pdf")
)



# DESC: PLOT INCOME VERSUS WEALTH FOR HOUSEHOLDS AND CLANS -----------------------------------------------
# Formatting
x_min <- min(c(robust_clans$inc_all_median_clan, robust_households$inc_all), na.rm = TRUE)
x_max <- max(c(robust_clans$inc_all_median_clan, robust_households$inc_all), na.rm = TRUE)
y_min <- min(c(robust_clans$wealth_nohouse_median_clan, robust_households$wealth_nohouse), na.rm = TRUE)
y_max <- max(c(robust_clans$wealth_nohouse_median_clan, robust_households$wealth_nohouse), na.rm = TRUE)

group_colors <- c("Household" = "#ff7f0e", "Clan" = "#ffbb78")

# Clans
p_clan <- ggplot(robust_clans, aes(x = inc_all_median_clan, y = wealth_nohouse_median_clan)) +
  geom_point(color = group_colors["Clan"], alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = group_colors["Clan"], linetype = "solid") +
  scale_x_continuous(labels = label_comma(), limits = c(x_min, x_max)) +
  scale_y_continuous(labels = label_comma(), limits = c(y_min, y_max)) +
  labs(title = "Clans (Median Values)",
       x = "Income",
       y = "Wealth",
       caption = "Notes: PUT NOTES HERE.") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman")
  )

# Households
p_household <- ggplot(robust_households, aes(x = inc_all, y = wealth_nohouse)) +
  geom_point(color = group_colors["Household"], alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = group_colors["Household"], linetype = "dashed") +
  scale_x_continuous(labels = label_comma(), limits = c(x_min, x_max)) +
  scale_y_continuous(labels = label_comma(), limits = c(y_min, y_max)) +
  labs(title = "Households",
       x = "Income",
       y = "Wealth",
       caption = "Notes: PUT NOTES HERE.") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman")
  )

combined_plot <- p_clan + p_household + plot_layout(ncol = 2)

# Export
ggsave(
  filename = here("7_analysis", "output", "desc_inc_v_wealth.pdf"),
  plot = combined_plot,
  width = 12,
  height = 5
)


# DESC: WITHIN-CLAN INCOME AND WEALTH VARIATION -----------------------------------------------
# Format
clan_color <- "#ffbb78"

p_variation <- ggplot(robust_clans, aes(x = inc_all_sd_clan, y = wealth_nohouse_sd_clan)) +
  geom_point(color = clan_color, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = clan_color, linetype = "solid", linewidth = 0.7) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Income and Wealth Variation Within Clans",
    x = "Income SD Within Clans",
    y = "Wealth SD Within Clans",
    ,
       caption = "Notes: PUT NOTES HERE.") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5)  # center title if desired
  )

# Export
ggsave(
  filename = here("7_analysis", "output", "desc_variation.pdf"),
  plot = p_variation,
  width = 8,
  height = 5
)

# DESC: WITHIN-CLAN INCOME AND WEALTH VARIATION BY CLAN SIZE -----------------------------------------------
# Format
y_min <- min(c(robust_clans$inc_all_sd_clan, robust_clans$wealth_nohouse_sd_clan), na.rm = TRUE)
y_max <- max(c(robust_clans$inc_all_sd_clan, robust_clans$wealth_nohouse_sd_clan), na.rm = TRUE)
even_breaks <- as.character(seq(2, max(robust_clans$numclan, na.rm = TRUE), by = 2))

# Income 
p_income <- ggplot(robust_clans, aes(x = as.factor(numclan), y = inc_all_sd_clan)) +
  geom_violin(fill = "#ffbb78", alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.size = 0.5, fill = "#ff7f0e", alpha = 0.4) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(y_min, y_max)) +
  scale_x_discrete(breaks = even_breaks) +
  labs(
    title = "Within-Clan Variation by Clan Size",
    x = "Number of Households within Clan",
    y = "Income SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(hjust = 1, size = 10),
    legend.position = "none"
  )

# Wealth 
p_wealth <- ggplot(robust_clans, aes(x = as.factor(numclan), y = wealth_nohouse_sd_clan)) +
  geom_violin(fill = "#aec7e8", alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.size = 0.5, fill = "#1f77b4", alpha = 0.4) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(y_min, y_max)) +
  scale_x_discrete(breaks = even_breaks) +
  labs(
    title = " ",
    x = "Number of Households within Clan",
    y = "Wealth SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(hjust = 1, size = 10),
    legend.position = "none"
  )

p_combined <- p_income + p_wealth + plot_layout(ncol = 2)

# Export
ggsave(
  filename = here::here("7_analysis", "output", "desc_variation_by_size.pdf"),
  plot = p_combined,
  width = 12,
  height = 5
)


