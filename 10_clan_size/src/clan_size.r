library(here)
library(dplyr)
library(ggplot2)


# LOAD DATA ------------------------------------------------------------------
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

# Average clan income/wealth (numclan = # of households in the clan)
r_clans <- r_clans %>%
  mutate(avg_inc = inc_all / numclan)

r_clans_wealth <- r_clans_wealth %>%
  mutate(avg_wealth_nohome = wealth_nohouse / numclan)

r_clans_wealth <- r_clans_wealth %>%
  mutate(avg_wealth = wealth / numclan)


options(scipen = 999)   # no scientific notation

# Function to plot clan size by income or wealth
size_by_metric <- function(df, y_var, year = "all", save_pdf = FALSE, pdf_file = "size.pdf") {
  
  if (!y_var %in% names(df)) {
    stop(paste("Variable", y_var, "not found in dataframe"))
  }
  
  if (length(year) == 1 && year == "all") {
    years_to_plot <- sort(unique(df$year))
  } else {
    years_to_plot <- year
  }
  
  df <- df %>% filter(year %in% years_to_plot)
  df_split <- split(df, df$year)
  
  if (save_pdf) {
    dir.create(dirname(pdf_file), recursive = TRUE, showWarnings = FALSE)
    pdf(pdf_file, width = 8, height = 6)
  }
  
  for (yr in years_to_plot) {
    d_yr <- df_split[[as.character(yr)]]
    if (is.null(d_yr)) next  # skip years not present, or warn instead
    
    p <- ggplot(d_yr,
                aes(x = numclan, y = .data[[y_var]])) +
      geom_point(alpha = 0.7) +
      geom_smooth(se = FALSE) +
      labs(
        title = paste("Clan Size vs", y_var, "— Year", yr),
        x = "Clan Size",
        y = y_var
      ) +
      theme_minimal(base_size = 14)
    
    print(p)
  }
  
  if (save_pdf) dev.off()
}

# Income
size_by_metric(
  df = r_clans,
  y_var = "avg_inc",
  year = c(1989, 1999, 2009, 2019),
  save_pdf = TRUE,
  pdf_file = "10_clan_size/output/clan_size_income.pdf"
)

# Wealth excluding home equity
size_by_metric(
  df = r_clans_wealth,
  y_var = "avg_wealth_nohome",
  year = c(1984, 1999, 2009, 2019),
  save_pdf = TRUE,
  pdf_file = "10_clan_size/output/clan_size_wealth_nohouse.pdf"
)

# Wealth with home equity
size_by_metric(
  df = r_clans_wealth,
  y_var = "avg_wealth",
  year = c(2009, 2019),
  save_pdf = TRUE,
  pdf_file = "10_clan_size/output/clan_size_wealth.pdf"
)

# Duration + average income
dur_inc <- r_clans %>%
  group_by(id1968) %>%
  summarise(
    n_years_inc = n_distinct(year),
    inc_avg = mean(avg_inc, na.rm = TRUE),
    .groups = "drop"
  )

p_inc <- ggplot(dur_inc, aes(x = n_years_inc, y = inc_avg)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Prevalence of Clans in PSID Data by Income",
    x = "Number of Years Clan is in Data",
    y = "Mean Average Income Across Observed Years"
  ) +
  theme_minimal(base_size = 14)

pdf("10_clan_size/output/in_data_income.pdf", width = 8, height = 6)
print(p_inc)
dev.off()

# Duration + average wealth
dur_wealth <- r_clans_wealth %>%
  group_by(id1968) %>%
  summarise(
    n_years_wealth = n_distinct(year),
    wealth_nohouse_avg = mean(avg_wealth_nohome, na.rm = TRUE),
    .groups = "drop"
  )

p_wealth <- ggplot(dur_wealth, aes(x = n_years_wealth, y = wealth_nohouse_avg)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Prevalence of Clans in PSID Data by Wealth",
    x = "Number of Years Clan is in the Data",
    y = "Mean Average Wealth Across Observed Years"
  ) +
  theme_minimal(base_size = 14)

pdf("10_clan_size/output/in_data_wealth.pdf", width = 8, height = 6)
print(p_wealth)
dev.off()


# RERUN ON FULL SAMPLE
# I originally reran all of the code above on the full sample with single-HH clans
# There were no differences in any of the graphs besides the one below
clans <- readRDS(here("4_clans", "output", "clans.rds"))

clans <- clans %>%
  mutate(avg_inc = inc_all / numclan)

dur_inc_all <- clans %>%
  group_by(id1968) %>%
  summarise(
    n_years_inc = n_distinct(year),
    inc_avg = mean(avg_inc, na.rm = TRUE),
    .groups = "drop"
  )

p_inc_all <- ggplot(dur_inc_all, aes(x = n_years_inc, y = inc_avg)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Prevalence of Clans in PSID Data by Income (All)",
    x = "Number of Years Clan is in Data",
    y = "Mean Average Income Across Observed Years"
  ) +
  theme_minimal(base_size = 14)

pdf("10_clan_size/output/in_data_income_all.pdf", width = 8, height = 6)
print(p_inc_all)
dev.off()