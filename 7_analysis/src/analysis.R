# arg <- commandArgs(trailingOnly = TRUE)
# path <- str_remove(arg[2], "/Makefile")

# if (length(arg) == 0) {
#   path <- file.path(getwd(), "code")
# }

# source(file.path(path, "functions", "src", "functions.R"))

# load(file.path(path, "clean_merged", "input", "merged.Rds"))

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
gini_inc_all_median_clan <- gini_by_var(clans, "inc_all_median_clan") %>% mutate(level = "clan", type = "median")
gini_inc_all_mean_clan <- gini_by_var(clans, "inc_all_mean_clan") %>% mutate(level = "clan", type = "mean")
gini_wealth_home_clan <- gini_by_var(clans, "wealth_home_median_clan") %>% mutate(level = "clan", type = "median")
gini_wealth_nohouse_clan <- gini_by_var(clans, "wealth_nohouse_median_clan") %>% mutate(level = "clan", type = "median")

gini_clans <- bind_rows(
  gini_inc_all_median_clan,
  gini_inc_all_mean_clan,
  gini_wealth_home_clan,
  gini_wealth_nohouse_clan
)

# Calculate Gini for household-level 
gini_inc_all_hh <- gini_by_var(households, "inc_all") %>% mutate(level = "household", type = "individual")
gini_wealth_home_hh <- gini_by_var(households, "wealth_home") %>% mutate(level = "household", type = "median")
gini_wealth_nohouse_hh <- gini_by_var(households, "wealth_nohouse") %>% mutate(level = "household", type = "median")

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


# Create summary table
single_family_clans <- clans %>%
  group_by(year) %>%
  summarise(
    total_clans = n_distinct(id1968),
    single_family_clans = n_distinct(id1968[numclan == 1]),
    percent_single_family = round(100 * single_family_clans / total_clans, 2)
  )

# Turn the table into a grob (graphical object)
table_grob <- tableGrob(single_family_clans)

# Save as PDF
ggsave(
  filename = here("7_analysis", "output", "single_family_clans_summary.pdf"),
  plot = table_grob,
  width = 8,
  height = 20
)
