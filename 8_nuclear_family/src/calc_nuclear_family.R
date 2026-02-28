library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(flextable)
library(tibble)
library(rlang)
library(flextable)
library(officer)
library(survey)
library(convey)

# Load data
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

# SIZE STANDARDIZATION ---------------------------------------------------------------
# Households: divide by number of people in household (numfu)
# Clans: divide by number of households in clan (numclan)

r_hh          <- r_hh          %>% mutate(inc_all        = inc_all        / numfu)
r_hh_wealth   <- r_hh_wealth   %>% mutate(wealth_nohouse = wealth_nohouse / numfu,
                                            wealth         = wealth         / numfu)
r_clans        <- r_clans       %>% mutate(inc_all        = inc_all        / numclan)
r_clans_wealth <- r_clans_wealth %>% mutate(wealth_nohouse = wealth_nohouse / numclan,
                                              wealth         = wealth         / numclan)

# Output paths
out_dir  <- here("8_nuclear_family", "output")
dir.create(out_dir,  recursive = TRUE, showWarnings = FALSE)

# Adds an "ALL" row = mean across years for each numeric column.
join_hh_clan <- function(hh_df, clan_df, var_label) {
  out <- hh_df %>%
    rename_with(~ paste0(.x, "_hh"), starts_with("C")) %>%
    full_join(
      clan_df %>% rename_with(~ paste0(.x, "_clan"), starts_with("C")),
      by = "year"
    ) %>%
    arrange(year) %>%
    mutate(year = as.character(year))

  all_row <- out %>%
    filter(year != "ALL") %>%
    summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 3))) %>%
    mutate(year = "ALL", .before = 1)

  out <- bind_rows(out, all_row) %>%
    mutate(across(where(is.numeric), ~ round(as.numeric(.x), 3)))

  write_csv(out, file.path(out_dir, paste0(var_label, "_C123.csv")))
  out
}

# Compute C123 by year
# Income
hh_inc   <- C123_by_year(r_hh, value_var = inc_all,  weight = TRUE, weight_var = "fam_weight")
clan_inc <- C123_by_year(r_clans, value_var = inc_all, weight = TRUE, weight_var = "clan_weight")
inc      <- join_hh_clan(hh_inc, clan_inc, "income")

# Wealth (no home equity)
hh_wnh   <- C123_by_year(r_hh_wealth, value_var = wealth_nohouse, weight = TRUE, weight_var = "fam_weight")
clan_wnh <- C123_by_year(r_clans_wealth, value_var = wealth_nohouse, weight = TRUE, weight_var = "clan_weight")
wealth_nohouse <- join_hh_clan(hh_wnh, clan_wnh, "wealth_nohouse")

# Wealth (incl home equity)
hh_w   <- C123_by_year(r_hh_wealth, value_var = wealth, weight = TRUE, weight_var = "fam_weight")
clan_w <- C123_by_year(r_clans_wealth, value_var = wealth, weight = TRUE, weight_var = "clan_weight")
wealth <- join_hh_clan(hh_w, clan_w, "wealth")