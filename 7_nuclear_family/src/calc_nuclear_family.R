# LOAD DATA ------------------------------------------------------------------
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

# Function to join HH and Clan results for C1, C2, C3
join_hh_clan <- function(hh_df, clan_df, var_label) {
  hh_df %>%
    rename_with(~ paste0(.x, "_HH"), c("C1", "C2", "C3")) %>%
    full_join(
      clan_df %>% rename_with(~ paste0(.x, "_Clan"), c("C1", "C2", "C3")),
      by = "year"
    ) %>%
    arrange(year) %>%
    mutate(year = as.character(year)) %>%
    bind_rows(
      summarise(.,
        across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%
        mutate(year = "ALL", .before = 1)
    ) %>%
    select(
      year,
      C1_HH, C1_Clan,
      C2_HH, C2_Clan,
      C3_HH, C3_Clan
    ) %>%
    write_csv(here("7_nuclear_family", "output", "all_nuclear_family",
                   paste0(var_label, "_C123.csv")))
}


# Income, weighted
hh   <- C123_by_year(r_hh,    inc_all,        weight = TRUE, weight_var = "fam_weight")
clan <- C123_by_year(r_clans, inc_all,        weight = TRUE, weight_var = "clan_weight")
join_hh_clan(hh, clan, "income")

# Wealth excluding home equity, weighted
hh   <- C123_by_year(r_hh_wealth,    wealth_nohouse, weight = TRUE, weight_var = "fam_weight")
clan <- C123_by_year(r_clans_wealth, wealth_nohouse, weight = TRUE, weight_var = "clan_weight")
join_hh_clan(hh, clan, "wealth_nohome")

# Wealth including home equity, weighted
hh   <- C123_by_year(r_hh_wealth,    wealth, weight = TRUE, weight_var = "fam_weight")
clan <- C123_by_year(r_clans_wealth, wealth, weight = TRUE, weight_var = "clan_weight")
join_hh_clan(hh, clan, "wealth_withhome")



