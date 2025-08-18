# INPUTS: 3_households/output/households.rds, 3_robust_households/output/robust_households.rds
# INPUTS: 4_clans/output/clans.rds, 4_robust_clans/output/robust_clans.rds
# OUTPUTS: 7_summary/output/summary_statistics.docx


# DATA ---------------------------------------------------------------------
r_hh     <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans  <- readRDS(here("4_clans", "output", "robust_clans.rds"))

# Restrict to wealth supplement years
wealth_years <- c(1984, 1989, 1994, seq(1999, 2021, by = 2))
r_hh_wealth    <- r_hh %>% filter(year %in% wealth_years)
r_clans_wealth <- r_clans %>% filter(year %in% wealth_years)


# APPLY WEIGHTS -----------------------------------------------------------------
hh_design <- svydesign(ids = ~cluster, strata = ~stratum,
                       weights = ~fam_weight, data = r_hh, nest = TRUE)

clan_design <- svydesign(ids = ~cluster, strata = ~stratum,
                         weights = ~clan_weight, data = r_clans, nest = TRUE)

hhw_design <- svydesign(ids = ~cluster, strata = ~stratum,
                        weights = ~fam_weight, data = r_hh_wealth, nest = TRUE)

clanw_design <- svydesign(ids = ~cluster, strata = ~stratum,
                          weights = ~clan_weight, data = r_clans_wealth, nest = TRUE)


# FUNCTION FOR A WEIGHTED MEDIAN -------------------------------------------------
get_wtd_median <- function(design, var) {
  q <- svyquantile(
    as.formula(paste0("~", var)),
    design,
    quantiles = 0.5,
    na.rm = TRUE,
    ci = FALSE
  )
  as.numeric(q[1]) 
}

# INCOME SUMMARY -----------------------------------------------------------
hh_summary <- tibble(
  Table        = "Income",
  Unit         = "Household",
  N            = nrow(r_hh),
  black_pct_w  = 100 * as.numeric(svymean(~black_head, hh_design, na.rm = TRUE)),
  black_pct    = 100 * mean(r_hh$black_head, na.rm = TRUE),
  unique_clans = n_distinct(r_hh$id1968),
  mean_val_w   = as.numeric(svymean(~inc_all, hh_design, na.rm = TRUE)),
  sd_val_w     = sqrt(as.numeric(svyvar(~inc_all, hh_design, na.rm = TRUE))),
  mean_val     = mean(r_hh$inc_all, na.rm = TRUE),
  sd_val       = sd(r_hh$inc_all, na.rm = TRUE),
  median_val_w = get_wtd_median(hh_design, "inc_all"),
  median_val   = median(r_hh$inc_all, na.rm = TRUE)
)

clan_summary <- tibble(
  Table        = "Income",
  Unit         = "Clan",
  N            = nrow(r_clans),
  black_pct_w  = 100 * as.numeric(svymean(~black_clan, clan_design, na.rm = TRUE)),
  black_pct    = 100 * mean(r_clans$black_clan, na.rm = TRUE),
  unique_clans = n_distinct(r_clans$id1968),
  mean_val_w   = as.numeric(svymean(~inc_all, clan_design, na.rm = TRUE)),
  sd_val_w     = sqrt(as.numeric(svyvar(~inc_all, clan_design, na.rm = TRUE))),
  mean_val     = mean(r_clans$inc_all, na.rm = TRUE),
  sd_val       = sd(r_clans$inc_all, na.rm = TRUE),
  median_val_w = get_wtd_median(clan_design, "inc_all"),
  median_val   = median(r_clans$inc_all, na.rm = TRUE)
)

# WEALTH SUMMARY -----------------------------------------------------------
hhw_summary <- tibble(
  Table        = "Wealth",
  Unit         = "Household",
  N            = nrow(r_hh_wealth),
  black_pct_w  = 100 * as.numeric(svymean(~black_head, hhw_design, na.rm = TRUE)),
  black_pct    = 100 * mean(r_hh_wealth$black_head, na.rm = TRUE),
  unique_clans = n_distinct(r_hh_wealth$id1968),
  mean_val_w   = as.numeric(svymean(~wealth_nohouse, hhw_design, na.rm = TRUE)),
  sd_val_w     = sqrt(as.numeric(svyvar(~wealth_nohouse, hhw_design, na.rm = TRUE))),
  mean_val     = mean(r_hh_wealth$wealth_nohouse, na.rm = TRUE),
  sd_val       = sd(r_hh_wealth$wealth_nohouse, na.rm = TRUE),
  median_val_w = get_wtd_median(hhw_design, "wealth_nohouse"),
  median_val   = median(r_hh_wealth$wealth_nohouse, na.rm = TRUE)
)

clanw_summary <- tibble(
  Table        = "Wealth",
  Unit         = "Clan",
  N            = nrow(r_clans_wealth),
  black_pct_w  = 100 * as.numeric(svymean(~black_clan, clanw_design, na.rm = TRUE)),
  black_pct    = 100 * mean(r_clans_wealth$black_clan, na.rm = TRUE),
  unique_clans = n_distinct(r_clans_wealth$id1968),
  mean_val_w   = as.numeric(svymean(~wealth_nohouse, clanw_design, na.rm = TRUE)),
  sd_val_w     = sqrt(as.numeric(svyvar(~wealth_nohouse, clanw_design, na.rm = TRUE))),
  mean_val     = mean(r_clans_wealth$wealth_nohouse, na.rm = TRUE),
  sd_val       = sd(r_clans_wealth$wealth_nohouse, na.rm = TRUE),
  median_val_w = get_wtd_median(clanw_design, "wealth_nohouse"), # fixed
  median_val   = median(r_clans_wealth$wealth_nohouse, na.rm = TRUE)
)

# COMBINE AND OUTPUT ------------------------------------------------------
summary_table <- bind_rows(hh_summary, clan_summary, hhw_summary, clanw_summary)

summary_output <- summary_table %>%
  mutate(
    black_pct    = sprintf("%.1f%%", black_pct),
    black_pct_w  = sprintf("%.1f%%", black_pct_w),
    mean_val     = dollar(mean_val, accuracy = 1),
    median_val   = dollar(median_val, accuracy = 1),
    sd_val       = dollar(sd_val, accuracy = 1),
    mean_val_w   = dollar(mean_val_w, accuracy = 1),
    median_val_w = dollar(median_val_w, accuracy = 1),
    sd_val_w     = dollar(sd_val_w, accuracy = 1)
  ) %>%
  gt(groupname_col = "Table") %>%
  tab_header(title = md("**Descriptive Statistics**")) %>%
  cols_label(
    Unit         = "Unit",
    N            = "N",
    black_pct_w  = "Black % (Weighted)",
    black_pct    = "Black % (Unweighted)",
    unique_clans = "Unique Clans",
    mean_val_w   = "Mean (Weighted)",
    median_val_w = "Median (Weighted)",
    sd_val_w     = "SD (Weighted)",
    mean_val     = "Mean (Unweighted)",
    median_val   = "Median (Unweighted)",
    sd_val       = "SD (Unweighted)"
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size  = px(12),
    table.width      = pct(100)
  ) %>%
  tab_source_note(
    source_note = "Notes: Estimates are weighted for clans and households using survey design with strata and clusters. Wealth estimates exclude home equity."
  )

gtsave(summary_output, here("7_summary", "output", "summary_statistics.docx"))



# CREATE DECILE PLOTS FOR INCOME AND WEALTH ---------------------------------------------
# Function to get weighted deciles
get_deciles <- function(design, var, year) {
  qs <- svyquantile(
    as.formula(paste0("~", var)),
    design,
    quantiles = seq(0.1, 1, 0.1),
    na.rm = TRUE,
    ci = FALSE
  )
  tibble(
    decile = 1:10,
    value  = as.numeric(qs[[1]]),  # extract numeric vector
    year   = year
  )
}


decile_years <- c(1979, 1999, 2019)

hh_sub    <- r_hh    %>% filter(year %in% decile_years)
clans_sub <- r_clans %>% filter(year %in% decile_years)

# Household weights
hh_designs <- hh_sub %>%
  group_split(year) %>%
  setNames(decile_years) %>%
  map(~ svydesign(
    ids     = ~cluster,
    strata  = ~stratum,
    weights = ~fam_weight,
    data    = .x,
    nest    = TRUE
  ))

# Clan weights
clan_designs <- clans_sub %>%
  group_split(year) %>%
  setNames(decile_years) %>%
  map(~ svydesign(
    ids     = ~cluster,
    strata  = ~stratum,
    weights = ~clan_weight,
    data    = .x,
    nest    = TRUE
  ))

# Calculate households
hh_deciles <- map2_dfr(hh_designs, names(hh_designs),
                       ~ get_deciles(.x, "inc_all", as.integer(.y))) %>%
  mutate(unit = "Household")

# Calculate clans
clan_deciles <- map2_dfr(clan_designs, names(clan_designs),
                         ~ get_deciles(.x, "inc_all", as.integer(.y))) %>%
  mutate(unit = "Clan")

# Combine and plot
deciles_all <- bind_rows(hh_deciles, clan_deciles)

ggplot(deciles_all, aes(x = decile, y = value, color = factor(year), linetype = unit)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Weighted Income Deciles in 1979, 1999, 2019",
    x = "Decile",
    y = "Income (weighted $)",
    color = "Year",
    linetype = "Unit"
  ) +
  theme_minimal(base_size = 13)


rm(list = ls())