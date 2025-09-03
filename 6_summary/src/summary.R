# INPUTS: 3_households/output/households.rds, 3_robust_households/output/robust_households.rds
# INPUTS: 4_clans/output/clans.rds, 4_robust_clans/output/robust_clans.rds
# OUTPUTS: 6_summary/output/summary_statistics.docx


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

# COMBINE -------------------------------------------------------------------
summary_table <- bind_rows(hh_summary, clan_summary, hhw_summary, clanw_summary)

summary_output <- summary_table %>%
  mutate(
    black_pct    = sprintf("%.1f", black_pct),     # keep as numbers or percents
    black_pct_w  = sprintf("%.1f", black_pct_w),
    mean_val     = round(mean_val, 1),
    median_val   = round(median_val, 1),
    sd_val       = round(sd_val, 1),
    mean_val_w   = round(mean_val_w, 1),
    median_val_w = round(median_val_w, 1),
    sd_val_w     = round(sd_val_w, 1)
  )

# EXPORT 
write_csv(summary_output, here("6_summary", "output", "summary_statistics.csv"))

rm(list = ls())