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
# Without this there is one strata with a single PSU, which causes errors
options(survey.lonely.psu = "adjust")

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



# INCOME QUINTILE PLOT -----------------------------------------------------------
years_quintile <- c(1979, 1999, 2019)

hh_quint <- r_hh    %>% filter(year %in% years_quintile)
cl_quint <- r_clans %>% filter(year %in% years_quintile)

# Function to compute weighted quintile means 
get_quintile_means <- function(design, var) {
  qtiles <- svyquantile(as.formula(paste0("~", var)),
                        design, quantiles = seq(0, 1, 0.2),
                        na.rm = TRUE, ci = FALSE) |> unlist()

  design$variables$quintile <- cut(
    design$variables[[var]],
    breaks = c(-Inf, qtiles[2:5], Inf),
    labels = c("Lowest 20%", "2nd 20%", "3rd 20%", "4th 20%", "Highest 20%"),
    include.lowest = TRUE
  )

  m <- svyby(as.formula(paste0("~", var)),
             ~quintile, design, svymean, na.rm = TRUE)

  setNames(as.numeric(m[[var]]), as.character(m$quintile))
}

# Function to get results for each year and unit
results <- lapply(years_quintile, function(yr) {

  # Households
  hh_y <- hh_quint %>% filter(year == yr)
  hh_d <- svydesign(ids = ~cluster, strata = ~stratum,
                    weights = ~fam_weight, data = hh_y, nest = TRUE)

  tibble(
    Year                   = yr,
    Unit                   = "Household",
    Median                 = get_wtd_median(hh_d, "inc_all"),
    Mean                   = as.numeric(svymean(~inc_all, hh_d, na.rm = TRUE)),
    people_or_clan         = as.numeric(svymean(~numfu, hh_d, na.rm = TRUE)),
    `Lowest 20%`           = get_quintile_means(hh_d, "inc_all")[["Lowest 20%"]],
    `2nd 20%`              = get_quintile_means(hh_d, "inc_all")[["2nd 20%"]],
    `3rd 20%`              = get_quintile_means(hh_d, "inc_all")[["3rd 20%"]],
    `4th 20%`              = get_quintile_means(hh_d, "inc_all")[["4th 20%"]],
    `Highest 20%`          = get_quintile_means(hh_d, "inc_all")[["Highest 20%"]]
  )

}) |> bind_rows(
  lapply(years_quintile, function(yr) {

    # Clans
    cl_y <- cl_quint %>% filter(year == yr)
    cl_d <- svydesign(ids = ~cluster, strata = ~stratum,
                      weights = ~clan_weight, data = cl_y, nest = TRUE)

    tibble(
      Year                   = yr,
      Unit                   = "Clan",
      Median                 = get_wtd_median(cl_d, "inc_all"),
      Mean                   = as.numeric(svymean(~inc_all, cl_d, na.rm = TRUE)),
      people_or_clan         = as.numeric(svymean(~num_clan_people, cl_d, na.rm = TRUE)),
      hh_per_clan            = as.numeric(svymean(~numclan, cl_d, na.rm = TRUE)),
      `Lowest 20%`           = get_quintile_means(cl_d, "inc_all")[["Lowest 20%"]],
      `2nd 20%`              = get_quintile_means(cl_d, "inc_all")[["2nd 20%"]],
      `3rd 20%`              = get_quintile_means(cl_d, "inc_all")[["3rd 20%"]],
      `4th 20%`              = get_quintile_means(cl_d, "inc_all")[["4th 20%"]],
      `Highest 20%`          = get_quintile_means(cl_d, "inc_all")[["Highest 20%"]]
    )

  })
)

# Create and format final table
summary_table <- bind_rows(results) %>%
  rename(
    `Avg. No. of Individuals` = people_or_clan,
    `Avg. No. of HH`          = hh_per_clan
  ) %>%
  select(
    Year, Unit,
    `Avg. No. of Individuals`, `Avg. No. of HH`,
    Median, Mean,
    `Lowest 20%`, `2nd 20%`, `3rd 20%`, `4th 20%`, `Highest 20%`
  ) %>%
  arrange(Year, factor(Unit, levels = c("Household", "Clan")))

summary_fmt <- summary_table %>%
  mutate(across(
    where(is.numeric) & !matches("^Year$"),
    ~formatC(.x, format = "f", digits = 2, big.mark = ",")
  ))

ft <- flextable(summary_fmt) |>
  set_caption("Figure X. Weighted Income Median, Mean, and Quintile Means for Households and Clans – 1979, 1999, 2019") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

note_style <- fp_text(italic = TRUE, font.size = 10)

doc <- read_docx() |>
  body_add_par("", style = "Normal") |>          
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(
      ftext("Note: All values are weighted using PSID family or clan weights. Both median and quintile means are computed within each year’s distribution.",
      prop = note_style),
      fp_p = fp_par(text.align = "center")
    )
  )

print(doc, target = here("6_summary", "output", "quintile_summary.docx"))


