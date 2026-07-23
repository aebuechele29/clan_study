library(here)
library(survey)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(tidyr)
library(flextable)
library(officer)

# LOAD DATA ------------------------------------------------------------------
hh <- readRDS(here("3_households", "output", "households.rds"))
clans <- readRDS(here("4_clans", "output", "clans.rds"))
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

hh_wealth <- readRDS(
  here("3_households", "output", "households_wealth.rds")
)

clans_wealth <- readRDS(
  here("4_clans", "output", "clans_wealth.rds")
)

r_hh_wealth <- readRDS(
  here("3_households", "output", "robust_households_wealth.rds")
)

r_clans_wealth <- readRDS(
  here("4_clans", "output", "robust_clans_wealth.rds")
)


# APPLY WEIGHTS ---------------------------------------------------------------
# NOTE: the family demographic summaries below (build_family_demo) are now
# unweighted and do not use these survey designs. Left in place in case they
# are used elsewhere in this script / downstream.
# Adjust weighted designs for lonely PSUs
options(survey.lonely.psu = "adjust")

hh_design <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~fam_weight,
  data = r_hh,
  nest = TRUE
)

clan_design <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~clan_weight,
  data = r_clans,
  nest = TRUE
)

hhw_design <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~fam_weight,
  data = r_hh_wealth,
  nest = TRUE
)

clanw_design <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~clan_weight,
  data = r_clans_wealth,
  nest = TRUE
)

# Non-robust (full) designs — needed to show what family composition looks
# like before the robustness/kin-linkage restrictions are applied.
hh_design_full <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~fam_weight,
  data = hh,
  nest = TRUE
)

clan_design_full <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~clan_weight,
  data = clans,
  nest = TRUE
)

hhw_design_full <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~fam_weight,
  data = hh_wealth,
  nest = TRUE
)

clanw_design_full <- svydesign(
  ids = ~cluster,
  strata = ~stratum,
  weights = ~clan_weight,
  data = clans_wealth,
  nest = TRUE
)


# FUNCTIONS FOR SUMMARY TABLES ------------------------------------------------
# Function for a weighted median
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


# Create annual UNWEIGHTED household and clan demographic summaries
# NOTE: takes the raw (data-frame) household and clan data directly, not
# svydesign objects — plain group_by/summarise means, no survey weighting.
#
# hh_age_mean vs. ppl_age_mean:
#   - hh_age_mean is the average, across households, of each household's own
#     mean member age (hh_age_sum / hh_age_n). Every household counts once,
#     regardless of how many people are in it — this is "the average
#     household's age."
#   - ppl_age_mean pools hh_age_sum and hh_age_n across all households before
#     dividing, so every *person* counts once — this is "the average
#     individual's age," and will differ from hh_age_mean whenever household
#     size correlates with age (e.g. larger households pull ppl_age_mean
#     toward their composition more than hh_age_mean does).
build_family_demo <- function(hh_data, clan_data) {

  # Household-level variables:
  # Unweighted averages describe the average household in each year
  hh_summary <- hh_data %>%
    group_by(year) %>%
    summarise(
      hh_children = mean(hh_children, na.rm = TRUE), # Average number of children per household
      hh_other = mean(hh_other, na.rm = TRUE),        # Average number of other family members per household

      # Average age of the average household (households weighted equally)
      hh_age_mean = mean(
        if_else(hh_age_n > 0, hh_age_sum / hh_age_n, NA_real_),
        na.rm = TRUE
      ),

      # Average age of the average individual (people weighted equally)
      ppl_age_mean = if_else(
        sum(hh_age_n, na.rm = TRUE) > 0,
        sum(hh_age_sum, na.rm = TRUE) / sum(hh_age_n, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )

  # Clan-level variables:
  # Unweighted averages describe the average clan in each year
  clan_summary <- clan_data %>%
    group_by(year) %>%
    summarise(
      clan_age_mean = mean(clan_age_mean, na.rm = TRUE),                 # Average age of all clan members
      numclan = mean(numclan, na.rm = TRUE),                             # Number of households in the clan
      num_clan_people = mean(num_clan_people, na.rm = TRUE),             # Number of people in the clan
      clan_children_hh_mean = mean(clan_children_hh_mean, na.rm = TRUE), # Average number of children per household within the clan
      clan_other_hh_mean = mean(clan_other_hh_mean, na.rm = TRUE),       # Average number of other family members per household within the clan
      .groups = "drop"
    )

  left_join(
    hh_summary,
    clan_summary,
    by = "year"
  ) %>%
    arrange(year)
}


# CREATE FAMILY DEMOGRAPHIC SUMMARY FILES -------------------------------------
# Income sample: robust household and clan samples
inc_family_demo <- build_family_demo(
  r_hh,
  r_clans
)

# Wealth sample: robust household and clan samples, wealth years only
w_family_demo <- build_family_demo(
  r_hh_wealth,
  r_clans_wealth
)

# Income sample: full (non-robust) household and clan samples.
# Used to show the selection effect of the robust kin-linkage criteria —
# i.e., how family composition in the robust sample compares to the full,
# unrestricted PSID sample in each year.
inc_family_demo_full <- build_family_demo(
  hh,
  clans
)

# Wealth sample: full (non-robust) household and clan samples, wealth years only
w_family_demo_full <- build_family_demo(
  hh_wealth,
  clans_wealth
)


# EXPORT ----------------------------------------------------------------------
write_csv(
  inc_family_demo,
  here("5_summary", "output", "inc_family_demo.csv")
)

write_csv(
  w_family_demo,
  here("5_summary", "output", "w_family_demo.csv")
)

write_csv(
  inc_family_demo_full,
  here("5_summary", "output", "inc_family_demo_full.csv")
)

write_csv(
  w_family_demo_full,
  here("5_summary", "output", "w_family_demo_full.csv")
)


# Function to build quartile summary rows
build_quartile_rows <- function(hh_df, cl_df, var, year,
                                hh_black_var = "black_head",
                                cl_black_var = "black_clan") {
  if (nrow(hh_df) == 0 || nrow(cl_df) == 0) return(NULL)

  hh_d <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~fam_weight,  data = hh_df, nest = TRUE)
  cl_d <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~clan_weight, data = cl_df, nest = TRUE)

  hh_q <- assign_quartile(hh_d, var)
  hh_mean <- svyby(as.formula(paste0("~", var)), ~quartile, hh_q, svymean, na.rm = TRUE) |>
    transmute(Quartile = as.character(quartile),
              Mean = as.numeric(.data[[var]]))
  hh_people <- svyby(~numfu, ~quartile, hh_q, svymean, na.rm = TRUE) |>
    transmute(Quartile = as.character(quartile),
              `Avg # People` = as.numeric(numfu))
  hh_black <- svyby(as.formula(paste0("~", hh_black_var)), ~quartile, hh_q, svymean, na.rm = TRUE)
  hh_black <- tibble(
    Quartile = as.character(hh_black$quartile),
    `% Black` = 100 * as.numeric(hh_black[[hh_black_var]])
  )
  hh_tab <- list(hh_people, hh_mean, hh_black) |>
    purrr::reduce(left_join, by = "Quartile") |>
    mutate(Year = year, Unit = "Household", `Avg # HH per Clan` = NA_real_) |>
    select(Year, Unit, Quartile, `Avg # People`, `Avg # HH per Clan`, `% Black`, Mean)

  cl_q <- assign_quartile(cl_d, var)
  cl_mean <- svyby(as.formula(paste0("~", var)), ~quartile, cl_q, svymean, na.rm = TRUE) |>
    transmute(Quartile = as.character(quartile),
              Mean = as.numeric(.data[[var]]))
  cl_people <- svyby(~num_clan_people, ~quartile, cl_q, svymean, na.rm = TRUE) |>
    transmute(Quartile = as.character(quartile),
              `Avg # People` = as.numeric(num_clan_people))
  cl_hh <- svyby(~numclan, ~quartile, cl_q, svymean, na.rm = TRUE) |>
    transmute(Quartile = as.character(quartile),
              `Avg # HH per Clan` = as.numeric(numclan))
  cl_black <- svyby(as.formula(paste0("~", cl_black_var)), ~quartile, cl_q, svymean, na.rm = TRUE)
  cl_black <- tibble(
    Quartile = as.character(cl_black$quartile),
    `% Black` = 100 * as.numeric(cl_black[[cl_black_var]])
  )
  cl_tab <- list(cl_people, cl_hh, cl_black, cl_mean) |>
    purrr::reduce(left_join, by = "Quartile") |>
    mutate(Year = year, Unit = "Clan") |>
    select(Year, Unit, Quartile, `Avg # People`, `Avg # HH per Clan`, `% Black`, Mean)

  bind_rows(hh_tab, cl_tab) |>
    mutate(Quartile = factor(Quartile,
                             levels = c("Q1 (Lowest 25%)","Q2","Q3","Q4 (Highest 25%)")))
}

# Functions nested within the quartile summary function
assign_quartile <- function(design, var,
                            probs = c(0, .25, .5, .75, 1),
                            labs = c("Q1 (Lowest 25%)", "Q2", "Q3", "Q4 (Highest 25%)")) {
  qs <- svyquantile(as.formula(paste0("~", var)), design,
                    quantiles = probs, na.rm = TRUE, ci = FALSE) |> unlist()
  design$variables$quartile <- cut(design$variables[[var]],
                                   breaks = c(-Inf, qs[2:4], Inf),
                                   labels = labs, include.lowest = TRUE, right = TRUE)
  design
}

fmt_table <- function(dat) {
  flextable(dat) |>
    autofit() |>
    theme_vanilla() |>
    fontsize(size = 12, part = "all") |>
    fontsize(size = 10, part = "body") |>
    align(align = "center", part = "all")
}
doc_write <- function(ft, caption, note, outpath) {
  note_style <- fp_text(italic = TRUE, font.size = 10)
  ft <- set_caption(ft, caption)
  read_docx() |>
    body_add_par("", style = "Normal") |>
    body_add_flextable(ft) |>
    body_add_fpar(fpar(ftext(note, prop = note_style), fp_p = fp_par(text.align = "center"))) |>
    print(target = outpath)
  invisible(NULL)
}



# SUMMARY TABLE FOR INCOME AND WEALTH --------------------------------------------
# Income
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

# Wealth
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

# Combine
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

# Export
write_csv(summary_output, here("5_summary", "output", "summary_statistics.csv"))



# INCOME  AND WEALTH QUARTILES --------------------------------------------------
# Income
years_quartile <- c(1979, 1999, 2019)
hh_inc_sub <- r_hh    |> dplyr::filter(year %in% years_quartile)
cl_inc_sub <- r_clans |> dplyr::filter(year %in% years_quartile)

res_inc_quarts <- lapply(years_quartile, function(yr) {
  hh_y <- hh_inc_sub |> dplyr::filter(year == yr)
  cl_y <- cl_inc_sub |> dplyr::filter(year == yr)
  build_quartile_rows(hh_y, cl_y, var = "inc_all", year = yr,
                      hh_black_var = "black_head", cl_black_var = "black_clan")
}) |> dplyr::bind_rows() |>
  dplyr::arrange(Year, factor(Unit, levels = c("Household", "Clan")), Quartile)

inc_by_year <- read_csv(here("6_calculate_gini/output/income.csv"))
gini_lookup_inc_q <- inc_by_year |>
  dplyr::filter(year %in% years_quartile) |>
  dplyr::transmute(Year = as.numeric(year),
                   Household = r_hh_w_inc, Clan = r_cl_w_inc) |>
  tidyr::pivot_longer(c(Household, Clan), names_to = "Unit", values_to = "Gini")

t_inc_q <- res_inc_quarts |>
  dplyr::left_join(gini_lookup_inc_q, by = c("Year","Unit")) |>
  dplyr::relocate(Gini, .after = Unit)

t_inc_q_fmt <- t_inc_q |>
  dplyr::mutate(
    `Avg # People` = sprintf("%.2f", `Avg # People`),
    `Avg # HH per Clan` = dplyr::if_else(Unit == "Clan",
                                         sprintf("%.2f", `Avg # HH per Clan`), ""),
    `% Black` = sprintf("%.1f", `% Black`),
    Mean = formatC(Mean, format = "f", digits = 0, big.mark = ","),
    Gini = sprintf("%.3f", Gini)
  )

ft_inc_q <- fmt_table(t_inc_q_fmt)
doc_write(
  ft_inc_q,
  "Figure 1B: Income Quartiles (1979, 1999, 2019)",
  "Note: Values are survey-weighted. Quartiles computed within each year and unit. Avg # HH per Clan applies only to clans. % Black is the weighted share within each quartile.",
  here("5_summary", "output", "income_quartiles.docx")
)


# Wealth
years_quartile_w <- c(1989, 2009, 2019)
hh_w_sub <- r_hh    |> dplyr::filter(year %in% years_quartile_w)
cl_w_sub <- r_clans |> dplyr::filter(year %in% years_quartile_w)

res_w_quarts <- lapply(years_quartile_w, function(yr) {
  hh_y <- hh_w_sub |> dplyr::filter(year == yr)
  cl_y <- cl_w_sub |> dplyr::filter(year == yr)
  build_quartile_rows(hh_y, cl_y, var = "wealth_nohouse", year = yr,
                      hh_black_var = "black_head", cl_black_var = "black_clan")
}) |> dplyr::bind_rows() |>
  dplyr::arrange(Year, factor(Unit, levels = c("Household", "Clan")), Quartile)

wealth_by_year <- read_csv(here("6_calculate_gini/output/wealth_nohouse.csv"))
gini_lookup_w_q <- wealth_by_year |>
  dplyr::filter(year %in% years_quartile_w) |>
  dplyr::transmute(Year = as.numeric(year),
                   Household = r_hh_w_wealth, Clan = r_cl_w_wealth) |>
  tidyr::pivot_longer(c(Household, Clan), names_to = "Unit", values_to = "Gini")

t_w_q <- res_w_quarts |>
  dplyr::left_join(gini_lookup_w_q, by = c("Year","Unit")) |>
  dplyr::relocate(Gini, .after = Unit)

t_w_q_fmt <- t_w_q |>
  dplyr::mutate(
    `Avg # People` = sprintf("%.2f", `Avg # People`),
    `Avg # HH per Clan` = dplyr::if_else(Unit == "Clan",
                                         sprintf("%.2f", `Avg # HH per Clan`), ""),
    `% Black` = sprintf("%.1f", `% Black`),
    Mean = formatC(Mean, format = "f", digits = 0, big.mark = ","),
    Gini = sprintf("%.3f", Gini)
  )

ft_w_q <- fmt_table(t_w_q_fmt)
doc_write(
  ft_w_q,
  "Figure 2B: Wealth Quartiles (1989, 2009, 2019)",
  "Note: Values are survey-weighted. Wealth excludes home equity. Quartiles computed within each year and unit. Avg # HH per Clan applies only to clans. % Black is the weighted share within each quartile.",
  here("5_summary", "output", "wealth_quartiles.docx")
)