library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(cowplot)
library(flextable)
library(officer)
library(grid)
library(gridExtra)

source(here::here("functions", "all_functions.R"))

# Formatting helpers (mirrors those in the RMD setup chunk)
fmt2 <- function(x) format(round(x, 2), nsmall = 2)
fmt1 <- function(x) format(round(x, 1), nsmall = 1)
fmt0 <- function(x) format(round(x, 0), big.mark = ",")
fmtp <- function(x) paste0(format(round(x, 2), nsmall = 2), "%")

if (!exists("SAVE_FILES")) SAVE_FILES <- TRUE
dir.create(here("9_figures", "output"), recursive = TRUE, showWarnings = FALSE)

# Global styles
base_family <- "serif"
base_size   <- 22
title_size  <- 26
sub_size    <- 24
note_size   <- 14

NAVY        <- "#185FA5"
PALE_NAVY   <- "#85B7EB"
CORAL       <- "#D85A30"
PALE_CORAL  <- "#F0997B"
PURPLE      <- "#534AB7"
TEAL        <- "#0F6E56"
GREY_HDR    <- "#F2F2F2"

theme_set(theme_minimal(base_size = base_size, base_family = base_family))


# Load data
r_hh           <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans        <- readRDS(here("4_clans",      "output", "robust_clans.rds"))
r_hh_wealth    <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans_wealth <- readRDS(here("4_clans",      "output", "robust_clans_wealth.rds"))
mismatched     <- readRDS(here("3_households", "output", "mismatched.rds"))

inc_by_year    <- read_csv(here("6_calculate_gini", "output", "income.csv"),
                           show_col_types = FALSE)
wealth_by_year <- read_csv(here("6_calculate_gini", "output", "wealth_withhome.csv"),
                           show_col_types = FALSE)
summary_stats  <- read_csv(here("5_summary",         "output", "summary_statistics.csv"),
                           show_col_types = FALSE)
wealth_ratios  <- read_csv(here("7_gini_by_race",    "output", "wealth_withhome_race_ratios.csv"),
                           show_col_types = FALSE)


# Sample size scalars
n_mismatched   <- nrow(mismatched)
inc_uniq_clans <- n_distinct(r_clans$id1968)
w_uniq_clans   <- n_distinct(r_clans_wealth$id1968)
inc_hh_years   <- nrow(r_hh)
inc_cl_years   <- nrow(r_clans)
w_hh_years     <- nrow(r_hh_wealth)
w_cl_years     <- nrow(r_clans_wealth)

# Year-specific sample size scalars (for Figure 2 note)
inc_hh_years_1984 <- nrow(r_hh      %>% filter(year == 1984))
inc_cl_years_1984 <- nrow(r_clans   %>% filter(year == 1984))
inc_hh_years_2023 <- nrow(r_hh      %>% filter(year == 2023))
inc_cl_years_2023 <- nrow(r_clans   %>% filter(year == 2023))
w_hh_years_1984   <- nrow(r_hh_wealth    %>% filter(year == 1984))
w_cl_years_1984   <- nrow(r_clans_wealth %>% filter(year == 1984))
w_hh_years_2023   <- nrow(r_hh_wealth    %>% filter(year == 2023))
w_cl_years_2023   <- nrow(r_clans_wealth %>% filter(year == 2023))


# Gini scalars for inline Rmd text
inc_hh_1969 <- get_gini_at(inc_by_year,    1969, r_hh_w_inc)
inc_hh_2023 <- get_gini_at(inc_by_year,    2023, r_hh_w_inc)
inc_cl_1969 <- get_gini_at(inc_by_year,    1969, r_cl_w_inc)
inc_cl_2023 <- get_gini_at(inc_by_year,    2023, r_cl_w_inc)
w_hh_1984   <- get_gini_at(wealth_by_year, 1984, r_hh_w_wealth)
w_hh_2023   <- get_gini_at(wealth_by_year, 2023, r_hh_w_wealth)
w_cl_1984   <- get_gini_at(wealth_by_year, 1984, r_cl_w_wealth)
w_cl_2023   <- get_gini_at(wealth_by_year, 2023, r_cl_w_wealth)

inc_hh_pct <- 100 * (inc_hh_2023 - inc_hh_1969) / inc_hh_1969
inc_cl_pct <- 100 * (inc_cl_2023 - inc_cl_1969) / inc_cl_1969
w_hh_pct   <- 100 * (w_hh_2023   - w_hh_1984)   / w_hh_1984
w_cl_pct   <- 100 * (w_cl_2023   - w_cl_1984)    / w_cl_1984

inc_gap_1969          <- inc_hh_1969 - inc_cl_1969
inc_gap_2023          <- inc_hh_2023 - inc_cl_2023
inc_gap_change_per_yr <- (inc_gap_2023 - inc_gap_1969) / (2023 - 1969)
inc_convergence_yr    <- round(2023 + (0 - inc_gap_2023) / inc_gap_change_per_yr)

w_gap_1984 <- w_hh_1984 - w_cl_1984
w_gap_2023 <- w_hh_2023 - w_cl_2023

avg_inc_hh       <- inc_by_year    %>% filter(year == "ALL") %>% pull(r_hh_w_inc)
avg_inc_cl       <- inc_by_year    %>% filter(year == "ALL") %>% pull(r_cl_w_inc)
avg_w_hh         <- wealth_by_year %>% filter(year == "ALL") %>% pull(r_hh_w_wealth)
avg_w_cl         <- wealth_by_year %>% filter(year == "ALL") %>% pull(r_cl_w_wealth)
inc_gini_diff    <- avg_inc_hh - avg_inc_cl
inc_gini_pct_red <- 100 * inc_gini_diff / avg_inc_hh
w_gini_diff      <- avg_w_hh   - avg_w_cl
w_gini_pct_red   <- 100 * w_gini_diff  / avg_w_hh

inc_mean_hh_w <- summary_stats %>%
  filter(Table == "Income",  Unit == "Household") %>% pull(mean_val_w) %>% first()
inc_mean_cl_w <- summary_stats %>%
  filter(Table == "Income",  Unit == "Clan")      %>% pull(mean_val_w) %>% first()
w_mean_hh_w   <- summary_stats %>%
  filter(Table == "Wealth",  Unit == "Household") %>% pull(mean_val_w) %>% first()
w_mean_cl_w   <- summary_stats %>%
  filter(Table == "Wealth",  Unit == "Clan")      %>% pull(mean_val_w) %>% first()

# ── Figure 3 scalars: Mean wealth ratios (retained for Appendix E) ────────────
w_ratio_hh_first <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_hh_w_mean_ratio)
w_ratio_hh_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_hh_w_mean_ratio)
w_ratio_cl_first <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_cl_w_mean_ratio)
w_ratio_cl_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_cl_w_mean_ratio)

w_ratio_hh_pct <- 100 * (w_ratio_hh_last - w_ratio_hh_first) / abs(w_ratio_hh_first)
w_ratio_cl_pct <- 100 * (w_ratio_cl_last - w_ratio_cl_first) / abs(w_ratio_cl_first)
avg_w_ratio_gap <- round(mean(wealth_ratios$r_hh_w_mean_ratio - wealth_ratios$r_cl_w_mean_ratio,
                              na.rm = TRUE), 3)

w_first_yr <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(year)
w_last_yr  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(year)

# ── Figure 3 scalars: Median wealth ratios ────────────────────────────────────
w_med_ratio_hh_first <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_hh_w_median_ratio)
w_med_ratio_hh_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_hh_w_median_ratio)
w_med_ratio_cl_first <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_cl_w_median_ratio)
w_med_ratio_cl_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_cl_w_median_ratio)

w_med_ratio_hh_pct <- 100 * (w_med_ratio_hh_last - w_med_ratio_hh_first) / abs(w_med_ratio_hh_first)
w_med_ratio_cl_pct <- 100 * (w_med_ratio_cl_last - w_med_ratio_cl_first) / abs(w_med_ratio_cl_first)
avg_w_med_ratio_gap <- round(mean(wealth_ratios$r_hh_w_median_ratio - wealth_ratios$r_cl_w_median_ratio,
                                  na.rm = TRUE), 3)

# ── Pooled race means and medians ─────────────────────────────────────────────
.race_means <- function(df, value_var, weight_var, race_var) {
  df_s <- df %>% dplyr::filter(is.finite(.data[[value_var]]),
                                is.finite(.data[[weight_var]]),
                                .data[[weight_var]] > 0)
  list(
    black    = wtd_mean(df_s[[value_var]][df_s[[race_var]] == 1],
                        df_s[[weight_var]][df_s[[race_var]] == 1]),
    nonblack = wtd_mean(df_s[[value_var]][df_s[[race_var]] == 0],
                        df_s[[weight_var]][df_s[[race_var]] == 0])
  )
}

.race_medians <- function(df, value_var, weight_var, race_var) {
  df_s <- df %>% dplyr::filter(is.finite(.data[[value_var]]),
                                is.finite(.data[[weight_var]]),
                                .data[[weight_var]] > 0)
  list(
    black    = wtd_median(df_s[[value_var]][df_s[[race_var]] == 1],
                          df_s[[weight_var]][df_s[[race_var]] == 1]),
    nonblack = wtd_median(df_s[[value_var]][df_s[[race_var]] == 0],
                          df_s[[weight_var]][df_s[[race_var]] == 0])
  )
}

w_race_hh <- .race_means(r_hh_wealth    %>% dplyr::mutate(wealth = wealth / numfu),
                          "wealth", "fam_weight",  "black_head")
w_race_cl <- .race_means(r_clans_wealth %>% dplyr::mutate(wealth = wealth / numclan),
                          "wealth", "clan_weight", "black_clan")
w_med_hh  <- .race_medians(r_hh_wealth    %>% dplyr::mutate(wealth = wealth / numfu),
                             "wealth", "fam_weight",  "black_head")
w_med_cl  <- .race_medians(r_clans_wealth %>% dplyr::mutate(wealth = wealth / numclan),
                             "wealth", "clan_weight", "black_clan")

w_med_ratio_hh_min <- min(wealth_ratios$r_hh_w_median_ratio, na.rm = TRUE)
w_med_ratio_hh_max <- max(wealth_ratios$r_hh_w_median_ratio, na.rm = TRUE)
w_med_ratio_cl_min <- min(wealth_ratios$r_cl_w_median_ratio, na.rm = TRUE)
w_med_ratio_cl_max <- max(wealth_ratios$r_cl_w_median_ratio, na.rm = TRUE)

w_med_ratio_hh_mean <- mean(wealth_ratios$r_hh_w_median_ratio, na.rm = TRUE)
w_med_ratio_cl_mean <- mean(wealth_ratios$r_cl_w_median_ratio, na.rm = TRUE)

w_med_ratio_hh_min_yr <- wealth_ratios$year[which.min(wealth_ratios$r_hh_w_median_ratio)]
w_med_ratio_hh_max_yr <- wealth_ratios$year[which.max(wealth_ratios$r_hh_w_median_ratio)]
w_med_ratio_cl_min_yr <- wealth_ratios$year[which.min(wealth_ratios$r_cl_w_median_ratio)]
w_med_ratio_cl_max_yr <- wealth_ratios$year[which.max(wealth_ratios$r_cl_w_median_ratio)]

# ── Inline scalars: kin group and household size ──────────────────────────────
# Income sample: kin group size (unique clan-year observations weighted by clan_weight)
inc_cl_hh_mean   <- wtd_mean(r_clans$numclan,         r_clans$clan_weight)
inc_cl_hh_sd     <- wtd_sd(r_clans$numclan,           r_clans$clan_weight)
inc_cl_ppl_mean  <- wtd_mean(r_clans$num_clan_people,  r_clans$clan_weight)
inc_cl_ppl_sd    <- wtd_sd(r_clans$num_clan_people,   r_clans$clan_weight)

# Wealth sample: kin group size
w_cl_hh_mean     <- wtd_mean(r_clans_wealth$numclan,        r_clans_wealth$clan_weight)
w_cl_hh_sd       <- wtd_sd(r_clans_wealth$numclan,          r_clans_wealth$clan_weight)
w_cl_ppl_mean    <- wtd_mean(r_clans_wealth$num_clan_people, r_clans_wealth$clan_weight)
w_cl_ppl_sd      <- wtd_sd(r_clans_wealth$num_clan_people,  r_clans_wealth$clan_weight)

# Household size (same for income and wealth samples)
hh_size_mean       <- wtd_mean(r_hh$numfu,        r_hh$fam_weight)
hh_size_sd         <- wtd_sd(r_hh$numfu,          r_hh$fam_weight)
hh_size_w_mean     <- wtd_mean(r_hh_wealth$numfu,  r_hh_wealth$fam_weight)
hh_size_w_sd       <- wtd_sd(r_hh_wealth$numfu,   r_hh_wealth$fam_weight)

# ── Wealth stats by year for Figure 2 note ────────────────────────────────────
w_stats_hh <- r_hh_wealth %>%
  filter(year %in% c(1984, 2023), is.finite(wealth),
         is.finite(fam_weight), fam_weight > 0) %>%
  group_by(year) %>%
  summarise(mean = wtd_mean(wealth / numfu, fam_weight),
            med  = wtd_median(wealth / numfu, fam_weight), .groups = "drop")

w_stats_cl <- r_clans_wealth %>%
  filter(year %in% c(1984, 2023), is.finite(wealth),
         is.finite(clan_weight), clan_weight > 0) %>%
  group_by(year) %>%
  summarise(mean = wtd_mean(wealth / numclan, clan_weight),
            med  = wtd_median(wealth / numclan, clan_weight), .groups = "drop")


# ── Figure 1 — Gini over time ─────────────────────────────────────────────────
shared_y_fig1 <- {
  vals <- c(
    inc_by_year    %>% filter(year != "ALL") %>% pull(r_hh_w_inc),
    inc_by_year    %>% filter(year != "ALL") %>% pull(r_cl_w_inc),
    wealth_by_year %>% filter(year != "ALL") %>% pull(r_hh_w_wealth),
    wealth_by_year %>% filter(year != "ALL") %>% pull(r_cl_w_wealth))
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

fig1_inc <- make_gini_plot(inc_by_year, r_hh_w_inc, r_cl_w_inc,
                           "Gini Coefficient", y_limits = shared_y_fig1)
fig1_w   <- make_gini_plot(wealth_by_year, r_hh_w_wealth, r_cl_w_wealth,
                           "Gini Coefficient", y_limits = shared_y_fig1)

fig1_note <- sprintf(
  paste0(
    "Note: Figure 1 plots Gini coefficients for income (1969\u20132023) and wealth (1984\u20132023) ",
    "at the household and kin group level. ",
    "Gini coefficients are estimated from weighted PSID data. ",
    "Panel A shows that Gini coefficients for income are higher at the household level than at the kin group level. ",
    "There are %s household-years and %s kin group-years. ",
    "Among these, the weighted mean income is $%s (households) and $%s (kin groups). ",
    "Income Ginis changed by %.1f%% for households (%.2f in 1969 to %.2f in 2023) ",
    "and by %.1f%% for kin groups (%.2f to %.2f). ",
    "Panel B shows that Gini coefficients for wealth are also higher at the household level than at the kin group level. ",
    "There are %s household-years and %s kin group-years. ",
    "Among these, the weighted mean wealth is $%s (households) and $%s (kin groups). ",
    "Wealth includes home equity. ",
    "Wealth Ginis changed by %.1f%% for households (%.2f in 1984 to %.2f in 2023) ",
    "and by %.1f%% for kin groups (%.2f to %.2f). ",
    "On average across all years, the income Gini is %.3f higher for households than kin groups; ",
    "the wealth Gini is %.3f higher."
  ),
  fmt0(inc_hh_years), fmt0(inc_cl_years),
  fmt_money0(inc_mean_hh_w), fmt_money0(inc_mean_cl_w),
  inc_hh_pct, inc_hh_1969, inc_hh_2023,
  inc_cl_pct, inc_cl_1969, inc_cl_2023,
  fmt0(w_hh_years), fmt0(w_cl_years),
  fmt_money0(w_mean_hh_w), fmt_money0(w_mean_cl_w),
  w_hh_pct, w_hh_1984, w_hh_2023,
  w_cl_pct, w_cl_1984, w_cl_2023,
  inc_gini_diff, w_gini_diff
)

fig1 <- arrangeGrob(
  textGrob(
    "Figure 1. Income and Wealth Inequality Over Time",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Income inequality from 1969 to 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig1_inc, ncol = 1, heights = c(1, 12)),
    arrangeGrob(
      textGrob("Panel B: Wealth inequality from 1984 to 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig1_w, ncol = 1, heights = c(1, 12)),
    ncol = 2),
  ncol = 1, heights = unit(c(0.5, 9.5), "inches"))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure1.pdf"), fig1, width = 14, height = 11)
  message("Saved: figure1.pdf")
}


# ── Figure 2 — Lorenz curves ──────────────────────────────────────────────────
fig2_inc <- make_lorenz_plot(
  r_hh, r_clans, "inc_all", years = c(1984, 2023),
  ylab   = "Cumulative Proportion of Income",
  colors = c("1984" = NAVY, "2023" = CORAL))
fig2_w <- make_lorenz_plot(
  r_hh_wealth, r_clans_wealth, "wealth", years = c(1984, 2023),
  ylab   = "Cumulative Proportion of Wealth",
  colors = c("1984" = NAVY, "2023" = CORAL))

inc_stats_hh <- r_hh %>%
  filter(year %in% c(1984, 2023), is.finite(inc_all),
         is.finite(fam_weight), fam_weight > 0) %>%
  group_by(year) %>%
  summarise(mean = wtd_mean(inc_all, fam_weight),
            med  = wtd_median(inc_all, fam_weight), .groups = "drop")

inc_stats_cl <- r_clans %>%
  filter(year %in% c(1984, 2023), is.finite(inc_all),
         is.finite(clan_weight), clan_weight > 0) %>%
  group_by(year) %>%
  summarise(mean = wtd_mean(inc_all, clan_weight),
            med  = wtd_median(inc_all, clan_weight), .groups = "drop")

fig2_note <- sprintf(
  paste0(
    "Note: Figure 2 plots Lorenz curves for income and wealth in 1984 and 2023 ",
    "at the household and kin group levels. ",
    "Lorenz curves are estimated from weighted data using PSID family and kin group weights. ",
    "Wealth includes home equity. ",
    "Panel A shows the distribution of income. ",
    "The 1984 estimates are based on %s households and %s kin groups, ",
    "while the 2023 estimates are based on %s households and %s kin groups. ",
    "In 1984, weighted mean (median) income was $%s ($%s) for households ",
    "and $%s ($%s) for kin groups. ",
    "By 2023, weighted mean (median) income had increased to $%s ($%s) for households ",
    "and $%s ($%s) for kin groups. ",
    "Panel B shows the distribution of wealth. ",
    "The 1984 estimates are based on %s households and %s kin groups, ",
    "while the 2023 estimates are based on %s households and %s kin groups. ",
    "In 1984, weighted mean (median) wealth was $%s ($%s) for households ",
    "and $%s ($%s) for kin groups. ",
    "By 2023, weighted mean (median) wealth had increased to $%s ($%s) for households ",
    "and $%s ($%s) for kin groups."
  ),
  # Panel A: sample sizes
  fmt0(inc_hh_years_1984), fmt0(inc_cl_years_1984),
  fmt0(inc_hh_years_2023), fmt0(inc_cl_years_2023),
  # Panel A: income stats 1984
  fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 1984]),
  fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 1984]),
  fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 1984]),
  fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 1984]),
  # Panel A: income stats 2023
  fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 2023]),
  fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 2023]),
  fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 2023]),
  fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 2023]),
  # Panel B: sample sizes
  fmt0(w_hh_years_1984), fmt0(w_cl_years_1984),
  fmt0(w_hh_years_2023), fmt0(w_cl_years_2023),
  # Panel B: wealth stats 1984
  fmt_money0(w_stats_hh$mean[w_stats_hh$year == 1984]),
  fmt_money0(w_stats_hh$med [w_stats_hh$year == 1984]),
  fmt_money0(w_stats_cl$mean[w_stats_cl$year == 1984]),
  fmt_money0(w_stats_cl$med [w_stats_cl$year == 1984]),
  # Panel B: wealth stats 2023
  fmt_money0(w_stats_hh$mean[w_stats_hh$year == 2023]),
  fmt_money0(w_stats_hh$med [w_stats_hh$year == 2023]),
  fmt_money0(w_stats_cl$mean[w_stats_cl$year == 2023]),
  fmt_money0(w_stats_cl$med [w_stats_cl$year == 2023])
)

fig2 <- arrangeGrob(
  textGrob(
    "Figure 2. Lorenz Curves at the Household and Kin Group Levels",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Distribution of income in 1984 and 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig2_inc, ncol = 1, heights = c(1, 12)),
    arrangeGrob(
      textGrob("Panel B: Distribution of wealth in 1984 and 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig2_w, ncol = 1, heights = c(1, 12)),
    ncol = 2),
  ncol = 1, heights = unit(c(0.5, 8.4), "inches"))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure2.pdf"), fig2, width = 14, height = 9)
  message("Saved: figure2.pdf")
}


# ── Figure 3 — Black / Non-Black Median Wealth Ratio ─────────────────────────
fig3_med_plot <- make_ratio_plot(
  wealth_ratios %>% select(year,
                           r_hh_w_ratio = r_hh_w_median_ratio,
                           r_cl_w_ratio = r_cl_w_median_ratio),
  ylab = "Black / Non-Black")

fig3_note <- sprintf(
  paste0(
    "Note: Figure 3 shows the ratio of Black to Non-Black wealth for households and kin groups ",
    "(including home equity), using weighted medians. ",
    "Household estimates are based on %s household-years and kin group estimates are based on ",
    "%s kin group-years across the study period. ",
    "Across all years, weighted median wealth is $%s for Black households ",
    "and $%s for Non-Black households. ",
    "For kin groups, the weighted median wealth is $%s for Black kin groups ",
    "and $%s for Non-Black kin groups. ",
    "The median wealth ratio changed by %.1f%% for households (%.3f in %d to %.3f in %d) ",
    "and increased by %.1f%% for kin groups (%.3f to %.3f). ",
    "On average across all years, the median wealth ratio for kin groups is %.3f higher than for households. ",
    "See the Supplemental Information for mean wealth ratios."
  ),
  fmt0(w_hh_years), fmt0(w_cl_years),
  fmt_money0(w_med_hh$black),    fmt_money0(w_med_hh$nonblack),
  fmt_money0(w_med_cl$black),    fmt_money0(w_med_cl$nonblack),
  w_med_ratio_hh_pct, w_med_ratio_hh_first, w_first_yr, w_med_ratio_hh_last, w_last_yr,
  w_med_ratio_cl_pct, w_med_ratio_cl_first, w_med_ratio_cl_last,
  -avg_w_med_ratio_gap
)

fig3 <- arrangeGrob(
  textGrob(
    "Figure 3. Black / Non-Black Median Wealth Ratio: Households vs. Kin Groups",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)),
  fig3_med_plot,
  ncol = 1, heights = unit(c(0.5, 10.0), "inches"))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure3.pdf"), fig3, width = 14, height = 11)
  message("Saved: figure3.pdf")
}



# -- FIGURE 4 + TABLE 4 — Family Composition Over Time (Household vs. Kin Group)
inc_family_demo <- read_csv(here("5_summary", "output", "inc_family_demo.csv"),
                            show_col_types = FALSE)

shared_y_fig4a <- {
  vals <- c(inc_family_demo$hh_children, inc_family_demo$clan_children_hh_mean)
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 10) / 10, ceiling(max(vals) * 10) / 10)
}

fig4_children <- make_gini_plot(
  inc_family_demo, hh_children, clan_children_hh_mean,
  "Average Number of Children per Household", y_limits = shared_y_fig4a)

shared_y_fig4b <- {
  vals <- c(inc_family_demo$hh_other, inc_family_demo$clan_other_hh_mean)
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 10) / 10, ceiling(max(vals) * 10) / 10)
}

fig4_other <- make_gini_plot(
  inc_family_demo, hh_other, clan_other_hh_mean,
  "Average Number of Other Family Members per Household", y_limits = shared_y_fig4b)

# ── Scalars for the Figure 4 note ─────────────────────────────────────────────
fig4_first_yr <- min(inc_family_demo$year)
fig4_last_yr  <- max(inc_family_demo$year)

hh_child_first <- inc_family_demo$hh_children[inc_family_demo$year == fig4_first_yr]
hh_child_last  <- inc_family_demo$hh_children[inc_family_demo$year == fig4_last_yr]
cl_child_first <- inc_family_demo$clan_children_hh_mean[inc_family_demo$year == fig4_first_yr]
cl_child_last  <- inc_family_demo$clan_children_hh_mean[inc_family_demo$year == fig4_last_yr]

hh_other_first <- inc_family_demo$hh_other[inc_family_demo$year == fig4_first_yr]
hh_other_last  <- inc_family_demo$hh_other[inc_family_demo$year == fig4_last_yr]
cl_other_first <- inc_family_demo$clan_other_hh_mean[inc_family_demo$year == fig4_first_yr]
cl_other_last  <- inc_family_demo$clan_other_hh_mean[inc_family_demo$year == fig4_last_yr]

hh_child_pct <- 100 * (hh_child_last - hh_child_first) / hh_child_first
cl_child_pct <- 100 * (cl_child_last - cl_child_first) / cl_child_first
hh_other_pct <- 100 * (hh_other_last - hh_other_first) / hh_other_first
cl_other_pct <- 100 * (cl_other_last - cl_other_first) / cl_other_first

fig4_note <- sprintf(
  paste0(
    "Note: Figure 4 plots average family composition over time (%d\u2013%d) ",
    "at the household and kin group level, using weighted PSID data from the income sample. ",
    "\u201cKin group\u201d values are the average across households within each kin group. ",
    "Panel A shows the average number of children per household. ",
    "The number of children per household changed by %.1f%% (%.2f in %d to %.2f in %d), ",
    "while the kin group average changed by %.1f%% (%.2f to %.2f). ",
    "Panel B shows the average number of other family members (non-spouse, non-children) per household. ",
    "This changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
    "and by %.1f%% for kin groups (%.2f to %.2f)."
  ),
  fig4_first_yr, fig4_last_yr,
  hh_child_pct, hh_child_first, fig4_first_yr, hh_child_last, fig4_last_yr,
  cl_child_pct, cl_child_first, cl_child_last,
  hh_other_pct, hh_other_first, fig4_first_yr, hh_other_last, fig4_last_yr,
  cl_other_pct, cl_other_first, cl_other_last
)

# ── Assemble (same layout as Figure 1) ────────────────────────────────────────
fig4 <- arrangeGrob(
  textGrob(
    "Figure 4. Family Composition Over Time: Households vs. Kin Groups",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Average number of children per household",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig4_children, ncol = 1, heights = c(1, 12)),
    arrangeGrob(
      textGrob("Panel B: Average number of other family members per household",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig4_other, ncol = 1, heights = c(1, 12)),
    ncol = 2),
  ncol = 1, heights = unit(c(0.5, 9.5), "inches"))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure4.pdf"), fig4, width = 14, height = 11)
  message("Saved: figure4.pdf")
}



# ══════════════════════════════════════════════════════════════════════════
# FIGURE 4 + TABLE 4 — Kin Group Selection Over Time in the PSID
# ══════════════════════════════════════════════════════════════════════════
# Drop-in additions for the figures script:
#   1) Add the two read_csv() calls below near the other read_csv() calls at
#      the top of the script (with inc_by_year, wealth_by_year, etc.).
#   2) Add the Figure 4 / Table 4 code block after the Figure 3 section.
#
# Each panel now has 3 lines: Household (robust), Kin Group (robust), and
# Kin Group (Full Sample) — the last computed on the non-robust "clans" data
# (see build_family_demo.R) to show the selection effect of the robust
# kin-linkage criteria. None of the existing plot_helpers.R functions support
# a 3-series panel (make_gini_plot only takes two), so the panels are built
# directly with ggplot2 below, reusing make_x_breaks() and the existing color
# constants — no new functions are defined.

# ── Data load (place with the other read_csv() calls) ────────────────────────
inc_family_demo <- read_csv(here("5_summary", "output", "inc_family_demo.csv"),
                            show_col_types = FALSE)
inc_family_demo_full <- read_csv(here("5_summary", "output", "inc_family_demo_full.csv"),
                                 show_col_types = FALSE)

# ── Combine robust + full kin group series ────────────────────────────────────
fig4_dat <- inc_family_demo %>%
  dplyr::select(year, hh_children, hh_other,
                clan_children_hh_mean, clan_other_hh_mean) %>%
  dplyr::left_join(
    inc_family_demo_full %>%
      dplyr::select(year,
                    clan_children_hh_mean_full = clan_children_hh_mean,
                    clan_other_hh_mean_full    = clan_other_hh_mean),
    by = "year"
  )

fig4_series_colors <- c(
  "Household"               = NAVY,
  "Kin Group (Robust)"      = PALE_NAVY,
  "Kin Group (Full Sample)" = CORAL)
fig4_series_linetypes <- c(
  "Household"               = "solid",
  "Kin Group (Robust)"      = "dotted",
  "Kin Group (Full Sample)" = "dashed")
fig4_series_levels <- names(fig4_series_colors)

fig4_br <- make_x_breaks(min(fig4_dat$year, na.rm = TRUE), max(fig4_dat$year, na.rm = TRUE))

# ── Panel A: children per household ───────────────────────────────────────────
plot_a_dat <- fig4_dat %>%
  dplyr::transmute(
    year,
    `Household`               = hh_children,
    `Kin Group (Robust)`      = clan_children_hh_mean,
    `Kin Group (Full Sample)` = clan_children_hh_mean_full
  ) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "value") %>%
  dplyr::mutate(Series = factor(Series, levels = fig4_series_levels))

y4a <- {
  vals <- plot_a_dat$value[is.finite(plot_a_dat$value)]
  c(floor(min(vals) * 10) / 10, ceiling(max(vals) * 10) / 10)
}

fig4_children <- ggplot2::ggplot(plot_a_dat,
                                  ggplot2::aes(x = year, y = value,
                                               color = Series, linetype = Series)) +
  ggplot2::geom_line(linewidth = 1.7) +
  ggplot2::scale_color_manual(values = fig4_series_colors, name = NULL) +
  ggplot2::scale_linetype_manual(values = fig4_series_linetypes, name = NULL) +
  ggplot2::scale_y_continuous(limits = y4a) +
  ggplot2::scale_x_continuous(breaks = fig4_br,
                               expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::labs(x = NULL, y = "Avg. Children per Household") +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)))

# ── Panel B: other family members per household ──────────────────────────────
plot_b_dat <- fig4_dat %>%
  dplyr::transmute(
    year,
    `Household`               = hh_other,
    `Kin Group (Robust)`      = clan_other_hh_mean,
    `Kin Group (Full Sample)` = clan_other_hh_mean_full
  ) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "value") %>%
  dplyr::mutate(Series = factor(Series, levels = fig4_series_levels))

y4b <- {
  vals <- plot_b_dat$value[is.finite(plot_b_dat$value)]
  c(floor(min(vals) * 10) / 10, ceiling(max(vals) * 10) / 10)
}

fig4_other <- ggplot2::ggplot(plot_b_dat,
                               ggplot2::aes(x = year, y = value,
                                            color = Series, linetype = Series)) +
  ggplot2::geom_line(linewidth = 1.7) +
  ggplot2::scale_color_manual(values = fig4_series_colors, name = NULL) +
  ggplot2::scale_linetype_manual(values = fig4_series_linetypes, name = NULL) +
  ggplot2::scale_y_continuous(limits = y4b) +
  ggplot2::scale_x_continuous(breaks = fig4_br,
                               expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::labs(x = NULL, y = "Avg. Other Family Members per Household") +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)))

# ── Scalars for the Figure 4 note ─────────────────────────────────────────────
fig4_first_yr <- min(fig4_dat$year)
fig4_last_yr  <- max(fig4_dat$year)

get_fig4 <- function(var, yr) fig4_dat[[var]][fig4_dat$year == yr]

hh_child_first <- get_fig4("hh_children", fig4_first_yr)
hh_child_last  <- get_fig4("hh_children", fig4_last_yr)
cl_child_first <- get_fig4("clan_children_hh_mean", fig4_first_yr)
cl_child_last  <- get_fig4("clan_children_hh_mean", fig4_last_yr)
cl_child_full_first <- get_fig4("clan_children_hh_mean_full", fig4_first_yr)
cl_child_full_last  <- get_fig4("clan_children_hh_mean_full", fig4_last_yr)

hh_other_first <- get_fig4("hh_other", fig4_first_yr)
hh_other_last  <- get_fig4("hh_other", fig4_last_yr)
cl_other_first <- get_fig4("clan_other_hh_mean", fig4_first_yr)
cl_other_last  <- get_fig4("clan_other_hh_mean", fig4_last_yr)
cl_other_full_first <- get_fig4("clan_other_hh_mean_full", fig4_first_yr)
cl_other_full_last  <- get_fig4("clan_other_hh_mean_full", fig4_last_yr)

pct_chg <- function(first, last) 100 * (last - first) / first

hh_child_pct      <- pct_chg(hh_child_first, hh_child_last)
cl_child_pct      <- pct_chg(cl_child_first, cl_child_last)
cl_child_full_pct <- pct_chg(cl_child_full_first, cl_child_full_last)
hh_other_pct      <- pct_chg(hh_other_first, hh_other_last)
cl_other_pct      <- pct_chg(cl_other_first, cl_other_last)
cl_other_full_pct <- pct_chg(cl_other_full_first, cl_other_full_last)

# Average selection gap: robust kin group minus full-sample kin group
avg_child_selection_gap <- round(mean(fig4_dat$clan_children_hh_mean -
                                       fig4_dat$clan_children_hh_mean_full, na.rm = TRUE), 3)
avg_other_selection_gap <- round(mean(fig4_dat$clan_other_hh_mean -
                                       fig4_dat$clan_other_hh_mean_full, na.rm = TRUE), 3)

fig4_note <- sprintf(
  paste0(
    "Note: Figure 4 plots average family composition over time (%d\u2013%d) ",
    "for households, robust (kin-linked) kin groups, and the full, non-robust ",
    "sample of kin groups, using weighted PSID data from the income sample. ",
    "\u201cKin Group (Robust)\u201d and \u201cKin Group (Full Sample)\u201d values are ",
    "the average across households within each kin group, before and after ",
    "applying the robust kin-linkage criteria, respectively. ",
    "Panel A shows the average number of children per household. ",
    "This changed by %.1f%% for households (%.2f in %d to %.2f in %d), ",
    "by %.1f%% for robust kin groups (%.2f to %.2f), ",
    "and by %.1f%% for the full sample of kin groups (%.2f to %.2f). ",
    "On average, robust kin groups have %.3f more children per household than the full sample. ",
    "Panel B shows the average number of other family members (non-spouse, non-children) per household. ",
    "This changed by %.1f%% for households (%.2f in %d to %.2f in %d), ",
    "by %.1f%% for robust kin groups (%.2f to %.2f), ",
    "and by %.1f%% for the full sample of kin groups (%.2f to %.2f). ",
    "On average, robust kin groups have %.3f more other family members per household than the full sample."
  ),
  fig4_first_yr, fig4_last_yr,
  hh_child_pct, hh_child_first, fig4_first_yr, hh_child_last, fig4_last_yr,
  cl_child_pct, cl_child_first, cl_child_last,
  cl_child_full_pct, cl_child_full_first, cl_child_full_last,
  avg_child_selection_gap,
  hh_other_pct, hh_other_first, fig4_first_yr, hh_other_last, fig4_last_yr,
  cl_other_pct, cl_other_first, cl_other_last,
  cl_other_full_pct, cl_other_full_first, cl_other_full_last,
  avg_other_selection_gap
)

# ── Assemble (same layout as Figure 1) ────────────────────────────────────────
fig4 <- arrangeGrob(
  textGrob(
    "Figure 4. Kin Group Selection Over Time in the PSID",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)),
  arrangeGrob(
    arrangeGrob(
      textGrob("A: Avg. children per household",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig4_children, ncol = 1, heights = c(1, 12)),
    arrangeGrob(
      textGrob("B: Avg. other family members per household",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig4_other, ncol = 1, heights = c(1, 12)),
    ncol = 2),
  ncol = 1, heights = unit(c(0.5, 9.5), "inches"))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure4.pdf"), fig4, width = 14, height = 11)
  message("Saved: figure4.pdf")
}


# ══════════════════════════════════════════════════════════════════════════
# FIGURE 4 — Kin Group Selection Over Time in the PSID
# ══════════════════════════════════════════════════════════════════════════
# Drop-in additions for the figures script:
#   0) Add `library(patchwork)` near the top of the script with the other
#      library() calls (install.packages("patchwork") if not already
#      installed). This is a new dependency, needed only for combining the
#      panels with a single shared legend below.
#   1) Add the two read_csv() calls below near the other read_csv() calls at
#      the top of the script (with inc_by_year, wealth_by_year, etc.).
#   2) Add the Figure 4 / Figure 5 code blocks after the Figure 3 section.
#
# Each panel has 3 lines: Households, Kin Groups (excl. single-HH groups),
# and Kin Groups (all) — the last computed on the non-robust "clans" data
# (see build_family_demo.R). None of plot_helpers.R's functions support a
# 3-series single panel, so panels are built directly with ggplot2 here,
# reusing make_x_breaks() and the existing color constants.
#
# NOTE ON THE LEGEND: earlier versions of this figure tried to extract a
# single shared legend manually (via cowplot::get_legend() and then via a
# hand-rolled gtable search for a "guide-box" grob). Both approaches broke
# because ggplot2's internal legend-grob naming has changed across versions
# and isn't stable to rely on. patchwork's plot_layout(guides = "collect")
# is the maintained, version-robust way to combine multiple ggplots into one
# figure with a single de-duplicated legend, so the whole assembly below
# uses patchwork instead of gridExtra::arrangeGrob for Figure 4 and Figure 5.

# ── Data load (place with the other read_csv() calls) ────────────────────────
inc_family_demo <- read_csv(here("5_summary", "output", "inc_family_demo.csv"),
                            show_col_types = FALSE)
inc_family_demo_full <- read_csv(here("5_summary", "output", "inc_family_demo_full.csv"),
                                 show_col_types = FALSE)

# ── Combine robust + full kin group series, excluding 2023 from the x-axis ───
fig4_dat <- inc_family_demo %>%
  dplyr::select(year, hh_children, hh_other,
                clan_children_hh_mean, clan_other_hh_mean) %>%
  dplyr::left_join(
    inc_family_demo_full %>%
      dplyr::select(year,
                    clan_children_hh_mean_full = clan_children_hh_mean,
                    clan_other_hh_mean_full    = clan_other_hh_mean),
    by = "year"
  ) %>%
  dplyr::filter(year != 2023)

fig4_series_colors <- c(
  "Households"                           = NAVY,
  "Kin Groups (excl. single-HH groups)"  = PALE_NAVY,
  "Kin Groups (all)"                     = CORAL)
fig4_series_linetypes <- c(
  "Households"                           = "solid",
  "Kin Groups (excl. single-HH groups)"  = "dotted",
  "Kin Groups (all)"                     = "dashed")
fig4_series_levels <- names(fig4_series_colors)

fig4_br <- make_x_breaks(min(fig4_dat$year, na.rm = TRUE), max(fig4_dat$year, na.rm = TRUE))

# ── Panel A: children per household ───────────────────────────────────────────
plot_a_dat <- fig4_dat %>%
  dplyr::transmute(
    year,
    `Households`                          = hh_children,
    `Kin Groups (excl. single-HH groups)` = clan_children_hh_mean,
    `Kin Groups (all)`                    = clan_children_hh_mean_full
  ) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "value") %>%
  dplyr::mutate(Series = factor(Series, levels = fig4_series_levels))

y4a <- {
  vals <- plot_a_dat$value[is.finite(plot_a_dat$value)]
  c(floor(min(vals) * 10) / 10, ceiling(max(vals) * 10) / 10)
}

fig4_children <- ggplot2::ggplot(plot_a_dat,
                                  ggplot2::aes(x = year, y = value,
                                               color = Series, linetype = Series)) +
  ggplot2::geom_line(linewidth = 1.7) +
  ggplot2::scale_color_manual(values = fig4_series_colors, name = NULL) +
  ggplot2::scale_linetype_manual(values = fig4_series_linetypes, name = NULL) +
  ggplot2::scale_y_continuous(limits = y4a) +
  ggplot2::scale_x_continuous(breaks = fig4_br,
                               expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::labs(x = NULL, y = "Avg. Children per Household",
                title = "A: Avg. children per household") +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(family = base_family, size = sub_size,
                                             hjust = 0, face = "plain"),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1))

# ── Panel B: other family members per household ──────────────────────────────
plot_b_dat <- fig4_dat %>%
  dplyr::transmute(
    year,
    `Households`                          = hh_other,
    `Kin Groups (excl. single-HH groups)` = clan_other_hh_mean,
    `Kin Groups (all)`                    = clan_other_hh_mean_full
  ) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "value") %>%
  dplyr::mutate(Series = factor(Series, levels = fig4_series_levels))

y4b <- {
  vals <- plot_b_dat$value[is.finite(plot_b_dat$value)]
  c(floor(min(vals) * 10) / 10, ceiling(max(vals) * 10) / 10)
}

fig4_other <- ggplot2::ggplot(plot_b_dat,
                               ggplot2::aes(x = year, y = value,
                                            color = Series, linetype = Series)) +
  ggplot2::geom_line(linewidth = 1.7) +
  ggplot2::scale_color_manual(values = fig4_series_colors, name = NULL) +
  ggplot2::scale_linetype_manual(values = fig4_series_linetypes, name = NULL) +
  ggplot2::scale_y_continuous(limits = y4b) +
  ggplot2::scale_x_continuous(breaks = fig4_br,
                               expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::labs(x = NULL, y = "Avg. Other Family Members per Household",
                title = "B: Avg. other family members per household") +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(family = base_family, size = sub_size,
                                             hjust = 0, face = "plain"),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1))

# ── Combine with a single shared legend via patchwork ─────────────────────────
fig4_body <- (fig4_children | fig4_other) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

fig4 <- fig4_body +
  patchwork::plot_annotation(
    title = "Figure 4. Kin Group Selection Over Time in the PSID",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(family = base_family, face = "bold",
                                         size = title_size, hjust = 0)))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure4.pdf"), fig4, width = 14, height = 11)
  message("Saved: figure4.pdf")
}


# ── Scalars for the Figure 4 note ─────────────────────────────────────────────
fig4_first_yr <- min(fig4_dat$year)
fig4_last_yr  <- max(fig4_dat$year)

get_fig4 <- function(var, yr) fig4_dat[[var]][fig4_dat$year == yr]

hh_child_first <- get_fig4("hh_children", fig4_first_yr)
hh_child_last  <- get_fig4("hh_children", fig4_last_yr)
cl_child_first <- get_fig4("clan_children_hh_mean", fig4_first_yr)
cl_child_last  <- get_fig4("clan_children_hh_mean", fig4_last_yr)
cl_child_full_first <- get_fig4("clan_children_hh_mean_full", fig4_first_yr)
cl_child_full_last  <- get_fig4("clan_children_hh_mean_full", fig4_last_yr)

hh_other_first <- get_fig4("hh_other", fig4_first_yr)
hh_other_last  <- get_fig4("hh_other", fig4_last_yr)
cl_other_first <- get_fig4("clan_other_hh_mean", fig4_first_yr)
cl_other_last  <- get_fig4("clan_other_hh_mean", fig4_last_yr)
cl_other_full_first <- get_fig4("clan_other_hh_mean_full", fig4_first_yr)
cl_other_full_last  <- get_fig4("clan_other_hh_mean_full", fig4_last_yr)

pct_chg <- function(first, last) 100 * (last - first) / first

hh_child_pct      <- pct_chg(hh_child_first, hh_child_last)
cl_child_pct      <- pct_chg(cl_child_first, cl_child_last)
cl_child_full_pct <- pct_chg(cl_child_full_first, cl_child_full_last)
hh_other_pct      <- pct_chg(hh_other_first, hh_other_last)
cl_other_pct      <- pct_chg(cl_other_first, cl_other_last)
cl_other_full_pct <- pct_chg(cl_other_full_first, cl_other_full_last)

avg_child_selection_gap <- round(mean(fig4_dat$clan_children_hh_mean -
                                       fig4_dat$clan_children_hh_mean_full, na.rm = TRUE), 3)
avg_other_selection_gap <- round(mean(fig4_dat$clan_other_hh_mean -
                                       fig4_dat$clan_other_hh_mean_full, na.rm = TRUE), 3)

fig4_note <- sprintf(
  paste0(
    "Note: Figure 4 plots average family composition over time (%d\u2013%d; 2023 excluded) ",
    "for households, robust (kin-linked) kin groups, and the full, non-robust ",
    "sample of kin groups, using weighted PSID data from the income sample. ",
    "\u201cKin Groups (excl. single-HH groups)\u201d and \u201cKin Groups (all)\u201d values are ",
    "the average across households within each kin group, before and after ",
    "applying the robust kin-linkage criteria, respectively. ",
    "Panel A shows the average number of children per household. ",
    "This changed by %.1f%% for households (%.2f in %d to %.2f in %d), ",
    "by %.1f%% for robust kin groups (%.2f to %.2f), ",
    "and by %.1f%% for the full sample of kin groups (%.2f to %.2f). ",
    "On average, robust kin groups have %.3f more children per household than the full sample. ",
    "Panel B shows the average number of other family members (non-spouse, non-children) per household. ",
    "This changed by %.1f%% for households (%.2f in %d to %.2f in %d), ",
    "by %.1f%% for robust kin groups (%.2f to %.2f), ",
    "and by %.1f%% for the full sample of kin groups (%.2f to %.2f). ",
    "On average, robust kin groups have %.3f more other family members per household than the full sample."
  ),
  fig4_first_yr, fig4_last_yr,
  hh_child_pct, hh_child_first, fig4_first_yr, hh_child_last, fig4_last_yr,
  cl_child_pct, cl_child_first, cl_child_last,
  cl_child_full_pct, cl_child_full_first, cl_child_full_last,
  avg_child_selection_gap,
  hh_other_pct, hh_other_first, fig4_first_yr, hh_other_last, fig4_last_yr,
  cl_other_pct, cl_other_first, cl_other_last,
  cl_other_full_pct, cl_other_full_first, cl_other_full_last,
  avg_other_selection_gap
)


# ══════════════════════════════════════════════════════════════════════════
# FIGURE 5 — Kin Group Size and Age Over Time (Robust vs. Full Sample)
# ══════════════════════════════════════════════════════════════════════════
# clan_age_mean, numclan, and num_clan_people have no household-level
# counterpart, so this figure compares only the robust and non-robust (full)
# kin group samples over time — 3 panels in a single row, one shared legend
# via patchwork, 2023 excluded to match Figure 4.

fig5_dat <- inc_family_demo %>%
  dplyr::select(year, clan_age_mean, numclan, num_clan_people) %>%
  dplyr::left_join(
    inc_family_demo_full %>%
      dplyr::select(year,
                    clan_age_mean_full    = clan_age_mean,
                    numclan_full          = numclan,
                    num_clan_people_full  = num_clan_people),
    by = "year"
  ) %>%
  dplyr::filter(year != 2023)

fig5_series_colors <- c(
  "Kin Groups (excl. single-HH groups)" = NAVY,
  "Kin Groups (all)"                    = CORAL)
fig5_series_linetypes <- c(
  "Kin Groups (excl. single-HH groups)" = "solid",
  "Kin Groups (all)"                    = "dashed")
fig5_series_levels <- names(fig5_series_colors)

fig5_br <- make_x_breaks(min(fig5_dat$year, na.rm = TRUE), max(fig5_dat$year, na.rm = TRUE))

fig5_base_theme <- ggplot2::theme(
  plot.subtitle = ggplot2::element_text(size = sub_size * 0.6, hjust = 0.5),
  axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1))

# Panel A: average age of kin group members
pA_dat <- fig5_dat %>%
  dplyr::transmute(year,
                   `Kin Groups (excl. single-HH groups)` = clan_age_mean,
                   `Kin Groups (all)`                    = clan_age_mean_full) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "value") %>%
  dplyr::mutate(Series = factor(Series, levels = fig5_series_levels))

fig5_A <- ggplot2::ggplot(pA_dat, ggplot2::aes(x = year, y = value,
                                                color = Series, linetype = Series)) +
  ggplot2::geom_line(linewidth = 1.7) +
  ggplot2::scale_color_manual(values = fig5_series_colors, name = NULL) +
  ggplot2::scale_linetype_manual(values = fig5_series_linetypes, name = NULL) +
  ggplot2::scale_x_continuous(breaks = fig5_br,
                               expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::labs(x = NULL, y = "Avg. Age of Kin Group Members",
                subtitle = "A: Avg. Age of Kin Group Members") +
  fig5_base_theme

# Panel B: average households per kin group
pB_dat <- fig5_dat %>%
  dplyr::transmute(year,
                   `Kin Groups (excl. single-HH groups)` = numclan,
                   `Kin Groups (all)`                    = numclan_full) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "value") %>%
  dplyr::mutate(Series = factor(Series, levels = fig5_series_levels))

fig5_B <- ggplot2::ggplot(pB_dat, ggplot2::aes(x = year, y = value,
                                                color = Series, linetype = Series)) +
  ggplot2::geom_line(linewidth = 1.7) +
  ggplot2::scale_color_manual(values = fig5_series_colors, name = NULL) +
  ggplot2::scale_linetype_manual(values = fig5_series_linetypes, name = NULL) +
  ggplot2::scale_x_continuous(breaks = fig5_br,
                               expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::labs(x = NULL, y = "Avg. Households per Kin Group",
                subtitle = "B: Avg. Households per Kin Group") +
  fig5_base_theme

# Panel C: average people per kin group
pC_dat <- fig5_dat %>%
  dplyr::transmute(year,
                   `Kin Groups (excl. single-HH groups)` = num_clan_people,
                   `Kin Groups (all)`                    = num_clan_people_full) %>%
  tidyr::pivot_longer(-year, names_to = "Series", values_to = "value") %>%
  dplyr::mutate(Series = factor(Series, levels = fig5_series_levels))

fig5_C <- ggplot2::ggplot(pC_dat, ggplot2::aes(x = year, y = value,
                                                color = Series, linetype = Series)) +
  ggplot2::geom_line(linewidth = 1.7) +
  ggplot2::scale_color_manual(values = fig5_series_colors, name = NULL) +
  ggplot2::scale_linetype_manual(values = fig5_series_linetypes, name = NULL) +
  ggplot2::scale_x_continuous(breaks = fig5_br,
                               expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
  ggplot2::labs(x = NULL, y = "Avg. People per Kin Group",
                subtitle = "C: Avg. People per Kin Group") +
  fig5_base_theme

# ── Combine with a single shared legend via patchwork ─────────────────────────
fig5_body <- (fig5_A | fig5_B | fig5_C) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

fig5 <- fig5_body +
  patchwork::plot_annotation(
    title = "Figure 5. Kin Group Size and Age Over Time",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(family = base_family, face = "bold",
                                         size = title_size, hjust = 0)))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure5.pdf"), fig5, width = 16, height = 6)
  message("Saved: figure5.pdf")
}

fig5_note <- sprintf(
  paste0(
    "Note: Figure 5 plots kin group size and age over time (%d\u2013%d; 2023 excluded) ",
    "for the robust (kin-linked) sample and the full, non-robust sample of kin groups, ",
    "using weighted PSID data from the income sample. ",
    "Panel A shows the average age of kin group members. ",
    "Panel B shows the average number of households per kin group. ",
    "Panel C shows the average number of people per kin group. ",
    "In all three panels, robust kin groups differ from the full sample of kin groups ",
    "because the robust sample is restricted to kin groups meeting the study's ",
    "kin-linkage criteria."
  ),
  min(fig5_dat$year, na.rm = TRUE), max(fig5_dat$year, na.rm = TRUE)
)