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

# ── Figure 1 — Gini over time ─────────────────────────────────────────────────
# Income years shown as survey years (1969-2023); lag removed.

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
    "Note: Gini coefficients estimated from weighted PSID data. ",
    "Weighted mean income: $%s (households), $%s (kin groups). ",
    "Weighted mean wealth: $%s (households), $%s (kin groups; includes home equity). ",
    "Income Ginis changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
    "and by %.1f%% for kin groups (%.2f to %.2f). ",
    "Wealth Ginis changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
    "and by %.1f%% for kin groups (%.2f to %.2f). ",
    "On average across all years, the income Gini is %.3f higher for households than kin groups; ",
    "the wealth Gini is %.3f higher. "
  ),
  fmt_money0(inc_mean_hh_w), fmt_money0(inc_mean_cl_w),
  fmt_money0(w_mean_hh_w),   fmt_money0(w_mean_cl_w),
  inc_hh_pct, inc_hh_1969, 1969L, inc_hh_2023, 2023L,
  inc_cl_pct, inc_cl_1969, inc_cl_2023,
  w_hh_pct,   w_hh_1984,  1984L, w_hh_2023,  2023L,
  w_cl_pct,   w_cl_1984,  w_cl_2023,
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
    "Note: Lorenz curves are estimated from weighted data using PSID family and ",
    "kin group weights. Wealth includes home equity. ",
    "Income in 1984: weighted mean (median) $%s ($%s) for households, $%s ($%s) for kin groups. ",
    "Income in 2023: $%s ($%s) for households, $%s ($%s) for kin groups."
  ),
  fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 1984]),
  fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 1984]),
  fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 1984]),
  fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 1984]),
  fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 2023]),
  fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 2023]),
  fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 2023]),
  fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 2023]))

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
    "Note: Lines show the ratio of Black to Non-Black wealth for households and kin groups ",
    "(includes home equity), using weighted medians. ",
    "Across all years, weighted median wealth is $%s for Black households and $%s for Non-Black households. ",
    "For kin groups, the weighted median wealth is $%s for Black kin groups and $%s for Non-Black kin groups. ",
    "Median ratio changed by %.1f%% for households (%.3f in %d to %.3f in %d) ",
    "and %.1f%% for kin groups (%.3f to %.3f). ",
    "On average across all years, the median wealth ratio for kin groups is %.3f higher than for households. ",
    "See Appendix E for mean wealth ratios."
  ),
  fmt_money0(w_med_hh$black),    fmt_money0(w_med_hh$nonblack),
  fmt_money0(w_med_cl$black),    fmt_money0(w_med_cl$nonblack),
  w_med_ratio_hh_pct, w_med_ratio_hh_first, w_first_yr, w_med_ratio_hh_last, w_last_yr,
  w_med_ratio_cl_pct, w_med_ratio_cl_first, w_med_ratio_cl_last,
  -avg_w_med_ratio_gap)

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




