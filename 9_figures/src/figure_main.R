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

ORANGE      <- "#E66101"
PALE_ORANGE <- "#FDB863"
BLUE        <- "#4a71c7"
PALE_BLUE   <- "#92aee0"
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
inc_ratios     <- read_csv(here("7_gini_by_race",    "output", "income_race_ratios.csv"),
                           show_col_types = FALSE)
wealth_ratios  <- read_csv(here("7_gini_by_race",    "output", "wealth_withhome_race_ratios.csv"),
                           show_col_types = FALSE)


# Sample size scalars
n_mismatched   <- nrow(mismatched)
inc_uniq_clans <- n_distinct(r_clans$id1968)
w_uniq_clans   <- n_distinct(r_clans_wealth$id1968)
inc_hh_years <- nrow(r_hh)
inc_cl_years <- nrow(r_clans)
w_hh_years   <- nrow(r_hh_wealth)
w_cl_years   <- nrow(r_clans_wealth)


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

# Race ratio scalars
inc_ratio_hh_first <- inc_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_hh_w_ratio)
inc_ratio_hh_last  <- inc_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_hh_w_ratio)
inc_ratio_cl_first <- inc_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_cl_w_ratio)
inc_ratio_cl_last  <- inc_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_cl_w_ratio)

w_ratio_hh_first <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_hh_w_ratio)
w_ratio_hh_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_hh_w_ratio)
w_ratio_cl_first <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(r_cl_w_ratio)
w_ratio_cl_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_cl_w_ratio)

inc_ratio_hh_pct  <- 100 * (inc_ratio_hh_last - inc_ratio_hh_first) / abs(inc_ratio_hh_first)
inc_ratio_cl_pct  <- 100 * (inc_ratio_cl_last - inc_ratio_cl_first) / abs(inc_ratio_cl_first)
w_ratio_hh_pct    <- 100 * (w_ratio_hh_last   - w_ratio_hh_first)   / abs(w_ratio_hh_first)
w_ratio_cl_pct    <- 100 * (w_ratio_cl_last   - w_ratio_cl_first)   / abs(w_ratio_cl_first)

avg_inc_ratio_gap <- round(mean(inc_ratios$r_hh_w_ratio - inc_ratios$r_cl_w_ratio,
                                na.rm = TRUE), 3)
avg_w_ratio_gap   <- round(mean(wealth_ratios$r_hh_w_ratio - wealth_ratios$r_cl_w_ratio,
                                na.rm = TRUE), 3)

# Race-stratified weighted means (pooled across all years, size-standardised)
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

inc_race_hh <- .race_means(r_hh    %>% dplyr::mutate(inc_all = inc_all / numfu),
                            "inc_all", "fam_weight",  "black_head")
inc_race_cl <- .race_means(r_clans %>% dplyr::mutate(inc_all = inc_all / numclan),
                            "inc_all", "clan_weight", "black_clan")
w_race_hh   <- .race_means(r_hh_wealth    %>% dplyr::mutate(wealth = wealth / numfu),
                            "wealth", "fam_weight",  "black_head")
w_race_cl   <- .race_means(r_clans_wealth %>% dplyr::mutate(wealth = wealth / numclan),
                            "wealth", "clan_weight", "black_clan")

inc_first_yr <- inc_ratios    %>% arrange(year) %>% slice(1)   %>% pull(year)
inc_last_yr  <- inc_ratios    %>% arrange(year) %>% slice(n()) %>% pull(year)
w_first_yr   <- wealth_ratios %>% arrange(year) %>% slice(1)   %>% pull(year)
w_last_yr    <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(year)


# ── Figure 1 — Gini over time ─────────────────────────────────────────────────
shared_y_fig1 <- {
  vals <- c(
    inc_by_year    %>% filter(year != "ALL") %>% pull(r_hh_w_inc),
    inc_by_year    %>% filter(year != "ALL") %>% pull(r_cl_w_inc),
    wealth_by_year %>% filter(year != "ALL") %>% pull(r_hh_w_wealth),
    wealth_by_year %>% filter(year != "ALL") %>% pull(r_cl_w_wealth)
  )
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

fig1_inc <- make_gini_plot(inc_by_year,    r_hh_w_inc,    r_cl_w_inc,
                           "Gini Coefficient", y_limits = shared_y_fig1)
fig1_w   <- make_gini_plot(wealth_by_year, r_hh_w_wealth, r_cl_w_wealth,
                           "Gini Coefficient", y_limits = shared_y_fig1)

fig1_note <- sprintf(
  paste0(
    "Note: Gini coefficients estimated from weighted PSID data. ",
    "Weighted mean income: $%s (households), $%s (clans). ",
    "Weighted mean wealth: $%s (households), $%s (clans; includes home equity). ",
    "Income Gini changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
    "and by %.1f%% for clans (%.2f to %.2f). ",
    "Wealth Gini changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
    "and by %.1f%% for clans (%.2f to %.2f). ",
    "On average across all years, the income Gini is %.3f higher for households than clans; ",
    "the wealth Gini is %.3f higher. ",
    "Solid lines = households; dotted lines = clans."
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
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
  ),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Income inequality from 1969 to 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig1_inc, ncol = 1, heights = c(1, 12)
    ),
    arrangeGrob(
      textGrob("Panel B: Wealth inequality from 1984 to 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig1_w, ncol = 1, heights = c(1, 12)
    ),
    ncol = 2
  ),
  ncol = 1, heights = unit(c(0.5, 9.5), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure1.pdf"), fig1, width = 14, height = 11)
  message("Saved: figure1.pdf")
}


# ── Figure 2 — Lorenz curves ──────────────────────────────────────────────────
fig2_inc <- make_lorenz_plot(
  r_hh, r_clans, "inc_all", years = c(1984, 2023),
  ylab   = "Cumulative Proportion of Income",
  colors = c("1984" = BLUE, "2023" = ORANGE)
)
fig2_w <- make_lorenz_plot(
  r_hh_wealth, r_clans_wealth, "wealth", years = c(1984, 2023),
  ylab   = "Cumulative Proportion of Wealth",
  colors = c("1984" = BLUE, "2023" = ORANGE)
)

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
    "clan weights. Wealth includes home equity. Solid lines = households; dotted lines = clans. ",
    "Income in 1984: weighted mean (median) $%s ($%s) for households, $%s ($%s) for clans. ",
    "Income in 2023: $%s ($%s) for households, $%s ($%s) for clans."
  ),
  fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 1984]),
  fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 1984]),
  fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 1984]),
  fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 1984]),
  fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 2023]),
  fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 2023]),
  fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 2023]),
  fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 2023])
)

fig2 <- arrangeGrob(
  textGrob(
    "Figure 2. Lorenz Curves at the Household and Clan Levels",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
  ),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Distribution of income in 1984 and 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig2_inc, ncol = 1, heights = c(1, 12)
    ),
    arrangeGrob(
      textGrob("Panel B: Distribution of wealth in 1984 and 2023",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig2_w, ncol = 1, heights = c(1, 12)
    ),
    ncol = 2
  ),
  ncol = 1, heights = unit(c(0.5, 8.4), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure2.pdf"), fig2, width = 14, height = 9)
  message("Saved: figure2.pdf")
}


# ── Figure 3 — Black / Non-Black Mean Ratios ──────────────────────────────────
all_ratio_vals <- c(inc_ratios$r_hh_w_ratio,   inc_ratios$r_cl_w_ratio,
                    wealth_ratios$r_hh_w_ratio, wealth_ratios$r_cl_w_ratio)
all_ratio_vals  <- all_ratio_vals[is.finite(all_ratio_vals)]
shared_y_ratios <- c(floor(min(all_ratio_vals) * 20) / 20,
                     ceiling(max(all_ratio_vals) * 20) / 20 + 0.05)

fig3_inc <- make_ratio_plot(
  inc_ratios,
  ylab     = "Black / Non-Black\nMean Income Ratio",
  y_limits = shared_y_ratios
)
fig3_w <- make_ratio_plot(
  wealth_ratios,
  ylab     = "Black / Non-Black\nMean Wealth Ratio",
  y_limits = shared_y_ratios
)

fig3_note <- paste0(
  sprintf(
    paste0(
      "Note: Lines show the ratio of weighted mean income (Panel A) or wealth (Panel B) ",
      "for Black relative to Non-Black households and clans. ",
      "Weighted mean income for Black HH is $%s, $%s for Non-Black HH, $%s for Black clans, and $%s for Non-Black clans. ",
      "The income ratio changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
      "and %.1f%% for clans (%.2f to %.2f). "
    ),
    fmt_money0(inc_race_hh$black), fmt_money0(inc_race_hh$nonblack),
    fmt_money0(inc_race_cl$black), fmt_money0(inc_race_cl$nonblack),
    inc_ratio_hh_pct, inc_ratio_hh_first, inc_first_yr, inc_ratio_hh_last, inc_last_yr,
    inc_ratio_cl_pct, inc_ratio_cl_first, inc_ratio_cl_last
  ),
  sprintf(
    paste0(
      "Weighted mean wealth for Black HH is $%s, $%s for Non-Black HH, $%s for Black clans, and $%s for Non-Black clans (includes home equity). ",
      "The wealth ratio changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
      "and %.1f%% for clans (%.2f to %.2f). ",
      "On average across all years, the income ratio is %.3f lower for households than clans, and ",
      "the wealth ratio is %.3f higher for households than clans. ",
      "Solid lines = households; dotted lines = clans."
    ),
    fmt_money0(w_race_hh$black), fmt_money0(w_race_hh$nonblack),
    fmt_money0(w_race_cl$black), fmt_money0(w_race_cl$nonblack),
    w_ratio_hh_pct, w_ratio_hh_first, w_first_yr, w_ratio_hh_last, w_last_yr,
    w_ratio_cl_pct, w_ratio_cl_first, w_ratio_cl_last,
    avg_inc_ratio_gap, avg_w_ratio_gap
  )
)

fig3 <- arrangeGrob(
  textGrob(
    "Figure 3. Black / Non-Black Mean Ratios: Households vs. Clans",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
  ),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Income",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig3_inc, ncol = 1, heights = c(1, 12)
    ),
    arrangeGrob(
      textGrob("Panel B: Wealth",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig3_w, ncol = 1, heights = c(1, 12)
    ),
    ncol = 2
  ),
  ncol = 1, heights = unit(c(0.5, 10.0), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure3.pdf"), fig3, width = 14, height = 11)
  message("Saved: figure3.pdf")
}


# ── Figure 4 — Black / Non-Black Median Ratios Over Time ─────────────────────

.median_ratio_by_year <- function(df, value_var, weight_var, race_var) {
  df %>%
    dplyr::filter(is.finite(.data[[value_var]]),
                  is.finite(.data[[weight_var]]),
                  .data[[weight_var]] > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      med_black    = wtd_median(
        .data[[value_var]][.data[[race_var]] == 1],
        .data[[weight_var]][.data[[race_var]] == 1]),
      med_nonblack = wtd_median(
        .data[[value_var]][.data[[race_var]] == 0],
        .data[[weight_var]][.data[[race_var]] == 0]),
      .groups = "drop"
    ) %>%
    dplyr::mutate(r_hh_w_ratio = med_black / med_nonblack)
}

fig4_inc_hh <- .median_ratio_by_year(
  r_hh    %>% dplyr::mutate(inc_all = inc_all / numfu),
  "inc_all", "fam_weight", "black_head"
) %>% dplyr::mutate(unit = "Household")

fig4_inc_cl <- .median_ratio_by_year(
  r_clans %>% dplyr::mutate(inc_all = inc_all / numclan),
  "inc_all", "clan_weight", "black_clan"
) %>% dplyr::rename(r_cl_w_ratio = r_hh_w_ratio) %>%
  dplyr::mutate(unit = "Clan")

fig4_inc_dat <- dplyr::full_join(
  fig4_inc_hh %>% dplyr::select(year, r_hh_w_ratio),
  fig4_inc_cl %>% dplyr::select(year, r_cl_w_ratio),
  by = "year"
)

fig4_w_hh <- .median_ratio_by_year(
  r_hh_wealth    %>% dplyr::mutate(wealth = wealth / numfu),
  "wealth", "fam_weight", "black_head"
) %>% dplyr::mutate(unit = "Household")

fig4_w_cl <- .median_ratio_by_year(
  r_clans_wealth %>% dplyr::mutate(wealth = wealth / numclan),
  "wealth", "clan_weight", "black_clan"
) %>% dplyr::rename(r_cl_w_ratio = r_hh_w_ratio) %>%
  dplyr::mutate(unit = "Clan")

fig4_w_dat <- dplyr::full_join(
  fig4_w_hh %>% dplyr::select(year, r_hh_w_ratio),
  fig4_w_cl %>% dplyr::select(year, r_cl_w_ratio),
  by = "year"
)

# Shared y limits
fig4_all_vals <- c(fig4_inc_dat$r_hh_w_ratio, fig4_inc_dat$r_cl_w_ratio,
                   fig4_w_dat$r_hh_w_ratio,   fig4_w_dat$r_cl_w_ratio)
fig4_all_vals <- fig4_all_vals[is.finite(fig4_all_vals)]
shared_y_fig4 <- c(floor(min(fig4_all_vals) * 20) / 20,
                   ceiling(max(fig4_all_vals) * 20) / 20 + 0.05)

fig4_inc_plot <- make_ratio_plot(
  fig4_inc_dat,
  ylab     = "Black / Non-Black\nMedian Income Ratio",
  y_limits = shared_y_fig4
)
fig4_w_plot <- make_ratio_plot(
  fig4_w_dat,
  ylab     = "Black / Non-Black\nMedian Wealth Ratio",
  y_limits = shared_y_fig4
)

# Scalars for fig4 note
inc4_hh_first <- fig4_inc_dat %>% dplyr::arrange(year) %>% dplyr::slice(1)   %>% dplyr::pull(r_hh_w_ratio)
inc4_hh_last  <- fig4_inc_dat %>% dplyr::arrange(year) %>% dplyr::slice(n()) %>% dplyr::pull(r_hh_w_ratio)
inc4_cl_first <- fig4_inc_dat %>% dplyr::arrange(year) %>% dplyr::slice(1)   %>% dplyr::pull(r_cl_w_ratio)
inc4_cl_last  <- fig4_inc_dat %>% dplyr::arrange(year) %>% dplyr::slice(n()) %>% dplyr::pull(r_cl_w_ratio)
w4_hh_first   <- fig4_w_dat   %>% dplyr::arrange(year) %>% dplyr::slice(1)   %>% dplyr::pull(r_hh_w_ratio)
w4_hh_last    <- fig4_w_dat   %>% dplyr::arrange(year) %>% dplyr::slice(n()) %>% dplyr::pull(r_hh_w_ratio)
w4_cl_first   <- fig4_w_dat   %>% dplyr::arrange(year) %>% dplyr::slice(1)   %>% dplyr::pull(r_cl_w_ratio)
w4_cl_last    <- fig4_w_dat   %>% dplyr::arrange(year) %>% dplyr::slice(n()) %>% dplyr::pull(r_cl_w_ratio)

inc4_first_yr <- fig4_inc_dat %>% dplyr::arrange(year) %>% dplyr::slice(1)   %>% dplyr::pull(year)
inc4_last_yr  <- fig4_inc_dat %>% dplyr::arrange(year) %>% dplyr::slice(n()) %>% dplyr::pull(year)
w4_first_yr   <- fig4_w_dat   %>% dplyr::arrange(year) %>% dplyr::slice(1)   %>% dplyr::pull(year)
w4_last_yr    <- fig4_w_dat   %>% dplyr::arrange(year) %>% dplyr::slice(n()) %>% dplyr::pull(year)

inc4_hh_pct   <- 100 * (inc4_hh_last - inc4_hh_first) / abs(inc4_hh_first)
inc4_cl_pct   <- 100 * (inc4_cl_last - inc4_cl_first) / abs(inc4_cl_first)
w4_hh_pct     <- 100 * (w4_hh_last   - w4_hh_first)   / abs(w4_hh_first)
w4_cl_pct     <- 100 * (w4_cl_last   - w4_cl_first)   / abs(w4_cl_first)

avg_inc4_gap  <- round(mean(fig4_inc_dat$r_hh_w_ratio - fig4_inc_dat$r_cl_w_ratio, na.rm = TRUE), 3)
avg_w4_gap    <- round(mean(fig4_w_dat$r_hh_w_ratio   - fig4_w_dat$r_cl_w_ratio,   na.rm = TRUE), 3)

# Pooled median income/wealth by race (for note and table)
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

inc_med_hh <- .race_medians(r_hh    %>% dplyr::mutate(inc_all = inc_all / numfu),
                             "inc_all", "fam_weight",  "black_head")
inc_med_cl <- .race_medians(r_clans %>% dplyr::mutate(inc_all = inc_all / numclan),
                             "inc_all", "clan_weight", "black_clan")
w_med_hh   <- .race_medians(r_hh_wealth    %>% dplyr::mutate(wealth = wealth / numfu),
                             "wealth", "fam_weight",  "black_head")
w_med_cl   <- .race_medians(r_clans_wealth %>% dplyr::mutate(wealth = wealth / numclan),
                             "wealth", "clan_weight", "black_clan")

fig4_note <- paste0(
  sprintf(
    paste0(
      "Note: Lines show the ratio of weighted median income (Panel A) or wealth (Panel B) ",
      "for Black relative to Non-Black households and clans. ",
      "The weighted median income for Black HHs is $%s, $%s for Non-Black HHs, $%s for Black clans, and $%s for Non-Black clans. ",
      "The income ratio changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
      "and %.1f%% for clans (%.2f to %.2f). "
    ),
    fmt_money0(inc_med_hh$black), fmt_money0(inc_med_hh$nonblack),
    fmt_money0(inc_med_cl$black), fmt_money0(inc_med_cl$nonblack),
    inc4_hh_pct, inc4_hh_first, inc4_first_yr, inc4_hh_last, inc4_last_yr,
    inc4_cl_pct, inc4_cl_first, inc4_cl_last
  ),
  sprintf(
    paste0(
      "Weighted median wealth for Black HHs is $%s, $%s for Non-Black HHs, $%s for Black clans, and $%s for Non-Black clans (includes home equity). ",
      "The wealth ratio changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
      "and %.1f%% for clans (%.2f to %.2f). ",
      "On average across all years, the income ratio is %.3f lower for households than clans, ",
      "and the wealth ratio is %.3f lower for households than clans. ",
      "Solid lines = households; dotted lines = clans."
    ),
    fmt_money0(w_med_hh$black), fmt_money0(w_med_hh$nonblack),
    fmt_money0(w_med_cl$black), fmt_money0(w_med_cl$nonblack),
    w4_hh_pct, w4_hh_first, w4_first_yr, w4_hh_last, w4_last_yr,
    w4_cl_pct, w4_cl_first, w4_cl_last,
    avg_inc4_gap, avg_w4_gap
  )
)

fig4 <- arrangeGrob(
  textGrob(
    "Figure 4. Black / Non-Black Median Ratios: Households vs. Clans",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
  ),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Income",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig4_inc_plot, ncol = 1, heights = c(1, 12)
    ),
    arrangeGrob(
      textGrob("Panel B: Wealth",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      fig4_w_plot, ncol = 1, heights = c(1, 12)
    ),
    ncol = 2
  ),
  ncol = 1, heights = unit(c(0.5, 10.0), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure4.pdf"), fig4, width = 14, height = 11)
  message("Saved: figure4.pdf")
}


# ── Table below Figure 4 — Race summary (two tables) ─────────────────────────
# Table A: Wealth — columns: Mean HH, Mean Clan, Median HH, Median Clan,
# Table B: Income — columns: Mean HH, Mean Clan, Median HH, Median Clan

#                             HH with 0 Wealth, Clans with 0 Wealth
# Rows for both: Black, Non-Black, Ratio (Black / Non-Black)

# Weighted % with zero (or negative) wealth, by race group
.pct_zero_wealth <- function(df, wealth_var, weight_var, race_var, race_val) {
  df_s <- df %>%
    dplyr::filter(is.finite(.data[[wealth_var]]),
                  is.finite(.data[[weight_var]]),
                  .data[[weight_var]] > 0,
                  .data[[race_var]] == race_val)
  w    <- df_s[[weight_var]]
  zero <- as.numeric(df_s[[wealth_var]] <= 0)
  100 * sum(w * zero) / sum(w)
}

# Size-standardised wealth frames
r_hhw_ss <- r_hh_wealth    %>% dplyr::mutate(wealth = wealth / numfu)
r_clw_ss <- r_clans_wealth %>% dplyr::mutate(wealth = wealth / numclan)

zero_w_hh_black    <- .pct_zero_wealth(r_hhw_ss, "wealth", "fam_weight",  "black_head", 1)
zero_w_hh_nonblack <- .pct_zero_wealth(r_hhw_ss, "wealth", "fam_weight",  "black_head", 0)
zero_w_cl_black    <- .pct_zero_wealth(r_clw_ss, "wealth", "clan_weight", "black_clan",  1)
zero_w_cl_nonblack <- .pct_zero_wealth(r_clw_ss, "wealth", "clan_weight", "black_clan",  0)

# ── Table B: Income ───────────────────────────────────────────────────────────
race_income_tbl <- tibble::tibble(
  Group                = c("Black", "Non-Black", "Ratio"),
  `Mean Income HH`     = c(
    fmt_money0(inc_race_hh$black),
    fmt_money0(inc_race_hh$nonblack),
    sprintf("%.2f", inc_race_hh$black / inc_race_hh$nonblack)
  ),
  `Mean Income Clan`   = c(
    fmt_money0(inc_race_cl$black),
    fmt_money0(inc_race_cl$nonblack),
    sprintf("%.2f", inc_race_cl$black / inc_race_cl$nonblack)
  ),
  `Median Income HH`   = c(
    fmt_money0(inc_med_hh$black),
    fmt_money0(inc_med_hh$nonblack),
    sprintf("%.2f", inc_med_hh$black / inc_med_hh$nonblack)
  ),
  `Median Income Clan` = c(
    fmt_money0(inc_med_cl$black),
    fmt_money0(inc_med_cl$nonblack),
    sprintf("%.2f", inc_med_cl$black / inc_med_cl$nonblack)
  )
)

race_income_ft <- flextable::flextable(race_income_tbl) %>%
  flextable::set_caption(
    caption = "Table B. Mean and Median Income by Race: Households vs. Clans",
    autonum = FALSE
  ) %>%
  flextable::bold(part = "header") %>%
  flextable::bold(j = "Group") %>%
  flextable::bg(part = "header", bg = GREY_HDR) %>%
  flextable::bg(i = 3, bg = GREY_HDR) %>%
  flextable::hline(i = 2, part = "body",
                   border = officer::fp_border(color = "grey60", width = 0.5)) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = "Group", align = "left", part = "body") %>%
  flextable::fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = base_family, part = "all") %>%
  flextable::padding(padding = 5, part = "all") %>%
  flextable::fit_to_width(max_width = 6.5)

# ── Table A: Wealth ───────────────────────────────────────────────────────────
race_wealth_tbl <- tibble::tibble(
  Group                  = c("Black", "Non-Black", "Ratio"),
  `Mean Wealth HH`       = c(
    fmt_money0(w_race_hh$black),
    fmt_money0(w_race_hh$nonblack),
    sprintf("%.2f", w_race_hh$black / w_race_hh$nonblack)
  ),
  `Mean Wealth Clan`     = c(
    fmt_money0(w_race_cl$black),
    fmt_money0(w_race_cl$nonblack),
    sprintf("%.2f", w_race_cl$black / w_race_cl$nonblack)
  ),
  `Median Wealth HH`     = c(
    fmt_money0(w_med_hh$black),
    fmt_money0(w_med_hh$nonblack),
    sprintf("%.2f", w_med_hh$black / w_med_hh$nonblack)
  ),
  `Median Wealth Clan`   = c(
    fmt_money0(w_med_cl$black),
    fmt_money0(w_med_cl$nonblack),
    sprintf("%.2f", w_med_cl$black / w_med_cl$nonblack)
  ),
  `HH with 0 Wealth`     = c(
    sprintf("%.1f%%", zero_w_hh_black),
    sprintf("%.1f%%", zero_w_hh_nonblack),
    sprintf("%.2f", zero_w_hh_black / zero_w_hh_nonblack)
  ),
  `Clans with 0 Wealth`  = c(
    sprintf("%.1f%%", zero_w_cl_black),
    sprintf("%.1f%%", zero_w_cl_nonblack),
    sprintf("%.2f", zero_w_cl_black / zero_w_cl_nonblack)
  )
)

race_wealth_ft <- flextable::flextable(race_wealth_tbl) %>%
  flextable::set_caption(
    caption = "Table A. Mean and Median Wealth by Race: Households vs. Clans",
    autonum = FALSE
  ) %>%
  flextable::bold(part = "header") %>%
  flextable::bold(j = "Group") %>%
  flextable::bg(part = "header", bg = GREY_HDR) %>%
  flextable::bg(i = 3, bg = GREY_HDR) %>%
  flextable::hline(i = 2, part = "body",
                   border = officer::fp_border(color = "grey60", width = 0.5)) %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = "Group", align = "left", part = "body") %>%
  flextable::fontsize(size = 10, part = "all") %>%
  flextable::font(fontname = base_family, part = "all") %>%
  flextable::padding(padding = 5, part = "all") %>%
  flextable::fit_to_width(max_width = 6.5)

fig4_table_note <- paste0(
  "Note: All values are weighted using PSID family and clan weights and pooled across all years. ",
  "Income and wealth are size-standardised (divided by household or clan size). ",
  "Wealth includes home equity. '0 Wealth' indicates wealth <= 0. ",
  "Ratio row shows Black / Non-Black."
)

# ── Inline scalars for racial wealth gap paragraph ────────────────────────────

# Mean wealth ratios (Black / Non-Black)
w_mean_ratio_hh   <- w_race_hh$black  / w_race_hh$nonblack
w_mean_ratio_cl   <- w_race_cl$black  / w_race_cl$nonblack

# Median wealth ratios (Black / Non-Black)
w_med_ratio_hh    <- w_med_hh$black   / w_med_hh$nonblack
w_med_ratio_cl    <- w_med_cl$black   / w_med_cl$nonblack

# % reduction in zero-wealth share moving from HH to Clan level
zero_w_pct_red_black    <- 100 * (zero_w_hh_black    - zero_w_cl_black)    / zero_w_hh_black
zero_w_pct_red_nonblack <- 100 * (zero_w_hh_nonblack - zero_w_cl_nonblack) / zero_w_hh_nonblack

