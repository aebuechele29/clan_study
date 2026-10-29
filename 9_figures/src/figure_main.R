# Functions used in this script:
#   Defined in this script: tab1_fmt2, tab1_fmt1, tab1_fmt0, get_legend_safe,
#     .race_means, .race_medians, tab1_wstat, tab1_n, tab1_prep_demo,
#     tab1_age, tab1_unit_stats, tab1_fmt_cell, tab1_make_df, tab1_make_ft,
#     neg_wstats, neg_race_wstats, money

#   From functions/data_utils.R: get_gini_at
#   From functions/wtd_stats.R: wtd_mean, wtd_median, wtd_sd
#   From functions/plot_helpers.R: make_gini_plot, make_lorenz_plot,
#     make_ratio_plot
#   From functions/table_helpers.R: fmt_money0

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

# Formatting helpers for Table 1. Same code as fmt0/fmt1/fmt2 in write.rmd, but renamed so the two sets no longer overwrite each other.
# Used in: figure_main
tab1_fmt2 <- function(x) format(round(x, 2), nsmall = 2)
# Used in: figure_main
tab1_fmt1 <- function(x) format(round(x, 1), nsmall = 1)
# Used in: figure_main
tab1_fmt0 <- function(x) format(round(x, 0), big.mark = ",")

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

# Version-safe legend extraction (cowplot::get_legend returns empty with ggplot2 >= 3.5)
# Used in: figure_main, figure_appendix
get_legend_safe <- function(p) {
  g   <- ggplotGrob(p)
  idx <- which(grepl("^guide-box", g$layout$name) &
               !vapply(g$grobs, inherits, logical(1), "zeroGrob"))
  if (length(idx) == 0) grid::nullGrob() else g$grobs[[idx[1]]]
}


# Load data
r_hh           <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans        <- readRDS(here("4_clans",      "output", "robust_clans.rds"))
r_hh_wealth    <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans_wealth <- readRDS(here("4_clans",      "output", "robust_clans_wealth.rds"))
mismatched     <- readRDS(here("3_households", "output", "mismatched.rds"))

# Negative-robust versions of the robust samples (negative income/wealth retained).
# Used for every median/mean reported in Table 1, Appendix F1, and the Figure 1–3 notes.
NEG_HH_PATH        <- here("3_households", "output", "neg_robust_households.rds")
NEG_CLANS_PATH     <- here("4_clans",      "output", "neg_robust_clans.rds")
NEG_HH_W_PATH      <- here("3_households", "output", "neg_robust_households_wealth.rds")
NEG_CLANS_W_PATH   <- here("4_clans",      "output", "neg_robust_clans_wealth.rds")
neg_paths <- c(NEG_HH_PATH, NEG_CLANS_PATH, NEG_HH_W_PATH, NEG_CLANS_W_PATH)
if (!all(file.exists(neg_paths))) {
  stop("Negative-robust datasets not found:\n  ",
       paste(neg_paths[!file.exists(neg_paths)], collapse = "\n  "),
       "\nUpdate NEG_*_PATH in figure_main.R to the files that retain negative values.")
}
neg_hh           <- readRDS(NEG_HH_PATH)
neg_clans        <- readRDS(NEG_CLANS_PATH)
neg_hh_wealth    <- readRDS(NEG_HH_W_PATH)
neg_clans_wealth <- readRDS(NEG_CLANS_W_PATH)

# Weighted median and mean of `var` (optionally divided by `denom`), optionally for one year
# Used in: figure_main
neg_wstats <- function(df, var, weight_var, yr = NULL, denom = NULL) {
  if (!is.null(yr)) df <- df[df$year == yr, , drop = FALSE]
  v  <- df[[var]]
  if (!is.null(denom)) v <- v / df[[denom]]
  w  <- df[[weight_var]]
  ok <- is.finite(v) & is.finite(w) & w > 0
  if (!any(ok)) return(list(med = NA_real_, mean = NA_real_))
  list(med = wtd_median(v[ok], w[ok]), mean = wtd_mean(v[ok], w[ok]))
}

# Same, split by race (race_var == 1 Black, 0 Non-Black)
# Used in: figure_main
neg_race_wstats <- function(df, var, weight_var, race_var, denom = NULL) {
  list(black    = neg_wstats(df[which(df[[race_var]] == 1), , drop = FALSE], var, weight_var, denom = denom),
       nonblack = neg_wstats(df[which(df[[race_var]] == 0), , drop = FALSE], var, weight_var, denom = denom))
}

# "$12,345" formatter for notes
# Used in: figure_main
money <- function(x) paste0("$", trimws(fmt_money0(x)))

inc_by_year    <- read_csv(here("6_calculate_gini", "output", "income.csv"),
                           show_col_types = FALSE)
wealth_by_year <- read_csv(here("6_calculate_gini", "output", "wealth_withhome.csv"),
                           show_col_types = FALSE)
wealth_ratios  <- read_csv(here("7_gini_by_race",    "output", "wealth_withhome_race_ratios.csv"),
                           show_col_types = FALSE)

# Family demographic summaries (robust sample), used for average age in Table 2
inc_family_demo <- read_csv(here("5_summary", "output", "inc_family_demo.csv"),
                            show_col_types = FALSE)

w_family_demo_path <- here("5_summary", "output", "w_family_demo.csv")
w_family_demo <- if (file.exists(w_family_demo_path)) {
  read_csv(w_family_demo_path, show_col_types = FALSE)
} else {
  message("Note: ", w_family_demo_path, " not found; ",
          "wealth-sample ages will be omitted from the Table 1 note.")
  NULL
}


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

# Early-1970s household income Gini, compared in the text with the 2023 kin group income Gini
inc_hh_1971 <- get_gini_at(inc_by_year, 1971, r_hh_w_inc)
inc_hh_1972 <- get_gini_at(inc_by_year, 1972, r_hh_w_inc)
inc_hh_1973 <- get_gini_at(inc_by_year, 1973, r_hh_w_inc)
inc_hh_early70s      <- c(inc_hh_1971, inc_hh_1972, inc_hh_1973)
inc_hh_early70s_mean <- mean(inc_hh_early70s, na.rm = TRUE)
inc_cl2023_vs_hh_early70s <- inc_hh_early70s_mean - inc_cl_2023  # positive = kin group 2023 lower

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

# ── Pooled weighted means and medians, all years (negative values retained) 
neg_inc_hh_all <- neg_wstats(neg_hh,           "inc_all", "fam_weight")
neg_inc_cl_all <- neg_wstats(neg_clans,        "inc_all", "clan_weight")
neg_w_hh_all   <- neg_wstats(neg_hh_wealth,    "wealth",  "fam_weight")
neg_w_cl_all   <- neg_wstats(neg_clans_wealth, "wealth",  "clan_weight")


inc_mean_hh_w <- neg_inc_hh_all$mean
inc_mean_cl_w <- neg_inc_cl_all$mean
w_mean_hh_w   <- neg_w_hh_all$mean
w_mean_cl_w   <- neg_w_cl_all$mean
inc_med_hh_w  <- neg_inc_hh_all$med
inc_med_cl_w  <- neg_inc_cl_all$med
w_med_hh_w    <- neg_w_hh_all$med
w_med_cl_w    <- neg_w_cl_all$med

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
# Used in: figure_main
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

# Used in: figure_main
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

# Single shared legend and integer decade breaks on the year axis
fig1_legend <- get_legend_safe(fig1_inc + theme(legend.position = "bottom"))
fig1_inc <- fig1_inc +
  scale_x_continuous(breaks = seq(1970, 2020, 10), limits = c(1969, 2023)) +
  theme(legend.position = "none")
fig1_w   <- fig1_w +
  scale_x_continuous(breaks = seq(1985, 2020, 5), limits = c(1984, 2023)) +
  theme(legend.position = "none")

# Figure 1 note: pooled medians/means come from neg_*_all (defined above)
fig1_note <- sprintf(
  paste0(
    "Note: Gini coefficients estimated from weighted PSID data. ",
    "Across all years, median (mean) income is %s (%s) for households and %s (%s) for kin groups. ",
    "Median (mean) wealth is %s (%s) for households and %s (%s) for kin groups, including home equity. ",
    "Negative income and wealth values are set to zero for calculating Gini coefficients, ",
    "but are retained in the median and mean estimates reported here. ",
    "Estimates are weighted, adjusted for inflation, and expressed in 2023 dollars. ",
    "Income Ginis changed by %.1f%% for households (%.2f in 1969 to %.2f in 2023) ",
    "and by %.1f%% for kin groups (%.2f to %.2f). ",
    "Wealth Ginis changed by %.1f%% for households (%.2f in 1984 to %.2f in 2023) ",
    "and by %.1f%% for kin groups (%.2f to %.2f). ",
    "On average across all years, the income Gini is %.3f higher for households than kin groups; ",
    "the wealth Gini is %.3f higher."
  ),
  money(neg_inc_hh_all$med), money(neg_inc_hh_all$mean),
  money(neg_inc_cl_all$med), money(neg_inc_cl_all$mean),
  money(neg_w_hh_all$med),   money(neg_w_hh_all$mean),
  money(neg_w_cl_all$med),   money(neg_w_cl_all$mean),
  inc_hh_pct, inc_hh_1969, inc_hh_2023,
  inc_cl_pct, inc_cl_1969, inc_cl_2023,
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
  fig1_legend,
  ncol = 1, heights = unit(c(0.5, 9.5, 0.7), "inches"))

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

# Single shared legend below both panels
fig2_legend_donor <- ggplot(
  tidyr::expand_grid(Year = c("1984", "2023"), Unit = c("Households", "Kin Groups"), x = 0:1),
  aes(x = x, y = x, color = Year, linetype = Unit, group = interaction(Year, Unit))) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("1984" = NAVY, "2023" = CORAL), name = NULL) +
  scale_linetype_manual(values = c("Households" = "solid", "Kin Groups" = "dotted"), name = NULL) +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))
fig2_legend <- get_legend_safe(fig2_legend_donor)
fig2_inc    <- fig2_inc + theme(legend.position = "none")
fig2_w      <- fig2_w   + theme(legend.position = "none")

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

# 1984 / 2023 medians/means for the Figure 2 note (negative values retained)
neg_fig2 <- purrr::map(c(`1984` = 1984, `2023` = 2023), function(yr) list(
  inc_hh = neg_wstats(neg_hh,           "inc_all", "fam_weight",  yr),
  inc_cl = neg_wstats(neg_clans,        "inc_all", "clan_weight", yr),
  w_hh   = neg_wstats(neg_hh_wealth,    "wealth",  "fam_weight",  yr),
  w_cl   = neg_wstats(neg_clans_wealth, "wealth",  "clan_weight", yr)))

fig2_note <- sprintf(
  paste0(
    "Note: Lorenz curves are estimated using PSID family and kin group weights. ",
    "In 1984, median (mean) income was %s (%s) for households and %s (%s) for kin groups; ",
    "in 2023, it was %s (%s) for households and %s (%s) for kin groups. ",
    "Median (mean) wealth was %s (%s) for households and %s (%s) for kin groups in 1984, ",
    "compared with %s (%s) and %s (%s), respectively, in 2023. ",
    "Wealth measures include home equity. ",
    "Negative income and wealth values are set to zero when constructing Lorenz curves ",
    "but retained when calculating the reported medians and means. ",
    "All estimates are weighted, adjusted for inflation, and expressed in 2023 dollars."
  ),
  money(neg_fig2$`1984`$inc_hh$med), money(neg_fig2$`1984`$inc_hh$mean),
  money(neg_fig2$`1984`$inc_cl$med), money(neg_fig2$`1984`$inc_cl$mean),
  money(neg_fig2$`2023`$inc_hh$med), money(neg_fig2$`2023`$inc_hh$mean),
  money(neg_fig2$`2023`$inc_cl$med), money(neg_fig2$`2023`$inc_cl$mean),
  money(neg_fig2$`1984`$w_hh$med),   money(neg_fig2$`1984`$w_hh$mean),
  money(neg_fig2$`1984`$w_cl$med),   money(neg_fig2$`1984`$w_cl$mean),
  money(neg_fig2$`2023`$w_hh$med),   money(neg_fig2$`2023`$w_hh$mean),
  money(neg_fig2$`2023`$w_cl$med),   money(neg_fig2$`2023`$w_cl$mean)
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
  fig2_legend,
  ncol = 1, heights = unit(c(0.5, 7.8, 0.6), "inches"))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure2.pdf"), fig2, width = 14, height = 9)
  message("Saved: figure2.pdf")
}


# ── Figure 3 — Black / Non-Black Median Wealth Ratio ─────────────────────────
fig3_med_plot <- make_ratio_plot(
  wealth_ratios %>% select(year,
                           r_hh_w_ratio = r_hh_w_median_ratio,
                           r_cl_w_ratio = r_cl_w_median_ratio),
  ylab = "Black / Non-Black") +
  scale_x_continuous(breaks = seq(1985, 2020, 5), limits = c(1984, 2023))

# Pooled wealth by race for the Figure 3 note 
neg_race_hh <- neg_race_wstats(neg_hh_wealth,    "wealth", "fam_weight",  "black_head", denom = "numfu")
neg_race_cl <- neg_race_wstats(neg_clans_wealth, "wealth", "clan_weight", "black_clan", denom = "numclan")

fig3_note <- sprintf(
  paste0(
    "Note: Lines show the ratio of Black to Non-Black wealth for households and kin groups. ",
    "Wealth measures include home equity and use weighted medians. ",
    "Across all years, weighted median (mean) wealth is %s (%s) for Black households ",
    "and %s (%s) for Non-Black households. ",
    "For kin groups, the weighted median (mean) wealth is %s (%s) for Black kin groups ",
    "and %s (%s) for Non-Black kin groups. ",
    "Negative wealth values are set to zero for calculating race ratios, ",
    "but are retained in the mean and median estimates reported here. ",
    "Estimates are weighted, adjusted for inflation, and expressed in 2023 dollars. ",
    "The median ratio changed by %.1f%% for households (%.3f in %d to %.3f in %d) ",
    "and %.1f%% for kin groups (%.3f to %.3f). ",
    "On average across all years, the median wealth ratio for kin groups is %.3f higher than for households. ",
    "See Appendix E1 for mean wealth ratios."
  ),
  money(neg_race_hh$black$med),    money(neg_race_hh$black$mean),
  money(neg_race_hh$nonblack$med), money(neg_race_hh$nonblack$mean),
  money(neg_race_cl$black$med),    money(neg_race_cl$black$mean),
  money(neg_race_cl$nonblack$med), money(neg_race_cl$nonblack$mean),
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



# TABLE 1 — Characteristics of Households and Kin Groups
tab1_years    <- c(1984, 2023)
tab1_periods  <- c(as.character(tab1_years), "All years")
tab1_labels   <- c(as.character(tab1_years), "All years (1969\u20132023)")
tab1_pkeys    <- c(paste0("y", tab1_years), "yall")
tab1_col_keys <- paste0(rep(tab1_pkeys, each = 2), "_", c("hh", "kg"))


# Used in: figure_main
tab1_wstat <- function(df, var, weight_var, fun, yr = NULL) {
  if (!is.null(yr)) df <- df[df$year == yr, , drop = FALSE]
  v  <- df[[var]]
  w  <- df[[weight_var]]
  ok <- is.finite(v) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  fun(v[ok], w[ok])
}

# Used in: figure_main
tab1_n <- function(df, yr = NULL) {
  if (is.null(yr)) nrow(df) else sum(df$year == yr, na.rm = TRUE)
}


# Used in: figure_main
tab1_prep_demo <- function(demo) {
  demo %>%
    dplyr::mutate(year = suppressWarnings(as.numeric(year))) %>%
    dplyr::filter(!is.na(year))
}

# Used in: figure_main
tab1_age <- function(demo, var, df, yr = NULL) {
  if (!is.null(yr)) {
    v <- demo[[var]][demo$year == yr]
    return(if (length(v) == 0 || !is.finite(v[1])) NA_real_ else v[1])
  }
  d <- demo %>%
    dplyr::select(year, val = dplyr::all_of(var)) %>%
    dplyr::inner_join(df %>% dplyr::count(year, name = "n"), by = "year") %>%
    dplyr::filter(is.finite(val))
  if (nrow(d) == 0) NA_real_ else sum(d$val * d$n) / sum(d$n)
}

tab1_demo_inc <- tab1_prep_demo(inc_family_demo)

# All statistics for one unit (hh / kg) in one period
# Used in: figure_main
tab1_unit_stats <- function(unit, period) {
  yr <- if (period == "All years") NULL else as.numeric(period)
  if (unit == "hh") {
    list(
      age      = tab1_age(tab1_demo_inc, "hh_age_mean", r_hh, yr),
      size_ppl = tab1_wstat(r_hh, "numfu", "fam_weight", wtd_mean, yr),
      size_hh  = NA_real_,
      # Medians use the negative-robust samples (negative values retained)
      inc      = tab1_wstat(neg_hh,        "inc_all", "fam_weight", wtd_median, yr),
      wealth   = tab1_wstat(neg_hh_wealth, "wealth",  "fam_weight", wtd_median, yr),
      n        = tab1_n(r_hh, yr),
      n_black  = tab1_n(r_hh[which(r_hh$black_head == 1), , drop = FALSE], yr))
  } else {
    list(
      age      = tab1_age(tab1_demo_inc, "clan_age_mean", r_clans, yr),
      size_ppl = tab1_wstat(r_clans, "num_clan_people", "clan_weight", wtd_mean, yr),
      size_hh  = tab1_wstat(r_clans, "numclan",         "clan_weight", wtd_mean, yr),
      # Medians use the negative-robust samples (negative values retained)
      inc      = tab1_wstat(neg_clans,        "inc_all", "clan_weight", wtd_median, yr),
      wealth   = tab1_wstat(neg_clans_wealth, "wealth",  "clan_weight", wtd_median, yr),
      n        = tab1_n(r_clans, yr),
      n_black  = tab1_n(r_clans[which(r_clans$black_clan == 1), , drop = FALSE], yr))
  }
}

tab1_stats <- purrr::map(tab1_periods, function(p)
  list(hh = tab1_unit_stats("hh", p), kg = tab1_unit_stats("kg", p)))
names(tab1_stats) <- tab1_periods

# Row definitions: label, formatter, and what to print when a cell is empty
tab1_rows <- list(
  age      = list(label = "Average age (years)",     fmt = tab1_fmt1, na = "\u2014"),
  size_ppl = list(label = "Average # of people",     fmt = tab1_fmt2, na = "\u2014"),
  size_hh  = list(label = "Average # of households", fmt = tab1_fmt2, na = "\u2014"),
  inc      = list(label = "Median income",      fmt = function(x) paste0("$", fmt_money0(x)), na = "\u2014"),
  wealth   = list(label = "Median wealth",      fmt = function(x) paste0("$", fmt_money0(x)), na = "\u2014"),
  n        = list(label = "N (unit-years)",     fmt = tab1_fmt0, na = "\u2014"),
  n_black  = list(label = "N Black (unit-years)", fmt = tab1_fmt0, na = "\u2014")
)

# Used in: figure_main
tab1_fmt_cell <- function(x, spec) {
  if (length(x) == 0 || is.na(x)) spec$na else trimws(spec$fmt(x))
}

# Used in: figure_main
tab1_make_df <- function(periods, col_keys) {
  purrr::imap_dfr(tab1_rows, function(spec, key) {
    vals <- unlist(purrr::map(periods, function(p) c(
      tab1_fmt_cell(tab1_stats[[p]]$hh[[key]], spec),
      tab1_fmt_cell(tab1_stats[[p]]$kg[[key]], spec))))
    names(vals) <- col_keys
    tibble::as_tibble_row(c(label = spec$label, vals))
  })
}

# ── Table 1 note ──────────────────────────────────────────────────────────────
tab1_w_age_txt <- if (!is.null(w_family_demo)) {
  tab1_demo_w <- tab1_prep_demo(w_family_demo)
  sprintf(", and an average age of %s for households and %s for kin groups",
          tab1_fmt1(tab1_age(tab1_demo_w, "hh_age_mean",   r_hh_wealth)),
          tab1_fmt1(tab1_age(tab1_demo_w, "clan_age_mean", r_clans_wealth)))
} else ""

tab1_note <- sprintf(
  paste0(
    "Note: Average age, number of people, and number of households are based on the income sample (1969-2023), ",
    "which contains %s unique kin groups. ",
    "Median wealth is based on the wealth sample (1984-2023) and includes home equity. ",
    "Size and median values are weighted using PSID family and constructed kin group weights (see Methods). ",
    "Pooled across all years, the wealth sample contains %s household-years and %s kin group-years ",
    "(%s and %s Black, respectively) ",
    "from %s unique kin groups, with an average size of %s individuals per household ",
    "and %s individuals (%s households) per kin group%s. ",
    "Median income and wealth include households and kin groups with negative income or wealth. ",
    "Estimates are weighted, adjusted for inflation, and expressed in 2023 dollars."
  ),
  tab1_fmt0(inc_uniq_clans),
  tab1_fmt0(w_hh_years), tab1_fmt0(w_cl_years),
  trimws(tab1_fmt0(sum(r_hh_wealth$black_head == 1, na.rm = TRUE))),
  trimws(tab1_fmt0(sum(r_clans_wealth$black_clan == 1, na.rm = TRUE))),
  tab1_fmt0(w_uniq_clans),
  trimws(tab1_fmt2(hh_size_w_mean)), trimws(tab1_fmt2(w_cl_ppl_mean)), trimws(tab1_fmt2(w_cl_hh_mean)),
  tab1_w_age_txt
)

# ── Table 1 flextable ─────────────────────────────────────────────────────────
tab1_border <- officer::fp_border(color = "black", width = 0.75)

# Used in: figure_main, figure_appendix
tab1_make_ft <- function(periods, labels, pkeys, caption,
                         width = NULL, label_share = 0.4) {
  col_keys <- paste0(rep(pkeys, each = 2), "_", c("hh", "kg"))
  df       <- tab1_make_df(periods, col_keys)
  n_row    <- which(df$label == "N (unit-years)")
  ft <- flextable::flextable(df, col_keys = c("label", col_keys)) %>%
  flextable::set_header_labels(
    values = setNames(as.list(c("", rep(c("Households", "Kin Groups"), length(periods)))),
                      c("label", col_keys))) %>%
  flextable::add_header_row(values    = c("", labels),
                            colwidths = c(1, rep(2, length(periods)))) %>%
  flextable::theme_booktabs() %>%
  flextable::merge_v(j = 1, part = "header") %>%
  # thin rule under the spanning year labels
  flextable::hline(i = 1, j = 2:(1 + length(col_keys)),
                   border = tab1_border, part = "header") %>%
  # rule separating the N row from the statistics above
  flextable::hline(i = n_row - 1, border = tab1_border, part = "body") %>%
  flextable::align(j = 1,  align = "left",   part = "all") %>%
  flextable::align(j = -1, align = "center", part = "all") %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 10, part = "all") %>%
  flextable::add_footer_lines(tab1_note) %>%
  flextable::fontsize(size = 9, part = "footer") %>%
  flextable::font(fontname = "Times New Roman", part = "footer") %>%
  flextable::set_caption(caption) %>%
  flextable::autofit()

  if (!is.null(width)) {
    ft <- ft %>%
      flextable::width(j = 1,  width = width * label_share) %>%
      flextable::width(j = -1, width = width * (1 - label_share) / length(col_keys)) %>%
      flextable::set_table_properties(layout = "fixed")
  }
  ft
}

# Table 1 reports pooled all-years columns only; 1984/2023 columns go to the appendix table
tab1_ft <- tab1_make_ft("All years", "All years (1969–2023)", "yall",
                        "Table 1. Characteristics of Households and Kin Groups",
                        width = 6.5)

if (SAVE_FILES) {
  flextable::save_as_docx(
    tab1_ft,
    path = here("9_figures", "output", "table1.docx"),
    pr_section = officer::prop_section(
      page_size = officer::page_size(orient = "landscape")))
  message("Saved: table1.docx")
}