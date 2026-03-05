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
inc_ratio_hh_first <- inc_ratios %>% arrange(year) %>% slice(1)  %>% pull(r_hh_w_ratio)
inc_ratio_hh_last  <- inc_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_hh_w_ratio)
inc_ratio_cl_first <- inc_ratios %>% arrange(year) %>% slice(1)  %>% pull(r_cl_w_ratio)
inc_ratio_cl_last  <- inc_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_cl_w_ratio)

w_ratio_hh_first <- wealth_ratios %>% arrange(year) %>% slice(1)  %>% pull(r_hh_w_ratio)
w_ratio_hh_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_hh_w_ratio)
w_ratio_cl_first <- wealth_ratios %>% arrange(year) %>% slice(1)  %>% pull(r_cl_w_ratio)
w_ratio_cl_last  <- wealth_ratios %>% arrange(year) %>% slice(n()) %>% pull(r_cl_w_ratio)


# Figure 1 — Gini over time
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
    "Note: Gini coefficients are estimated from weighted data using PSID family ",
    "and clan weights. Weighted mean income is $%s for households and $%s for clans. ",
    "Weighted mean wealth is $%s for households and $%s for clans (wealth includes ",
    "home equity). Income Gini for households rose by %.1f%% (%.2f in 1969 to %.2f in 2023); ",
    "for clans by %.1f%% (%.2f to %.2f). Wealth Gini for households rose by %.1f%% ",
    "(%.2f in 1984 to %.2f in 2023); for clans by %.1f%% (%.2f to %.2f). ",
    "Solid lines = households; dotted lines = clans."
  ),
  fmt_money0(inc_mean_hh_w), fmt_money0(inc_mean_cl_w),
  fmt_money0(w_mean_hh_w),   fmt_money0(w_mean_cl_w),
  inc_hh_pct, inc_hh_1969, inc_hh_2023,
  inc_cl_pct, inc_cl_1969, inc_cl_2023,
  w_hh_pct,   w_hh_1984,   w_hh_2023,
  w_cl_pct,   w_cl_1984,   w_cl_2023
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
  note_grob(fig1_note, width = 150),
  ncol = 1, heights = unit(c(0.5, 9.0, 1.0), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure1.pdf"), fig1, width = 14, height = 11)
  message("Saved: figure1.pdf")
}


# Figure 2 — Lorenz curves
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
  note_grob(fig2_note, width = 150),
  ncol = 1, heights = unit(c(0.5, 7.5, 0.9), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure2.pdf"), fig2, width = 14, height = 9)
  message("Saved: figure2.pdf")
}


# Figure 3 — Black / Non-Black Mean Ratios 
all_ratio_vals <- c(inc_ratios$r_hh_w_ratio,    inc_ratios$r_cl_w_ratio,
                    wealth_ratios$r_hh_w_ratio,  wealth_ratios$r_cl_w_ratio)
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

fig3_note <- sprintf(
  paste0(
    "Note: Lines show the ratio of weighted mean income (Panel A) or wealth (Panel B) ",
    "for Black households and clans relative to Non-Black households and clans. Estimates are weighted, ",
    "A value of 1 indicates parity between Black and non-Black households or clans.",
    "For income, the household ratio was %.2f in the first year and %.2f in the last year; ",
    "the clan ratio was %.2f and %.2f. ",
    "For wealth, the household ratio was %.2f in the first year and %.2f in the last year; ",
    "the clan ratio was %.2f and %.2f. ",
    "Wealth includes home equity."
  ),
  inc_ratio_hh_first, inc_ratio_hh_last,
  inc_ratio_cl_first, inc_ratio_cl_last,
  w_ratio_hh_first,   w_ratio_hh_last,
  w_ratio_cl_first,   w_ratio_cl_last
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
  ncol = 1, heights = unit(c(0.5, 9.0, 1.0), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure3.pdf"), fig3, width = 14, height = 11)
  message("Saved: figure3.pdf")
}

