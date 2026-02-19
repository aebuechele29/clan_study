library(here)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(readr)
library(ggplot2)
library(cowplot)
library(flextable)
library(officer)
library(grid)
library(gridExtra)

# Load external functions
source(here::here("functions", "functions.R"))

# Load data
# Set SAVE_FILES <- FALSE before sourcing from paper.Rmd to skip ggsave calls
if (!exists("SAVE_FILES")) SAVE_FILES <- TRUE

dir.create(here("9_figures", "output"), recursive = TRUE, showWarnings = FALSE)

r_hh    <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans",      "output", "robust_clans.rds"))

wealth_years   <- c(1984, 1989, 1994, seq(1999, 2023, by = 2))
r_hh_wealth    <- r_hh    %>% filter(year %in% wealth_years)
r_clans_wealth <- r_clans %>% filter(year %in% wealth_years)

# Sample size scalars for inline text
mismatched        <- readRDS(here("3_households", "output", "mismatched.rds"))
r_hh_wealth_full  <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))

n_mismatched      <- nrow(mismatched)

inc_hh_years      <- nrow(r_hh)
inc_cl_years      <- nrow(r_clans)
inc_uniq_clans    <- n_distinct(r_clans$id1968)

w_hh_years        <- nrow(r_hh_wealth_full)
w_cl_years        <- nrow(r_clans_wealth)
w_uniq_clans      <- n_distinct(r_clans_wealth$id1968)

inc_by_year    <- read_csv(here("6_calculate_gini", "output", "income.csv"),              show_col_types = FALSE)
wealth_by_year <- read_csv(here("6_calculate_gini", "output", "wealth_withhome.csv"),      show_col_types = FALSE)
summary        <- read_csv(here("5_summary",         "output", "summary_statistics.csv"), show_col_types = FALSE)

# Global styles
base_family <- "serif"
base_size   <- 18
title_size  <- 22
sub_size    <- 20
note_size   <- 12

theme_set(theme_minimal(base_size = base_size, base_family = base_family))
note_style <- fp_text(italic = TRUE, font.size = note_size)

# Helper functions
fmt_se     <- function(x, se) sprintf("%.3f\n(SE = %.3f)", x, se)
fmt_money0 <- function(x) format(round(x, 0), big.mark = ",")
fmt_int    <- function(x) format(round(as.numeric(x), 0), big.mark = ",")
pct_change <- function(start, end) 100 * (end - start) / start

wtd_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  sum(x[keep] * w[keep]) / sum(w[keep])
}

wtd_median <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  x[which(cumsum(w) / sum(w) >= 0.5)[1]]
}

lorenz_tbl <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  tibble(p = c(0, cumsum(w) / sum(w)), L = c(0, cumsum(x * w) / sum(x * w)))
}

get_lorenz_weighted <- function(df, value_var, weight_var, years, unit_label) {
  df %>%
    filter(year %in% years, is.finite(.data[[value_var]])) %>%
    transmute(year, value = .data[[value_var]], w = .data[[weight_var]]) %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      lorenz_tbl(d$value, d$w) %>%
        mutate(year = unique(d$year), Unit = factor(unit_label, levels = c("Household", "Clan")))
    })
}

get_year_counts <- function(sum_df) {
  tibble(
    hh_years = sum_df %>% filter(Unit == "Household") %>% pull(N)            %>% first(),
    cl_years = sum_df %>% filter(Unit == "Clan")      %>% pull(N)            %>% first(),
    uniq_cl  = sum_df %>% filter(Unit == "Clan")      %>% pull(unique_clans) %>% first()
  )
}

get_gini_at <- function(df, year_value, col) {
  df %>%
    filter(year != "ALL") %>%
    mutate(year = as.numeric(year)) %>%
    filter(year == year_value) %>%
    pull({{ col }}) %>%
    first()
}

# note_grob: converts wrapped italic note text to a grob for use in arrangeGrob
note_grob <- function(txt, width = 110, size = note_size, family = base_family) {
  wrapped <- paste(strwrap(txt, width = width), collapse = "\n")
  textGrob(
    wrapped,
    x = unit(0.01, "npc"), just = "left",
    gp = gpar(fontfamily = family, fontface = "italic", fontsize = size),
    default.units = "npc"
  )
}

# make_gini_plot: line plot of Gini over time; accepts shared y limits for comparability
make_gini_plot <- function(by_year_df, hh_col, cl_col, ylab, y_limits = NULL) {
  dat <- by_year_df %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))

  min_yr <- min(dat$year, na.rm = TRUE)
  max_yr <- max(dat$year, na.rm = TRUE)

  br <- seq(ceiling(min_yr / 10) * 10, floor(max_yr / 10) * 10, by = 10)
  br <- br[br > (min_yr + 4) & br < (max_yr - 4)]
  br <- sort(unique(c(min_yr, br, max_yr)))

  if (is.null(y_limits)) {
    vals <- c(pull(dat, {{ hh_col }}), pull(dat, {{ cl_col }}))
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  ggplot(dat, aes(x = year)) +
    geom_line(aes(y = {{ hh_col }}, linetype = "Household"), color = "#E66101", linewidth = 0.9) +
    geom_line(aes(y = {{ cl_col }}, linetype = "Clan"),      color = "#FDB863", linewidth = 0.9) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    labs(x = "Year", y = ylab, linetype = "Unit") +
    theme(legend.position = "bottom", plot.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# make_lorenz_plot: Lorenz curves by year for households vs clans
make_lorenz_plot <- function(df_hh, df_cl, value_var, years, ylab, colors) {
  dat <- bind_rows(
    get_lorenz_weighted(df_hh, value_var, "fam_weight",  years, "Household"),
    get_lorenz_weighted(df_cl, value_var, "clan_weight", years, "Clan")
  )
  ggplot(dat, aes(x = p, y = L, color = factor(year), linetype = Unit,
                  group = interaction(year, Unit))) +
    geom_line(linewidth = 0.75) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    labs(x = "Cumulative Proportion of Units", y = ylab, color = "Year", linetype = "Unit") +
    theme(legend.position = "bottom", plot.title = element_blank())
}

# Precompute Gini endpoints and means for Figure 1 note
inc_hh_1969   <- get_gini_at(inc_by_year,    1969, r_hh_w_inc)
inc_hh_2023   <- get_gini_at(inc_by_year,    2023, r_hh_w_inc)
inc_cl_1969   <- get_gini_at(inc_by_year,    1969, r_cl_w_inc)
inc_cl_2023   <- get_gini_at(inc_by_year,    2023, r_cl_w_inc)
w_hh_1984     <- get_gini_at(wealth_by_year, 1984, r_hh_w_wealth)
w_hh_2023     <- get_gini_at(wealth_by_year, 2023, r_hh_w_wealth)
w_cl_1984     <- get_gini_at(wealth_by_year, 1984, r_cl_w_wealth)
w_cl_2023     <- get_gini_at(wealth_by_year, 2023, r_cl_w_wealth)

# Percent changes over time (used in figure notes and inline text)
inc_hh_pct <- pct_change(inc_hh_1969, inc_hh_2023)
inc_cl_pct <- pct_change(inc_cl_1969, inc_cl_2023)
w_hh_pct   <- pct_change(w_hh_1984,  w_hh_2023)
w_cl_pct   <- pct_change(w_cl_1984,  w_cl_2023)

# HH vs clan income gap at endpoints
inc_gap_1969 <- inc_hh_1969 - inc_cl_1969
inc_gap_2023 <- inc_hh_2023 - inc_cl_2023

# Linear projection: year when HH and clan income Ginis would converge
inc_gap_change_per_yr <- (inc_gap_2023 - inc_gap_1969) / (2023 - 1969)
inc_convergence_yr    <- round(2023 + (0 - inc_gap_2023) / inc_gap_change_per_yr)

# HH vs clan wealth gap at endpoints
w_gap_1984      <- w_hh_1984 - w_cl_1984
w_gap_2023      <- w_hh_2023 - w_cl_2023

# Aggregate Gini averages (ALL row) and HH vs clan differences (used in inline text)
avg_inc_hh      <- inc_by_year    %>% filter(year == "ALL") %>% pull(r_hh_w_inc)
avg_inc_cl      <- inc_by_year    %>% filter(year == "ALL") %>% pull(r_cl_w_inc)
avg_w_hh        <- wealth_by_year %>% filter(year == "ALL") %>% pull(r_hh_w_wealth)
avg_w_cl        <- wealth_by_year %>% filter(year == "ALL") %>% pull(r_cl_w_wealth)
inc_gini_diff    <- avg_inc_hh - avg_inc_cl
inc_gini_pct_red <- 100 * inc_gini_diff / avg_inc_hh
w_gini_diff      <- avg_w_hh - avg_w_cl
w_gini_pct_red   <- 100 * w_gini_diff / avg_w_hh

inc_mean_hh_w <- summary %>% filter(Table == "Income", Unit == "Household") %>% pull(mean_val_w) %>% first()
inc_mean_cl_w <- summary %>% filter(Table == "Income", Unit == "Clan")      %>% pull(mean_val_w) %>% first()
w_mean_hh_w   <- summary %>% filter(Table == "Wealth", Unit == "Household") %>% pull(mean_val_w) %>% first()
w_mean_cl_w   <- summary %>% filter(Table == "Wealth", Unit == "Clan")      %>% pull(mean_val_w) %>% first()


# Figure 1
fig1_note <- sprintf(
  paste0(
    "Note: Gini coefficients are estimated from weighted data using PSID family and clan weights. ",
    "Weighted mean income is $%s for households and $%s for clans. ",
    "Weighted mean wealth is $%s for households and $%s for clans (wealth includes home equity). ",
    "The Gini coefficient for income for households rose by %.1f%% (%.2f in 1969 and %.2f in 2023). ",
    "The Gini coefficient for income for clans rose by %.1f%% (%.2f in 1969 and %.2f in 2023). ",
    "The Gini coefficient for wealth for households rose by %.1f%% (%.2f in 1984 and %.2f in 2023). ",
    "The Gini coefficient for wealth for clans rose by %.1f%% (%.2f in 1984 and %.2f in 2023)."
  ),
  fmt_money0(inc_mean_hh_w), fmt_money0(inc_mean_cl_w),
  fmt_money0(w_mean_hh_w),   fmt_money0(w_mean_cl_w),
  pct_change(inc_hh_1969, inc_hh_2023), inc_hh_1969, inc_hh_2023,
  pct_change(inc_cl_1969, inc_cl_2023), inc_cl_1969, inc_cl_2023,
  pct_change(w_hh_1984,   w_hh_2023),  w_hh_1984,   w_hh_2023,
  pct_change(w_cl_1984,   w_cl_2023),  w_cl_1984,   w_cl_2023
)

shared_y <- {
  vals <- c(
    inc_by_year    %>% filter(year != "ALL") %>% pull(r_hh_w_inc),
    inc_by_year    %>% filter(year != "ALL") %>% pull(r_cl_w_inc),
    wealth_by_year %>% filter(year != "ALL") %>% pull(r_hh_w_wealth),
    wealth_by_year %>% filter(year != "ALL") %>% pull(r_cl_w_wealth)
  )
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

fig1_inc <- make_gini_plot(inc_by_year,    r_hh_w_inc,    r_cl_w_inc,    "Gini Coefficient", y_limits = shared_y)
fig1_w   <- make_gini_plot(wealth_by_year, r_hh_w_wealth, r_cl_w_wealth, "Gini Coefficient", y_limits = shared_y)

fig1_title <- textGrob(
  "Figure 1. Income and Wealth Inequality Over Time",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
)
fig1_subA <- textGrob(
  "Panel A: Income inequality from 1969 to 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size)
)
fig1_subB <- textGrob(
  "Panel B: Wealth inequality from 1984 to 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size)
)

fig1_panelA <- arrangeGrob(fig1_subA, fig1_inc, ncol = 1, heights = c(1, 12))
fig1_panelB <- arrangeGrob(fig1_subB, fig1_w,   ncol = 1, heights = c(1, 12))

fig1 <- arrangeGrob(
  fig1_title,
  arrangeGrob(fig1_panelA, fig1_panelB, ncol = 2),
  ncol = 1,
  heights = unit(c(1.2, 7.8), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure1.pdf"), fig1, width = 14, height = 9)
  message("Saved: figure1.pdf")
}

# Figure 2
fig2_inc <- make_lorenz_plot(
  r_hh, r_clans, "inc_all", years = c(1984, 2023),
  ylab = "Cumulative Proportion of Income", colors = c("1984" = "#4a71c7", "2023" = "#E66101")
)
fig2_w <- make_lorenz_plot(
  r_hh_wealth, r_clans_wealth, "wealth", years = c(1984, 2023),
  ylab = "Cumulative Proportion of Wealth", colors = c("1984" = "#4a71c7", "2023" = "#E66101")
)

inc_stats_hh <- r_hh %>%
  filter(year %in% c(1984, 2023), is.finite(inc_all), is.finite(fam_weight), fam_weight > 0) %>%
  group_by(year) %>%
  summarise(mean = wtd_mean(inc_all, fam_weight), med = wtd_median(inc_all, fam_weight), .groups = "drop")

inc_stats_cl <- r_clans %>%
  filter(year %in% c(1984, 2023), is.finite(inc_all), is.finite(clan_weight), clan_weight > 0) %>%
  group_by(year) %>%
  summarise(mean = wtd_mean(inc_all, clan_weight), med = wtd_median(inc_all, clan_weight), .groups = "drop")

fig2_note <- sprintf(
  paste0(
    "Note: Lorenz curves are estimated from weighted data using PSID family and clan weights. ",
    "Wealth is measured including home equity. ",
    "For income in 1984, the weighted mean (median) is $%s ($%s) for households and $%s ($%s) for clans. ",
    "For income in 2023, the weighted mean (median) is $%s ($%s) for households and $%s ($%s) for clans."
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

fig2_title <- textGrob(
  "Figure 2. Lorenz Curves at the Household and Clan Levels",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
)
fig2_subA <- textGrob(
  "Panel A: Distribution of income in 1984 and 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size)
)
fig2_subB <- textGrob(
  "Panel B: Distribution of wealth in 1984 and 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size)
)

fig2_panelA <- arrangeGrob(fig2_subA, fig2_inc, ncol = 1, heights = c(1, 12))
fig2_panelB <- arrangeGrob(fig2_subB, fig2_w,   ncol = 1, heights = c(1, 12))

fig2 <- arrangeGrob(
  fig2_title,
  arrangeGrob(fig2_panelA, fig2_panelB, ncol = 2),
  ncol = 1,
  heights = unit(c(1.2, 7.8), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure2.pdf"), fig2, width = 14, height = 9)
  message("Saved: figure2.pdf")
}

# Table 1
inc_race <- read_csv(here("7_gini_by_race", "output", "income_race.csv"),         show_col_types = FALSE) %>% filter(year == "ALL")
w_race   <- read_csv(here("7_gini_by_race", "output", "wealth_withhome_race.csv"), show_col_types = FALSE) %>% filter(year == "ALL")

# Race-specific Gini scalars for inline text
inc_diff_black    <- inc_race$r_hh_w_inc_black    - inc_race$r_cl_w_inc_black
inc_diff_nonblack <- inc_race$r_hh_w_inc_nonblack - inc_race$r_cl_w_inc_nonblack
w_diff_black      <- w_race$r_hh_w_wealth_black   - w_race$r_cl_w_wealth_black
w_diff_nonblack   <- w_race$r_hh_w_wealth_nonblack - w_race$r_cl_w_wealth_nonblack

inc_hh_counts <- r_hh %>%
  filter(is.finite(inc_all), is.finite(fam_weight), fam_weight > 0, !is.na(black_head)) %>%
  summarise(black = sum(black_head == 1), nonblack = sum(black_head == 0))

inc_cl_counts <- r_clans %>%
  filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0, !is.na(black_clan)) %>%
  summarise(
    black = sum(black_clan == 1), nonblack = sum(black_clan == 0),
    uniq_black = n_distinct(id1968[black_clan == 1]), uniq_nonblack = n_distinct(id1968[black_clan == 0])
  )

w_hh_counts <- r_hh_wealth %>%
  filter(is.finite(wealth), is.finite(fam_weight), fam_weight > 0, !is.na(black_head)) %>%
  summarise(black = sum(black_head == 1), nonblack = sum(black_head == 0))

w_cl_counts <- r_clans_wealth %>%
  filter(is.finite(wealth), is.finite(clan_weight), clan_weight > 0, !is.na(black_clan)) %>%
  summarise(
    black = sum(black_clan == 1), nonblack = sum(black_clan == 0),
    uniq_black = n_distinct(id1968[black_clan == 1]), uniq_nonblack = n_distinct(id1968[black_clan == 0])
  )

inc_total_clans  <- r_clans %>% filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0) %>% summarise(n = n_distinct(id1968)) %>% pull(n)
inc_black_ids    <- r_clans %>% filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0, black_clan == 1) %>% distinct(id1968)
inc_nonblack_ids <- r_clans %>% filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0, black_clan == 0) %>% distinct(id1968)
inc_overlap      <- nrow(inner_join(inc_black_ids, inc_nonblack_ids, by = "id1968"))

w_total_clans  <- r_clans_wealth %>% filter(is.finite(wealth), is.finite(clan_weight), clan_weight > 0) %>% summarise(n = n_distinct(id1968)) %>% pull(n)
w_black_ids    <- r_clans_wealth %>% filter(is.finite(wealth), is.finite(clan_weight), clan_weight > 0, black_clan == 1) %>% distinct(id1968)
w_nonblack_ids <- r_clans_wealth %>% filter(is.finite(wealth), is.finite(clan_weight), clan_weight > 0, black_clan == 0) %>% distinct(id1968)
w_overlap      <- nrow(inner_join(w_black_ids, w_nonblack_ids, by = "id1968"))

tbl1 <- tribble(
  ~Measure,
  ~`Black HH`, ~`Black Clans`, ~`Diff. (Black)`,
  ~`Non-Black HH`, ~`Non-Black Clans`, ~`Diff. (Non-Black)`,

  "Income",
  fmt_se(inc_race$r_hh_w_inc_black,    inc_race$r_hh_w_inc_black_se),
  fmt_se(inc_race$r_cl_w_inc_black,    inc_race$r_cl_w_inc_black_se),
  fmt_se(inc_race$r_hh_w_inc_black    - inc_race$r_cl_w_inc_black,
         sqrt(inc_race$r_hh_w_inc_black_se^2    + inc_race$r_cl_w_inc_black_se^2)),
  fmt_se(inc_race$r_hh_w_inc_nonblack, inc_race$r_hh_w_inc_nonblack_se),
  fmt_se(inc_race$r_cl_w_inc_nonblack, inc_race$r_cl_w_inc_nonblack_se),
  fmt_se(inc_race$r_hh_w_inc_nonblack - inc_race$r_cl_w_inc_nonblack,
         sqrt(inc_race$r_hh_w_inc_nonblack_se^2 + inc_race$r_cl_w_inc_nonblack_se^2)),

  "Wealth",
  fmt_se(w_race$r_hh_w_wealth_black,    w_race$r_hh_w_wealth_black_se),
  fmt_se(w_race$r_cl_w_wealth_black,    w_race$r_cl_w_wealth_black_se),
  fmt_se(w_race$r_hh_w_wealth_black    - w_race$r_cl_w_wealth_black,
         sqrt(w_race$r_hh_w_wealth_black_se^2    + w_race$r_cl_w_wealth_black_se^2)),
  fmt_se(w_race$r_hh_w_wealth_nonblack, w_race$r_hh_w_wealth_nonblack_se),
  fmt_se(w_race$r_cl_w_wealth_nonblack, w_race$r_cl_w_wealth_nonblack_se),
  fmt_se(w_race$r_hh_w_wealth_nonblack - w_race$r_cl_w_wealth_nonblack,
         sqrt(w_race$r_hh_w_wealth_nonblack_se^2 + w_race$r_cl_w_wealth_nonblack_se^2))
)

tbl1_note <- sprintf(
  paste0(
    "Note: Gini coefficients reported are averages across all years using weighted data. ",
    "Income data were collected annually until 1997, and biennially thereafter. ",
    "Wealth data were collected every five years from 1984 to 1999, and every other year thereafter. ",
    "Both income and wealth are adjusted for inflation. Wealth is measured including home equity. ",
    "Standard errors are shown in parentheses. ",
    "Income estimates use %s Black household-years and %s Black clan-years, and %s Non-Black household-years and %s Non-Black clan-years, ",
    "with %s unique clans in total; %s clans ever classified as Black and %s clans ever classified as Non-Black, ",
    "including %s clans appearing in both groups across years. ",
    "Wealth estimates use %s Black household-years and %s Black clan-years, and %s Non-Black household-years and %s Non-Black clan-years, ",
    "with %s unique clans in total; %s clans ever classified as Black and %s clans ever classified as Non-Black, ",
    "including %s clans appearing in both groups across years."
  ),
  fmt_int(inc_hh_counts$black),    fmt_int(inc_cl_counts$black),
  fmt_int(inc_hh_counts$nonblack), fmt_int(inc_cl_counts$nonblack),
  fmt_int(inc_total_clans),        fmt_int(nrow(inc_black_ids)),
  fmt_int(nrow(inc_nonblack_ids)), fmt_int(inc_overlap),
  fmt_int(w_hh_counts$black),      fmt_int(w_cl_counts$black),
  fmt_int(w_hh_counts$nonblack),   fmt_int(w_cl_counts$nonblack),
  fmt_int(w_total_clans),          fmt_int(nrow(w_black_ids)),
  fmt_int(nrow(w_nonblack_ids)),   fmt_int(w_overlap)
)

doc1 <- read_docx() %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(
    flextable(tbl1) %>%
      set_caption("Table 1. Differences in Inequality by Race") %>%
      theme_vanilla() %>%
      bold(part = "header") %>%
      align(align = "center", part = "all") %>%
      fontsize(size = 12, part = "all") %>%
      fontsize(size = 10, part = "body") %>%
      autofit()
  ) %>%
  body_add_fpar(fpar(ftext(tbl1_note, prop = note_style), fp_p = fp_par(text.align = "center")))

if (SAVE_FILES) {
  print(doc1, target = here("9_figures", "output", "table1.docx"))
  message("Saved: table1.docx")
}


# Size Standardization 
options(survey.lonely.psu = "adjust")

inc_current <- inc_by_year %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year)) %>%
  select(year, current_hh = r_hh_w_inc, current_clan = r_cl_w_inc)

wealth_current <- wealth_by_year %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year)) %>%
  select(year, current_hh = r_hh_w_wealth, current_clan = r_cl_w_wealth)

# Method 1: divide by size (numfu for HH; numclan for clans)
r_hh_m1         <- r_hh          %>% mutate(inc_std    = inc_all          / numfu)
r_clans_m1      <- r_clans       %>% mutate(inc_std    = inc_all          / numclan)
r_hh_wealth_m1  <- r_hh_wealth   %>% mutate(wealth_std = wealth_nohouse   / numfu)
r_clans_wealth_m1 <- r_clans_wealth %>% mutate(wealth_std = wealth_nohouse / numclan)

# Method 2: divide by sqrt of people (numfu for HH; num_clan_people for clans)
r_hh_m2         <- r_hh          %>% mutate(inc_std    = inc_all          / sqrt(numfu))
r_clans_m2      <- r_clans       %>% mutate(inc_std    = inc_all          / sqrt(num_clan_people))
r_hh_wealth_m2  <- r_hh_wealth   %>% mutate(wealth_std = wealth_nohouse   / sqrt(numfu))
r_clans_wealth_m2 <- r_clans_wealth %>% mutate(wealth_std = wealth_nohouse / sqrt(num_clan_people))

# Compute Ginis for each standardized series
inc_std <- reduce(
  list(
    run_gini(r_hh_m1,    "inc_std", "fam_weight",  FALSE, FALSE, "m1_hh"),
    run_gini(r_clans_m1, "inc_std", "clan_weight", FALSE, FALSE, "m1_clan"),
    run_gini(r_hh_m2,    "inc_std", "fam_weight",  FALSE, FALSE, "m2_hh"),
    run_gini(r_clans_m2, "inc_std", "clan_weight", FALSE, FALSE, "m2_clan")
  ),
  full_join, by = "year"
)

wealth_std <- reduce(
  list(
    run_gini(r_hh_wealth_m1,    "wealth_std", "fam_weight",  FALSE, FALSE, "m1_hh"),
    run_gini(r_clans_wealth_m1, "wealth_std", "clan_weight", FALSE, FALSE, "m1_clan"),
    run_gini(r_hh_wealth_m2,    "wealth_std", "fam_weight",  FALSE, FALSE, "m2_hh"),
    run_gini(r_clans_wealth_m2, "wealth_std", "clan_weight", FALSE, FALSE, "m2_clan")
  ),
  full_join, by = "year"
)

# Join with current (unstandardized) estimates
inc_size_tbl <- inc_current %>%
  left_join(inc_std, by = "year") %>%
  select(year, m1_hh, m1_clan, m2_hh, m2_clan, current_hh, current_clan) %>%
  arrange(year)

wealth_size_tbl <- wealth_current %>%
  left_join(wealth_std, by = "year") %>%
  select(year, m1_hh, m1_clan, m2_hh, m2_clan, current_hh, current_clan) %>%
  arrange(year)

if (SAVE_FILES) {
  write.csv(inc_size_tbl,    here("9_figures", "output", "income_size_standardized.csv"),  row.names = FALSE)
  write.csv(wealth_size_tbl, here("9_figures", "output", "wealth_size_standardized.csv"),  row.names = FALSE)
  message("Saved: income_size_standardized.csv")
  message("Saved: wealth_size_standardized.csv")
}