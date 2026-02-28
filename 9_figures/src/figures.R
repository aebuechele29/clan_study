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

inc_by_year    <- read_csv(here("6_calculate_gini", "output", "income.csv"),               show_col_types = FALSE)
wealth_by_year <- read_csv(here("6_calculate_gini", "output", "wealth_withhome.csv"),       show_col_types = FALSE)
summary        <- read_csv(here("5_summary",         "output", "summary_statistics.csv"),  show_col_types = FALSE)

# Global styles
base_family <- "serif"
base_size   <- 22
title_size  <- 26
sub_size    <- 24
note_size   <- 14

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

get_gini_at <- function(df, year_value, col) {
  df %>%
    filter(year != "ALL") %>%
    mutate(year = as.numeric(year)) %>%
    filter(year == year_value) %>%
    pull({{ col }}) %>%
    first()
}

note_grob <- function(txt, width = 110, size = note_size, family = base_family) {
  wrapped <- paste(strwrap(txt, width = width), collapse = "\n")
  textGrob(
    wrapped,
    x = unit(0.01, "npc"), just = "left",
    gp = gpar(fontfamily = family, fontface = "italic", fontsize = size),
    default.units = "npc"
  )
}

make_x_breaks <- function(min_yr, max_yr) {
  br <- seq(ceiling(min_yr / 10) * 10, floor(max_yr / 10) * 10, by = 10)
  br <- br[br > (min_yr + 4) & br < (max_yr - 4)]
  sort(unique(c(min_yr, br, max_yr)))
}

# make_gini_plot: line plot of Gini over time (used for Figure 1)
make_gini_plot <- function(by_year_df, hh_col, cl_col, ylab, y_limits = NULL) {
  dat <- by_year_df %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))
  br  <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  if (is.null(y_limits)) {
    vals <- c(pull(dat, {{ hh_col }}), pull(dat, {{ cl_col }}))
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  ggplot(dat, aes(x = year)) +
    geom_line(aes(y = {{ hh_col }}, linetype = "Household"), color = "#E66101", linewidth = 1.7) +
    geom_line(aes(y = {{ cl_col }}, linetype = "Clan"),      color = "#FDB863", linewidth = 1.7) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    labs(x = "Year", y = ylab, linetype = NULL) +
    theme(legend.position = "bottom", plot.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(linetype = guide_legend(
      override.aes = list(color = c("#FDB863", "#E66101"), linewidth = 1.2)
    ))
}

# =============================================================================
# make_lorenz_plot: Lorenz curves (Figure 2)
# Fix: use a single combined scale so color (year) and linetype (unit) appear
# in one clean shared legend instead of two duplicate ones.
# =============================================================================
make_lorenz_plot <- function(df_hh, df_cl, value_var, years, ylab, colors) {
  dat <- bind_rows(
    get_lorenz_weighted(df_hh, value_var, "fam_weight",  years, "Household"),
    get_lorenz_weighted(df_cl, value_var, "clan_weight", years, "Clan")
  ) %>%
    mutate(
      year_chr  = as.character(year),
      # Create a combined series label for the legend
      series = paste0(year_chr, ": ", as.character(Unit))
    )

  # Build colour/linetype mappings from the combinations actually present
  yr_labels   <- as.character(sort(unique(years)))
  series_keys <- c(
    paste0(yr_labels[1], ": Household"),
    paste0(yr_labels[1], ": Clan"),
    paste0(yr_labels[2], ": Household"),
    paste0(yr_labels[2], ": Clan")
  )
  series_colors    <- setNames(
    c(colors[yr_labels[1]], colors[yr_labels[1]], colors[yr_labels[2]], colors[yr_labels[2]]),
    series_keys
  )
  series_linetypes <- setNames(
    c("solid", "dotted", "solid", "dotted"),
    series_keys
  )

  ggplot(dat, aes(x = p, y = L, color = series, linetype = series, group = series)) +
    geom_line(linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = series_colors, name = NULL) +
    scale_linetype_manual(values = series_linetypes, name = NULL) +
    labs(x = "Cumulative Proportion of Units", y = ylab) +
    theme(legend.position = "bottom", plot.title = element_blank()) +
    guides(
      color    = guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)),
      linetype = guide_legend(nrow = 2)
    )
}

# =============================================================================
# make_race_plot: Figure 3 panels (Black vs Non-Black, HH vs Clan)
# =============================================================================
make_race_plot <- function(dat, hh_black, cl_black, hh_nonblack, cl_nonblack, ylab, y_limits = NULL) {

  hh_b  <- deparse(substitute(hh_black))
  cl_b  <- deparse(substitute(cl_black))
  hh_nb <- deparse(substitute(hh_nonblack))
  cl_nb <- deparse(substitute(cl_nonblack))

  long <- dat %>%
    select(year, all_of(c(hh_b, cl_b, hh_nb, cl_nb))) %>%
    pivot_longer(-year, names_to = "series", values_to = "gini") %>%
    mutate(series = recode(series,
      !!hh_b  := "Black: Household",
      !!cl_b  := "Black: Clan",
      !!hh_nb := "Non-Black: Household",
      !!cl_nb := "Non-Black: Clan"
    ))

  if (is.null(y_limits)) {
    vals <- long$gini[is.finite(long$gini)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  br <- make_x_breaks(min(long$year, na.rm = TRUE), max(long$year, na.rm = TRUE))

  series_colors    <- c("Black: Household"     = "#4a71c7",
                        "Black: Clan"          = "#4a71c7",
                        "Non-Black: Household" = "#E66101",
                        "Non-Black: Clan"      = "#E66101")
  series_linetypes <- c("Black: Household"     = "solid",
                        "Black: Clan"          = "dotted",
                        "Non-Black: Household" = "solid",
                        "Non-Black: Clan"      = "dotted")

  ggplot(long, aes(x = year, y = gini, color = series, linetype = series, group = series)) +
    geom_line(linewidth = 1.7) +
    scale_color_manual(values = series_colors) +
    scale_linetype_manual(values = series_linetypes) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = ylab, color = NULL, linetype = NULL) +
    theme(legend.position = "bottom", plot.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
}

# =============================================================================
# make_race_diff_plot: Panel C for Figure 3 — HH minus Clan gap by race
# Black gap = blue (matching make_race_plot), Non-Black gap = orange
# =============================================================================
make_race_diff_plot <- function(dat, hh_black, cl_black, hh_nonblack, cl_nonblack,
                                y_limits = NULL) {
  hh_b  <- deparse(substitute(hh_black))
  cl_b  <- deparse(substitute(cl_black))
  hh_nb <- deparse(substitute(hh_nonblack))
  cl_nb <- deparse(substitute(cl_nonblack))

  diff_dat <- dat %>%
    transmute(
      year,
      `Black`     = .data[[hh_b]]  - .data[[cl_b]],
      `Non-Black` = .data[[hh_nb]] - .data[[cl_nb]]
    ) %>%
    pivot_longer(-year, names_to = "Race", values_to = "Difference")

  if (is.null(y_limits)) {
    vals <- diff_dat$Difference[is.finite(diff_dat$Difference)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  br <- make_x_breaks(min(diff_dat$year, na.rm = TRUE), max(diff_dat$year, na.rm = TRUE))

  ggplot(diff_dat, aes(x = year, y = Difference, color = Race, group = Race)) +
    geom_line(linewidth = 1.7) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    scale_color_manual(values = c("Black" = "#4a71c7", "Non-Black" = "#E66101"), name = NULL) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = "HH - Clan Gini", color = NULL) +
    theme(legend.position = "bottom", plot.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)))
}

# =============================================================================
# make_sensitivity_plot:
#   3-panel plot: Left = main (orange), Middle = alternative (blue),
#   Right = HH-Clan diff for both (orange line = main, blue line = alt).
#   One shared legend across all three panels.
# =============================================================================
make_sensitivity_plot <- function(df,
                                  main_hh, main_cl,
                                  alt_hh,  alt_cl,
                                  left_label  = "Main Results",
                                  right_label = "Alternative",
                                  y_limits    = NULL,
                                  diff_limits = NULL) {

  dat <- df %>%
    filter(year != "ALL") %>%
    mutate(year = as.numeric(year)) %>%
    rename(
      main_hh_  = all_of(main_hh),
      main_cl_  = all_of(main_cl),
      alt_hh_   = all_of(alt_hh),
      alt_cl_   = all_of(alt_cl)
    ) %>%
    mutate(
      main_diff = main_hh_ - main_cl_,
      alt_diff  = alt_hh_  - alt_cl_
    )

  if (is.null(y_limits)) {
    vals <- c(dat$main_hh_, dat$main_cl_, dat$alt_hh_, dat$alt_cl_)
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(dat$main_diff, dat$alt_diff)
    vals <- vals[is.finite(vals)]
    diff_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  # Colour scheme: Panel A = orange, Panel B = blue
  main_hh_col  <- "#E66101"
  main_cl_col  <- "#FDB863"
  alt_hh_col   <- "#4a71c7"
  alt_cl_col   <- "#92aee0"

  base_theme <- theme(
    legend.position = "none",
    plot.title      = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )
  sub_theme <- theme(plot.subtitle = element_text(size = sub_size * 0.6, hjust = 0.5))

  # ── Panel A: main results (orange) ────────────────────────────────────────
  long_main <- dat %>%
    select(year, Household = main_hh_, Clan = main_cl_) %>%
    pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pA <- ggplot(long_main, aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    geom_line(linewidth = 1.7) +
    scale_color_manual(values = c("Household" = main_hh_col, "Clan" = main_cl_col)) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = "Gini Coefficient", color = NULL, linetype = NULL,
         subtitle = left_label) +
    base_theme + sub_theme

  # ── Panel B: alternative (blue) ───────────────────────────────────────────
  long_alt <- dat %>%
    select(year, Household = alt_hh_, Clan = alt_cl_) %>%
    pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pB <- ggplot(long_alt, aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    geom_line(linewidth = 1.7) +
    scale_color_manual(values = c("Household" = alt_hh_col, "Clan" = alt_cl_col)) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = NULL, color = NULL, linetype = NULL,
         subtitle = right_label) +
    base_theme + sub_theme

  # ── Panel C: HH-Clan difference, one line per spec ────────────────────────
  # Main diff = orange, Alt diff = blue (matching their respective panels)
  long_diff <- dat %>%
    select(year, `Main (HH-Clan)` = main_diff, `Alt (HH-Clan)` = alt_diff) %>%
    pivot_longer(-year, names_to = "Specification", values_to = "Difference")

  diff_colors    <- c("Main (HH-Clan)" = main_hh_col, "Alt (HH-Clan)" = alt_hh_col)
  diff_linetypes <- c("Main (HH-Clan)" = "solid",     "Alt (HH-Clan)" = "solid")

  pC <- ggplot(long_diff,
               aes(x = year, y = Difference, color = Specification, linetype = Specification)) +
    geom_line(linewidth = 1.7) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    scale_color_manual(values = diff_colors) +
    scale_linetype_manual(values = diff_linetypes) +
    scale_y_continuous(limits = diff_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = "HH - Clan Gini", color = NULL, linetype = NULL,
         subtitle = "HH - Clan Difference") +
    base_theme + sub_theme

  # ── Shared legend ──────────────────────────────────────────────────────────
  # Donor plot encodes all 6 series so the legend shows them together
  legend_df <- bind_rows(
    long_main %>% mutate(group = paste0("Main: ", Unit)),
    long_alt  %>% mutate(group = paste0("Alt: ",  Unit)),
    long_diff %>% rename(Gini = Difference) %>% mutate(group = Specification)
  )

  legend_colors <- c(
    "Main: Household" = main_hh_col,
    "Main: Clan"      = main_cl_col,
    "Alt: Household"  = alt_hh_col,
    "Alt: Clan"       = alt_cl_col,
    "Main (HH-Clan)"  = main_hh_col,
    "Alt (HH-Clan)"   = alt_hh_col
  )
  legend_linetypes <- c(
    "Main: Household" = "solid",
    "Main: Clan"      = "dotted",
    "Alt: Household"  = "solid",
    "Alt: Clan"       = "dotted",
    "Main (HH-Clan)"  = "solid",
    "Alt (HH-Clan)"   = "solid"
  )

  donor <- ggplot(legend_df, aes(x = year, y = Gini, color = group, linetype = group)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = legend_colors, name = NULL,
                       breaks = c("Main: Household", "Main: Clan",
                                  "Alt: Household",  "Alt: Clan",
                                  "Main (HH-Clan)",  "Alt (HH-Clan)")) +
    scale_linetype_manual(values = legend_linetypes, name = NULL,
                          breaks = c("Main: Household", "Main: Clan",
                                     "Alt: Household",  "Alt: Clan",
                                     "Main (HH-Clan)",  "Alt (HH-Clan)")) +
    theme(legend.position = "bottom") +
    guides(color    = guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)),
           linetype = guide_legend(nrow = 2))

  shared_legend <- cowplot::get_legend(donor)

  panels <- cowplot::plot_grid(pA, pB, pC, nrow = 1, rel_widths = c(1, 1, 1))
  cowplot::plot_grid(panels, shared_legend, ncol = 1, rel_heights = c(10, 1.5))
}

# Helper: stack income + wealth sensitivity rows under a title
make_sensitivity_figure <- function(plot_inc, plot_w, title_str,
                                    sub_a = "Panel A: Income",
                                    sub_b = "Panel B: Wealth") {
  title_grob <- ggdraw() +
    draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
               fontfamily = base_family, size = title_size)
  sub_a_grob <- ggdraw() +
    draw_label(sub_a, x = 0, hjust = 0, fontfamily = base_family, size = sub_size)
  sub_b_grob <- ggdraw() +
    draw_label(sub_b, x = 0, hjust = 0, fontfamily = base_family, size = sub_size)

  cowplot::plot_grid(
    title_grob,
    sub_a_grob, plot_inc,
    sub_b_grob, plot_w,
    ncol = 1,
    rel_heights = c(0.08, 0.05, 1, 0.05, 1)
  )
}

# =============================================================================
# Scalars for inline text
# =============================================================================
inc_hh_1969   <- get_gini_at(inc_by_year,    1969, r_hh_w_inc)
inc_hh_2023   <- get_gini_at(inc_by_year,    2023, r_hh_w_inc)
inc_cl_1969   <- get_gini_at(inc_by_year,    1969, r_cl_w_inc)
inc_cl_2023   <- get_gini_at(inc_by_year,    2023, r_cl_w_inc)
w_hh_1984     <- get_gini_at(wealth_by_year, 1984, r_hh_w_wealth)
w_hh_2023     <- get_gini_at(wealth_by_year, 2023, r_hh_w_wealth)
w_cl_1984     <- get_gini_at(wealth_by_year, 1984, r_cl_w_wealth)
w_cl_2023     <- get_gini_at(wealth_by_year, 2023, r_cl_w_wealth)

inc_hh_pct <- pct_change(inc_hh_1969, inc_hh_2023)
inc_cl_pct <- pct_change(inc_cl_1969, inc_cl_2023)
w_hh_pct   <- pct_change(w_hh_1984,  w_hh_2023)
w_cl_pct   <- pct_change(w_cl_1984,  w_cl_2023)

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
w_gini_diff      <- avg_w_hh - avg_w_cl
w_gini_pct_red   <- 100 * w_gini_diff / avg_w_hh

inc_mean_hh_w <- summary %>% filter(Table == "Income", Unit == "Household") %>% pull(mean_val_w) %>% first()
inc_mean_cl_w <- summary %>% filter(Table == "Income", Unit == "Clan")      %>% pull(mean_val_w) %>% first()
w_mean_hh_w   <- summary %>% filter(Table == "Wealth", Unit == "Household") %>% pull(mean_val_w) %>% first()
w_mean_cl_w   <- summary %>% filter(Table == "Wealth", Unit == "Clan")      %>% pull(mean_val_w) %>% first()

# =============================================================================
# Figure 1
# =============================================================================
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

fig1_title <- textGrob("Figure 1. Income and Wealth Inequality Over Time",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size))
fig1_subA  <- textGrob("Panel A: Income inequality from 1969 to 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size))
fig1_subB  <- textGrob("Panel B: Wealth inequality from 1984 to 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size))

fig1 <- arrangeGrob(
  fig1_title,
  arrangeGrob(
    arrangeGrob(fig1_subA, fig1_inc, ncol = 1, heights = c(1, 12)),
    arrangeGrob(fig1_subB, fig1_w,   ncol = 1, heights = c(1, 12)),
    ncol = 2
  ),
  ncol = 1, heights = unit(c(0.5, 9.8), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure1.pdf"), fig1, width = 14, height = 11)
  message("Saved: figure1.pdf")
}

# =============================================================================
# Figure 2 — Lorenz curves
# Fixed: single combined legend (year × unit), no duplicate legends
# =============================================================================
fig2_inc <- make_lorenz_plot(
  r_hh, r_clans, "inc_all", years = c(1984, 2023),
  ylab = "Cumulative Proportion of Income",
  colors = c("1984" = "#4a71c7", "2023" = "#E66101")
)
fig2_w <- make_lorenz_plot(
  r_hh_wealth, r_clans_wealth, "wealth", years = c(1984, 2023),
  ylab = "Cumulative Proportion of Wealth",
  colors = c("1984" = "#4a71c7", "2023" = "#E66101")
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
    "Wealth is measured including home equity. Solid lines = households; dotted lines = clans. ",
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

fig2_title <- textGrob("Figure 2. Lorenz Curves at the Household and Clan Levels",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size))
fig2_subA  <- textGrob("Panel A: Distribution of income in 1984 and 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size))
fig2_subB  <- textGrob("Panel B: Distribution of wealth in 1984 and 2023",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontsize = sub_size))

fig2 <- arrangeGrob(
  fig2_title,
  arrangeGrob(
    arrangeGrob(fig2_subA, fig2_inc, ncol = 1, heights = c(1, 12)),
    arrangeGrob(fig2_subB, fig2_w,   ncol = 1, heights = c(1, 12)),
    ncol = 2
  ),
  ncol = 1, heights = unit(c(0.5, 7.8), "inches")
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figure2.pdf"), fig2, width = 14, height = 9)
  message("Saved: figure2.pdf")
}

# =============================================================================
# sensitivity_gini_6 — Race (moved from Figure 3 to appendix)
# Layout mirrors other sensitivity figures: two rows (income, wealth), each
# with 3 panels: left = Black (orange), middle = Non-Black (blue),
# right = HH-Clan diff for both races.
# =============================================================================
inc_race_yr <- read_csv(here("7_gini_by_race", "output", "income_race.csv"),
                        show_col_types = FALSE) %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year))

w_race_yr <- read_csv(here("7_gini_by_race", "output", "wealth_withhome_race.csv"),
                      show_col_types = FALSE) %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year))

inc_race <- read_csv(here("7_gini_by_race", "output", "income_race.csv"),
                     show_col_types = FALSE) %>% filter(year == "ALL")

w_race <- read_csv(here("7_gini_by_race", "output", "wealth_withhome_race.csv"),
                   show_col_types = FALSE) %>% filter(year == "ALL")

# Shared y limits across all HH/Clan lines
shared_y_race <- {
  vals <- c(
    inc_race_yr$r_hh_w_inc_black,     inc_race_yr$r_cl_w_inc_black,
    inc_race_yr$r_hh_w_inc_nonblack,  inc_race_yr$r_cl_w_inc_nonblack,
    w_race_yr$r_hh_w_wealth_black,    w_race_yr$r_cl_w_wealth_black,
    w_race_yr$r_hh_w_wealth_nonblack, w_race_yr$r_cl_w_wealth_nonblack
  )
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

shared_y_race_diff <- {
  vals <- c(
    inc_race_yr$r_hh_w_inc_black    - inc_race_yr$r_cl_w_inc_black,
    inc_race_yr$r_hh_w_inc_nonblack - inc_race_yr$r_cl_w_inc_nonblack,
    w_race_yr$r_hh_w_wealth_black    - w_race_yr$r_cl_w_wealth_black,
    w_race_yr$r_hh_w_wealth_nonblack - w_race_yr$r_cl_w_wealth_nonblack
  )
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

# make_race_sensitivity_plot: 3-panel row for one outcome (income or wealth)
# Left = Black (orange), Middle = Non-Black (blue), Right = HH-Clan diff by race
make_race_sensitivity_plot <- function(dat, hh_black, cl_black, hh_nonblack, cl_nonblack,
                                       y_limits = NULL, diff_limits = NULL,
                                       ylab = "Gini Coefficient") {
  hh_b  <- deparse(substitute(hh_black))
  cl_b  <- deparse(substitute(cl_black))
  hh_nb <- deparse(substitute(hh_nonblack))
  cl_nb <- deparse(substitute(cl_nonblack))

  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  # Colour scheme: Black panel = orange (main), Non-Black = blue (alt)
  black_hh_col    <- "#E66101"
  black_cl_col    <- "#FDB863"
  nonblack_hh_col <- "#4a71c7"
  nonblack_cl_col <- "#92aee0"

  base_theme <- theme(
    legend.position = "none",
    plot.title      = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )
  sub_theme <- theme(plot.subtitle = element_text(size = sub_size * 0.6, hjust = 0.5))

  # Compute y limits if not supplied
  if (is.null(y_limits)) {
    vals <- c(dat[[hh_b]], dat[[cl_b]], dat[[hh_nb]], dat[[cl_nb]])
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(dat[[hh_b]] - dat[[cl_b]], dat[[hh_nb]] - dat[[cl_nb]])
    vals <- vals[is.finite(vals)]
    diff_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  # Panel A: Black (orange)
  long_black <- dat %>%
    transmute(year, Household = .data[[hh_b]], Clan = .data[[cl_b]]) %>%
    pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pA <- ggplot(long_black, aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    geom_line(linewidth = 1.7) +
    scale_color_manual(values = c("Household" = black_hh_col, "Clan" = black_cl_col)) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = ylab, subtitle = "Black", color = NULL, linetype = NULL) +
    base_theme + sub_theme

  # Panel B: Non-Black (blue)
  long_nonblack <- dat %>%
    transmute(year, Household = .data[[hh_nb]], Clan = .data[[cl_nb]]) %>%
    pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pB <- ggplot(long_nonblack, aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    geom_line(linewidth = 1.7) +
    scale_color_manual(values = c("Household" = nonblack_hh_col, "Clan" = nonblack_cl_col)) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = NULL, subtitle = "Non-Black", color = NULL, linetype = NULL) +
    base_theme + sub_theme

  # Panel C: HH-Clan diff, Black = orange, Non-Black = blue
  diff_dat <- dat %>%
    transmute(
      year,
      Black     = .data[[hh_b]]  - .data[[cl_b]],
      `Non-Black` = .data[[hh_nb]] - .data[[cl_nb]]
    ) %>%
    pivot_longer(-year, names_to = "Race", values_to = "Difference")

  pC <- ggplot(diff_dat, aes(x = year, y = Difference, color = Race, group = Race)) +
    geom_line(linewidth = 1.7) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    scale_color_manual(values = c("Black" = black_hh_col, "Non-Black" = nonblack_hh_col),
                       name = NULL) +
    scale_y_continuous(limits = diff_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = "HH - Clan Gini", subtitle = "HH - Clan Difference",
         color = NULL) +
    base_theme + sub_theme

  # Shared legend
  legend_df <- bind_rows(
    long_black    %>% mutate(group = paste0("Black: ",     Unit)),
    long_nonblack %>% mutate(group = paste0("Non-Black: ", Unit)),
    diff_dat      %>% rename(Gini = Difference) %>% mutate(group = Race)
  )
  legend_colors <- c(
    "Black: Household"     = black_hh_col,
    "Black: Clan"          = black_cl_col,
    "Non-Black: Household" = nonblack_hh_col,
    "Non-Black: Clan"      = nonblack_cl_col,
    "Black"                = black_hh_col,
    "Non-Black"            = nonblack_hh_col
  )
  legend_linetypes <- c(
    "Black: Household"     = "solid",
    "Black: Clan"          = "dotted",
    "Non-Black: Household" = "solid",
    "Non-Black: Clan"      = "dotted",
    "Black"                = "solid",
    "Non-Black"            = "solid"
  )
  legend_breaks <- c("Black: Household", "Black: Clan",
                     "Non-Black: Household", "Non-Black: Clan",
                     "Black", "Non-Black")

  donor <- ggplot(legend_df, aes(x = year, y = Gini, color = group, linetype = group)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = legend_colors, name = NULL, breaks = legend_breaks) +
    scale_linetype_manual(values = legend_linetypes, name = NULL, breaks = legend_breaks) +
    theme(legend.position = "bottom") +
    guides(color    = guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)),
           linetype = guide_legend(nrow = 2))

  shared_legend <- cowplot::get_legend(donor)

  panels <- cowplot::plot_grid(pA, pB, pC, nrow = 1, rel_widths = c(1, 1, 1))
  cowplot::plot_grid(panels, shared_legend, ncol = 1, rel_heights = c(10, 1.5))
}

# Build the two rows
race6_inc <- make_race_sensitivity_plot(
  inc_race_yr,
  r_hh_w_inc_black, r_cl_w_inc_black,
  r_hh_w_inc_nonblack, r_cl_w_inc_nonblack,
  y_limits    = shared_y_race,
  diff_limits = shared_y_race_diff,
  ylab = "Gini Coefficient"
)

race6_w <- make_race_sensitivity_plot(
  w_race_yr,
  r_hh_w_wealth_black, r_cl_w_wealth_black,
  r_hh_w_wealth_nonblack, r_cl_w_wealth_nonblack,
  y_limits    = shared_y_race,
  diff_limits = shared_y_race_diff,
  ylab = "Gini Coefficient"
)

sensitivity_gini_6 <- make_sensitivity_figure(
  race6_inc, race6_w,
  title_str = "Appendix D6. Inequality by Race",
  sub_a     = "Panel A: Income",
  sub_b     = "Panel B: Wealth (incl. home equity)"
)

# Gap scalars for Rmd note
gap6_black_inc    <- round(mean(inc_race_yr$r_hh_w_inc_black    - inc_race_yr$r_cl_w_inc_black,    na.rm = TRUE), 3)
gap6_nonblack_inc <- round(mean(inc_race_yr$r_hh_w_inc_nonblack - inc_race_yr$r_cl_w_inc_nonblack, na.rm = TRUE), 3)
gap6_black_w      <- round(mean(w_race_yr$r_hh_w_wealth_black    - w_race_yr$r_cl_w_wealth_black,   na.rm = TRUE), 3)
gap6_nonblack_w   <- round(mean(w_race_yr$r_hh_w_wealth_nonblack - w_race_yr$r_cl_w_wealth_nonblack, na.rm = TRUE), 3)

# Inline scalars used in write.rmd body text (previously derived from Figure 3 block)
inc_diff_black    <- inc_race$r_hh_w_inc_black    - inc_race$r_cl_w_inc_black
inc_diff_nonblack <- inc_race$r_hh_w_inc_nonblack - inc_race$r_cl_w_inc_nonblack
w_diff_black      <- w_race$r_hh_w_wealth_black   - w_race$r_cl_w_wealth_black
w_diff_nonblack   <- w_race$r_hh_w_wealth_nonblack - w_race$r_cl_w_wealth_nonblack

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "sensitivity_gini_6.pdf"),
         sensitivity_gini_6, width = 14, height = 11)
  message("Saved: sensitivity_gini_6.pdf")
}

# =============================================================================
# Table 1
# =============================================================================
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
      theme_vanilla() %>% bold(part = "header") %>%
      align(align = "center", part = "all") %>%
      fontsize(size = 12, part = "all") %>% fontsize(size = 10, part = "body") %>%
      autofit()
  ) %>%
  body_add_fpar(fpar(ftext(tbl1_note, prop = note_style), fp_p = fp_par(text.align = "center")))

# =============================================================================
# Appendix C (figG in paper): C1/C2/C3
# 3 panels per row: HH, Clan, Diff. Shared legend at bottom.
# =============================================================================
inc_c123 <- read.csv(here("8_nuclear_family", "output", "income_C123.csv"))
wnh_c123 <- read.csv(here("8_nuclear_family", "output", "wealth_C123.csv"))

inc_c123_yr <- inc_c123 %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))
wnh_c123_yr <- wnh_c123 %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))

coef_colors <- c("C1 (Bonferroni)" = "#4a71c7", "C2 (Gini)" = "#E66101", "C3 (Upper Tail)" = "#33a02c")

make_c123_panel <- function(dat, y_limits = NULL, show_unit, ylab = "Inequality Coefficient") {
  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  if (show_unit == "Diff") {
    diff_dat <- dat %>%
      transmute(
        year,
        `C1 (Bonferroni)` = C1_hh - C1_clan,
        `C2 (Gini)`       = C2_hh - C2_clan,
        `C3 (Upper Tail)` = C3_hh - C3_clan
      ) %>%
      pivot_longer(-year, names_to = "Coefficient", values_to = "value")

    if (is.null(y_limits)) {
      vals <- diff_dat$value[is.finite(diff_dat$value)]
      y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }

    return(
      ggplot(diff_dat, aes(x = year, y = value, color = Coefficient, group = Coefficient)) +
        geom_line(linewidth = 1.7) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
        scale_color_manual(values = coef_colors) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
        labs(x = "Year", y = "HH - Clan", color = NULL) +
        theme(legend.position = "none", plot.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }

  # "Household" or "Clan": filter to one unit, 3 lines (C1/C2/C3)
  long <- dat %>%
    dplyr::select(year, C1_hh, C1_clan, C2_hh, C2_clan, C3_hh, C3_clan) %>%
    pivot_longer(-year, names_to = "series", values_to = "value") %>%
    dplyr::mutate(
      Coefficient = dplyr::case_when(
        grepl("C1", series) ~ "C1 (Bonferroni)",
        grepl("C2", series) ~ "C2 (Gini)",
        grepl("C3", series) ~ "C3 (Upper Tail)"
      ),
      Unit = ifelse(grepl("_hh", series), "Household", "Clan")
    ) %>%
    dplyr::filter(Unit == show_unit)

  if (is.null(y_limits)) {
    vals <- long$value[is.finite(long$value)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  ggplot(long, aes(x = year, y = value, color = Coefficient, group = Coefficient)) +
    geom_line(linewidth = 1.7) +
    scale_color_manual(values = coef_colors) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = br, expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Year", y = ylab, color = NULL) +
    theme(legend.position = "none", plot.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

shared_y_c123 <- {
  vals <- c(inc_c123_yr$C1_hh, inc_c123_yr$C1_clan,
            inc_c123_yr$C2_hh, inc_c123_yr$C2_clan,
            inc_c123_yr$C3_hh, inc_c123_yr$C3_clan,
            wnh_c123_yr$C1_hh, wnh_c123_yr$C1_clan,
            wnh_c123_yr$C2_hh, wnh_c123_yr$C2_clan,
            wnh_c123_yr$C3_hh, wnh_c123_yr$C3_clan)
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

shared_diff_c123 <- {
  vals <- c(
    inc_c123_yr$C1_hh - inc_c123_yr$C1_clan,
    inc_c123_yr$C2_hh - inc_c123_yr$C2_clan,
    inc_c123_yr$C3_hh - inc_c123_yr$C3_clan,
    wnh_c123_yr$C1_hh - wnh_c123_yr$C1_clan,
    wnh_c123_yr$C2_hh - wnh_c123_yr$C2_clan,
    wnh_c123_yr$C3_hh - wnh_c123_yr$C3_clan
  )
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

inc_pHH   <- make_c123_panel(inc_c123_yr, shared_y_c123,    "Household", "Inequality Coefficient")
inc_pClan <- make_c123_panel(inc_c123_yr, shared_y_c123,    "Clan",      NULL)
inc_pDiff <- make_c123_panel(inc_c123_yr, shared_diff_c123, "Diff",      NULL)

wnh_pHH   <- make_c123_panel(wnh_c123_yr, shared_y_c123,    "Household", "Inequality Coefficient")
wnh_pClan <- make_c123_panel(wnh_c123_yr, shared_y_c123,    "Clan",      NULL)
wnh_pDiff <- make_c123_panel(wnh_c123_yr, shared_diff_c123, "Diff",      NULL)

# Shared legend donor: 3 lines (one per coefficient), solid
c123_legend_donor <- ggplot(
  inc_c123_yr %>%
    dplyr::select(year, C1_hh, C2_hh, C3_hh) %>%
    pivot_longer(-year, names_to = "s", values_to = "v") %>%
    dplyr::mutate(Coefficient = dplyr::recode(s,
      C1_hh = "C1 (Bonferroni)", C2_hh = "C2 (Gini)", C3_hh = "C3 (Upper Tail)")),
  aes(x = year, y = v, color = Coefficient)
) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = coef_colors, name = NULL) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)))

c123_legend <- cowplot::get_legend(c123_legend_donor)

make_sub_draw <- function(txt) ggdraw() +
  draw_label(txt, x = 0, hjust = 0, fontfamily = base_family, size = sub_size)

figG_rows <- cowplot::plot_grid(
  make_sub_draw("Panel A: Income"),
  cowplot::plot_grid(inc_pHH, inc_pClan, inc_pDiff, nrow = 1),
  make_sub_draw("Panel B: Wealth (excl. home equity)"),
  cowplot::plot_grid(wnh_pHH, wnh_pClan, wnh_pDiff, nrow = 1),
  ncol = 1, rel_heights = c(0.05, 1, 0.05, 1)
)

figG_title <- ggdraw() +
  draw_label("Appendix C. Alternative Inequality Measures (C1, C2, C3) Over Time",
             x = 0, hjust = 0, fontface = "bold",
             fontfamily = base_family, size = title_size)

figG <- cowplot::plot_grid(
  figG_title, figG_rows, c123_legend,
  ncol = 1, rel_heights = c(0.06, 1, 0.06)
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "figureG.pdf"), figG, width = 14, height = 13)
  message("Saved: figureG.pdf")
}

# =============================================================================
# Sensitivity figures for Gini coefficients — load CSVs
# Named sensitivity_gini_1 through sensitivity_gini_5
# =============================================================================
income_all    <- read_csv(here("6_calculate_gini", "output", "income.csv"),           show_col_types = FALSE)
wealth_wh_all <- read_csv(here("6_calculate_gini", "output", "wealth_withhome.csv"),  show_col_types = FALSE)
wealth_nh_all <- read_csv(here("6_calculate_gini", "output", "wealth_nohouse.csv"),   show_col_types = FALSE)

# Helper: pooled average HH-Clan gap across all non-ALL years
avg_gap <- function(df, hh_col, cl_col) {
  d <- df %>%
    filter(year != "ALL") %>%
    mutate(diff = as.numeric(.data[[hh_col]]) - as.numeric(.data[[cl_col]])) %>%
    filter(is.finite(diff))
  round(mean(d$diff), 3)
}

# ── Gap scalars for sensitivity_gini_1: Negative values ───────────────────
gap1_main_inc  <- avg_gap(income_all,    "r_hh_w_inc",      "r_cl_w_inc")
gap1_alt_inc   <- avg_gap(income_all,    "neg_r_hh_inc",    "neg_r_cl_inc")
gap1_main_w    <- avg_gap(wealth_wh_all, "r_hh_w_wealth",   "r_cl_w_wealth")
gap1_alt_w     <- avg_gap(wealth_wh_all, "neg_r_hh_wealth", "neg_r_cl_wealth")

# ── Gap scalars for sensitivity_gini_2: Home equity ───────────────────────
gap2_main_w    <- avg_gap(wealth_wh_all, "r_hh_w_wealth",   "r_cl_w_wealth")
gap2_alt_w     <- avg_gap(wealth_nh_all, "r_hh_w_wealth",   "r_cl_w_wealth")

# ── Gap scalars for sensitivity_gini_3: Single-HH clans ───────────────────
gap3_main_inc  <- avg_gap(income_all,    "r_hh_w_inc",      "r_cl_w_inc")
gap3_alt_inc   <- avg_gap(income_all,    "hh_w_inc",        "cl_w_inc")
gap3_main_w    <- avg_gap(wealth_wh_all, "r_hh_w_wealth",   "r_cl_w_wealth")
gap3_alt_w     <- avg_gap(wealth_wh_all, "hh_w_wealth",     "cl_w_wealth")

# ── Gap scalars for sensitivity_gini_4: Weighting ─────────────────────────
gap4_main_inc  <- avg_gap(income_all,    "r_hh_w_inc",      "r_cl_w_inc")
gap4_alt_inc   <- avg_gap(income_all,    "r_hh_unw_inc",    "r_cl_unw_inc")
gap4_main_w    <- avg_gap(wealth_wh_all, "r_hh_w_wealth",   "r_cl_w_wealth")
gap4_alt_w     <- avg_gap(wealth_wh_all, "r_hh_unw_wealth", "r_cl_unw_wealth")

# ── sensitivity_gini_1: Negative values ───────────────────────────────────
sensitivity_gini_1 <- make_sensitivity_figure(
  make_sensitivity_plot(
    df = income_all,
    main_hh = "r_hh_w_inc",   main_cl = "r_cl_w_inc",
    alt_hh  = "neg_r_hh_inc", alt_cl  = "neg_r_cl_inc",
    left_label  = "Excl. negative values",
    right_label = "Incl. negative values"
  ),
  make_sensitivity_plot(
    df = wealth_wh_all,
    main_hh = "r_hh_w_wealth",   main_cl = "r_cl_w_wealth",
    alt_hh  = "neg_r_hh_wealth", alt_cl  = "neg_r_cl_wealth",
    left_label  = "Excl. negative values",
    right_label = "Incl. negative values"
  ),
  title_str = "Appendix D1. Negative Values"
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "sensitivity_gini_1.pdf"),
         sensitivity_gini_1, width = 14, height = 11)
  message("Saved: sensitivity_gini_1.pdf")
}

# ── sensitivity_gini_2: Home equity (wealth only) ─────────────────────────
wealth_d_df <- wealth_wh_all %>%
  left_join(
    wealth_nh_all %>% select(year,
                              r_hh_w_wealth_nh = r_hh_w_wealth,
                              r_cl_w_wealth_nh = r_cl_w_wealth),
    by = "year"
  )

sensitivity_gini_2 <- make_sensitivity_figure(
  # Income panel: not applicable — show wealth-only note as placeholder
  ggdraw() + draw_label(
    "Home equity applies to wealth only",
    fontfamily = base_family, size = base_size * 0.8, color = "grey50",
    x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5
  ),
  make_sensitivity_plot(
    df = wealth_d_df,
    main_hh = "r_hh_w_wealth",    main_cl = "r_cl_w_wealth",
    alt_hh  = "r_hh_w_wealth_nh", alt_cl  = "r_cl_w_wealth_nh",
    left_label  = "Incl. home equity",
    right_label = "Excl. home equity"
  ),
  title_str = "Appendix D2. Home Equity",
  sub_a     = "Panel A: Income (not applicable)",
  sub_b     = "Panel B: Wealth"
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "sensitivity_gini_2.pdf"),
         sensitivity_gini_2, width = 14, height = 11)
  message("Saved: sensitivity_gini_2.pdf")
}

# ── sensitivity_gini_3: Single-household clans ────────────────────────────
sensitivity_gini_3 <- make_sensitivity_figure(
  make_sensitivity_plot(
    df = income_all,
    main_hh = "r_hh_w_inc", main_cl = "r_cl_w_inc",
    alt_hh  = "hh_w_inc",   alt_cl  = "cl_w_inc",
    left_label  = "Excl. single-HH clans",
    right_label = "Incl. single-HH clans"
  ),
  make_sensitivity_plot(
    df = wealth_wh_all,
    main_hh = "r_hh_w_wealth", main_cl = "r_cl_w_wealth",
    alt_hh  = "hh_w_wealth",   alt_cl  = "cl_w_wealth",
    left_label  = "Excl. single-HH clans",
    right_label = "Incl. single-HH clans"
  ),
  title_str = "Appendix D3. Single-Household Clans"
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "sensitivity_gini_3.pdf"),
         sensitivity_gini_3, width = 14, height = 11)
  message("Saved: sensitivity_gini_3.pdf")
}

# ── sensitivity_gini_4: Weighting ─────────────────────────────────────────
sensitivity_gini_4 <- make_sensitivity_figure(
  make_sensitivity_plot(
    df = income_all,
    main_hh = "r_hh_w_inc",   main_cl = "r_cl_w_inc",
    alt_hh  = "r_hh_unw_inc", alt_cl  = "r_cl_unw_inc",
    left_label  = "Weighted",
    right_label = "Unweighted"
  ),
  make_sensitivity_plot(
    df = wealth_wh_all,
    main_hh = "r_hh_w_wealth",   main_cl = "r_cl_w_wealth",
    alt_hh  = "r_hh_unw_wealth", alt_cl  = "r_cl_unw_wealth",
    left_label  = "Weighted",
    right_label = "Unweighted"
  ),
  title_str = "Appendix D4. Weighting"
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "sensitivity_gini_4.pdf"),
         sensitivity_gini_4, width = 14, height = 11)
  message("Saved: sensitivity_gini_4.pdf")
}

# ── sensitivity_gini_5: Size standardization (current vs. unadjusted only) ─
options(survey.lonely.psu = "adjust")

r_hh_raw          <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans_raw        <- readRDS(here("4_clans",      "output", "robust_clans.rds"))
r_hh_wealth_raw    <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans_wealth_raw <- readRDS(here("4_clans",      "output", "robust_clans_wealth.rds"))

inc_current <- inc_by_year %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year)) %>%
  select(year, current_hh = r_hh_w_inc, current_clan = r_cl_w_inc)

wealth_current <- wealth_by_year %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year)) %>%
  select(year, current_hh = r_hh_w_wealth, current_clan = r_cl_w_wealth)

inc_unadj <- reduce(
  list(
    run_gini(r_hh_raw,    "inc_all", "fam_weight",  FALSE, FALSE, "unadj_hh"),
    run_gini(r_clans_raw, "inc_all", "clan_weight", FALSE, FALSE, "unadj_clan")
  ),
  full_join, by = "year"
)

wealth_unadj <- reduce(
  list(
    run_gini(r_hh_wealth_raw,    "wealth_nohouse", "fam_weight",  FALSE, FALSE, "unadj_hh"),
    run_gini(r_clans_wealth_raw, "wealth_nohouse", "clan_weight", FALSE, FALSE, "unadj_clan")
  ),
  full_join, by = "year"
)

inc_size_tbl <- inc_current %>%
  left_join(inc_unadj, by = "year") %>%
  select(year, current_hh, current_clan, unadj_hh, unadj_clan) %>%
  arrange(year)

wealth_size_tbl <- wealth_current %>%
  left_join(wealth_unadj, by = "year") %>%
  select(year, current_hh, current_clan, unadj_hh, unadj_clan) %>%
  arrange(year)

# ── Gap scalars for sensitivity_gini_5: Size standardization ──────────────
gap5_main_inc  <- avg_gap(inc_size_tbl,    "current_hh",   "current_clan")
gap5_alt_inc   <- avg_gap(inc_size_tbl,    "unadj_hh",     "unadj_clan")
gap5_main_w    <- avg_gap(wealth_size_tbl, "current_hh",   "current_clan")
gap5_alt_w     <- avg_gap(wealth_size_tbl, "unadj_hh",     "unadj_clan")

sensitivity_gini_5 <- make_sensitivity_figure(
  make_sensitivity_plot(
    df = inc_size_tbl,
    main_hh = "current_hh",   main_cl = "current_clan",
    alt_hh  = "unadj_hh",     alt_cl  = "unadj_clan",
    left_label  = "Current (divide by size)",
    right_label = "Unadjusted"
  ),
  make_sensitivity_plot(
    df = wealth_size_tbl,
    main_hh = "current_hh",   main_cl = "current_clan",
    alt_hh  = "unadj_hh",     alt_cl  = "unadj_clan",
    left_label  = "Current (divide by size)",
    right_label = "Unadjusted"
  ),
  title_str = "Appendix D5. Size Standardization"
)

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "sensitivity_gini_5.pdf"),
         sensitivity_gini_5, width = 14, height = 11)
  message("Saved: sensitivity_gini_5.pdf")
}