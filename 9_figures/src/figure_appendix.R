# Functions used in this script:
#   Defined in this script: make_x_breaks_appendix, add_sens_legend,
#     add_hh_age_wt, add_clan_age_wt, race_ratios_by_year, ratio_diff_limits,
#     make_f_plot, wtd_by_year, n_by_year, f4_panel

#   From 9_figures/src/figure_main.R: get_legend_safe, tab1_make_ft
#   From functions/data_utils.R: avg_gap
#   From functions/gini_utils.R: run_gini
#   From functions/wtd_stats.R: wtd_mean, wtd_median
#   From functions/plot_helpers.R: make_sub_draw, make_ratio_plot,
#     make_sensitivity_plot, make_sensitivity_figure,
#     make_single_sensitivity_figure, make_race_sensitivity_plot,
#     make_c123_panel, make_ratio_sensitivity_plot,
#     make_ratio_sensitivity_figure
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

# Appendix year breaks: decade (or 5-year) ticks only, no colliding start/end years.
# Previously this overrode plot_helpers::make_x_breaks for everything built in
# this script; it now has its own name and is passed to the plot_helpers
# functions via x_breaks_fn = make_x_breaks_appendix, so the output is unchanged.
# Used in: figure_appendix
make_x_breaks_appendix <- function(start, end) {
  step <- if (end - start > 45) 10 else 5
  seq(ceiling(start / step) * step, floor(end / step) * step, by = step)
}

if (!exists("SAVE_FILES")) SAVE_FILES <- TRUE
if (!exists("base_family")) {
  stop("Source figure_main.R before figure_appendix.R to set style constants.")
}

# Load data
income_all    <- read_csv(here("6_calculate_gini", "output", "income.csv"),
                          show_col_types = FALSE)
wealth_wh_all <- read_csv(here("6_calculate_gini", "output", "wealth_withhome.csv"),
                          show_col_types = FALSE)
wealth_nh_all <- read_csv(here("6_calculate_gini", "output", "wealth_nohouse.csv"),
                          show_col_types = FALSE)

inc_c123    <- read_csv(here("8_nuclear_family", "output", "income_C123.csv"),
                        show_col_types = FALSE)
wnh_c123    <- read_csv(here("8_nuclear_family", "output", "wealth_C123.csv"),
                        show_col_types = FALSE)
inc_c123_yr <- inc_c123 %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))
wnh_c123_yr <- wnh_c123 %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))

inc_race_yr <- read_csv(here("7_gini_by_race", "output", "income_race.csv"),
                        show_col_types = FALSE) %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year))
w_race_yr   <- read_csv(here("7_gini_by_race", "output", "wealth_withhome_race.csv"),
                        show_col_types = FALSE) %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year))
inc_race_all <- read_csv(here("7_gini_by_race", "output", "income_race.csv"),
                         show_col_types = FALSE) %>% filter(year == "ALL")
w_race_all   <- read_csv(here("7_gini_by_race", "output", "wealth_withhome_race.csv"),
                         show_col_types = FALSE) %>% filter(year == "ALL")

wealth_ratios <- read_csv(here("7_gini_by_race", "output", "wealth_withhome_race_ratios.csv"),
                          show_col_types = FALSE)
wnh_ratios    <- read_csv(here("7_gini_by_race", "output", "wealth_nohouse_race_ratios.csv"),
                          show_col_types = FALSE)


# Gap scalars for notes
gap1_main_inc <- avg_gap(income_all,    "r_hh_w_inc",       "r_cl_w_inc")
gap1_alt_inc  <- avg_gap(income_all,    "neg_r_hh_inc",     "neg_r_cl_inc")
gap1_main_w   <- avg_gap(wealth_wh_all, "r_hh_w_wealth",    "r_cl_w_wealth")
gap1_alt_w    <- avg_gap(wealth_wh_all, "neg_r_hh_wealth",  "neg_r_cl_wealth")
gap2_main_w   <- avg_gap(wealth_wh_all, "r_hh_w_wealth",    "r_cl_w_wealth")
gap2_alt_w    <- avg_gap(wealth_nh_all, "r_hh_w_wealth",    "r_cl_w_wealth")
gap3_main_inc <- avg_gap(income_all,    "r_hh_w_inc",       "r_cl_w_inc")
gap3_alt_inc  <- avg_gap(income_all,    "hh_w_inc",         "cl_w_inc")
gap3_main_w   <- avg_gap(wealth_wh_all, "r_hh_w_wealth",    "r_cl_w_wealth")
gap3_alt_w    <- avg_gap(wealth_wh_all, "hh_w_wealth",      "cl_w_wealth")
gap4_main_inc <- avg_gap(income_all,    "r_hh_w_inc",       "r_cl_w_inc")
gap4_alt_inc  <- avg_gap(income_all,    "r_hh_unw_inc",     "r_cl_unw_inc")
gap4_main_w   <- avg_gap(wealth_wh_all, "r_hh_w_wealth",    "r_cl_w_wealth")
gap4_alt_w    <- avg_gap(wealth_wh_all, "r_hh_unw_wealth",  "r_cl_unw_wealth")

gap6_black_inc    <- round(mean(inc_race_yr$r_hh_w_inc_black    - inc_race_yr$r_cl_w_inc_black,    na.rm = TRUE), 3)
gap6_nonblack_inc <- round(mean(inc_race_yr$r_hh_w_inc_nonblack - inc_race_yr$r_cl_w_inc_nonblack, na.rm = TRUE), 3)
gap6_black_w      <- round(mean(w_race_yr$r_hh_w_wealth_black    - w_race_yr$r_cl_w_wealth_black,   na.rm = TRUE), 3)
gap6_nonblack_w   <- round(mean(w_race_yr$r_hh_w_wealth_nonblack - w_race_yr$r_cl_w_wealth_nonblack, na.rm = TRUE), 3)

inc_diff_black    <- inc_race_all$r_hh_w_inc_black    - inc_race_all$r_cl_w_inc_black
inc_diff_nonblack <- inc_race_all$r_hh_w_inc_nonblack - inc_race_all$r_cl_w_inc_nonblack
w_diff_black      <- w_race_all$r_hh_w_wealth_black   - w_race_all$r_cl_w_wealth_black
w_diff_nonblack   <- w_race_all$r_hh_w_wealth_nonblack - w_race_all$r_cl_w_wealth_nonblack


# Shared legend for D1–D6 and E1–E6: colour = specification, linetype = unit
# Used in: figure_appendix
add_sens_legend <- function(fig, left_label, right_label, show_diff = TRUE) {
  left_label  <- paste0(left_label,  " (main text)")
  right_label <- paste0(right_label, " (sensitivity)")
  lines <- c("Households", "Kin Groups", "HH − Kin Group difference")[seq_len(if (show_diff) 3 else 2)]
  donor <- ggplot(
    tidyr::expand_grid(Version = factor(c(left_label, right_label), levels = c(left_label, right_label)),
                       Line    = factor(lines, levels = lines), x = 0:1),
    aes(x = x, y = x, color = Version, linetype = Line, group = interaction(Version, Line))) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = setNames(c(NAVY, CORAL), c(left_label, right_label)), name = NULL) +
    scale_linetype_manual(values = setNames(c("solid", "dotted", "dashed")[seq_along(lines)], lines), name = NULL) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.key.width = unit(1.5, "cm"))
  cowplot::plot_grid(fig, get_legend_safe(donor), ncol = 1, rel_heights = c(1, 0.12))
}

# ── Appendix C: Gini Sensitivity Analyses (C1–C7) ────────────────────────────

# C1 — Race (Gini by race subgroup) ──────────────────────────────────────────
shared_y_race <- {
  vals <- c(inc_race_yr$r_hh_w_inc_black,     inc_race_yr$r_cl_w_inc_black,
            inc_race_yr$r_hh_w_inc_nonblack,  inc_race_yr$r_cl_w_inc_nonblack,
            w_race_yr$r_hh_w_wealth_black,    w_race_yr$r_cl_w_wealth_black,
            w_race_yr$r_hh_w_wealth_nonblack, w_race_yr$r_cl_w_wealth_nonblack)
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}
shared_diff_race <- {
  vals <- c(inc_race_yr$r_hh_w_inc_black    - inc_race_yr$r_cl_w_inc_black,
            inc_race_yr$r_hh_w_inc_nonblack - inc_race_yr$r_cl_w_inc_nonblack,
            w_race_yr$r_hh_w_wealth_black    - w_race_yr$r_cl_w_wealth_black,
            w_race_yr$r_hh_w_wealth_nonblack - w_race_yr$r_cl_w_wealth_nonblack)
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

race6_inc <- make_race_sensitivity_plot(
  inc_race_yr,
  "r_hh_w_inc_black",    "r_cl_w_inc_black",
  "r_hh_w_inc_nonblack", "r_cl_w_inc_nonblack",
  y_limits = shared_y_race, diff_limits = shared_diff_race, x_breaks_fn = make_x_breaks_appendix)
race6_w <- make_race_sensitivity_plot(
  w_race_yr,
  "r_hh_w_wealth_black",    "r_cl_w_wealth_black",
  "r_hh_w_wealth_nonblack", "r_cl_w_wealth_nonblack",
  y_limits = shared_y_race, diff_limits = shared_diff_race, x_breaks_fn = make_x_breaks_appendix)

sensitivity_gini_c1 <- make_sensitivity_figure(
  race6_inc, race6_w,
  title_str = "Appendix D7. Gini Coefficients by Race Subgroup", show_title = FALSE,
  sub_a = "Panel A: Income",
  sub_b = "Panel B: Wealth (incl. home equity)",
  race_legend = TRUE)
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD7.pdf"),
         sensitivity_gini_c1, width = 14, height = 11)
  message("Saved: appendixD7.pdf")
}

# C2 — Sample: single-household kin groups ────────────────────────────────────
sensitivity_gini_c2 <- add_sens_legend(make_sensitivity_figure(
  make_sensitivity_plot(df = income_all,
    main_hh = "r_hh_w_inc", main_cl = "r_cl_w_inc",
    alt_hh  = "hh_w_inc",   alt_cl  = "cl_w_inc",
    left_label = "Excl. single-HH kin groups", right_label = "Incl. single-HH kin groups", x_breaks_fn = make_x_breaks_appendix),
  make_sensitivity_plot(df = wealth_wh_all,
    main_hh = "r_hh_w_wealth", main_cl = "r_cl_w_wealth",
    alt_hh  = "hh_w_wealth",   alt_cl  = "cl_w_wealth",
    left_label = "Excl. single-HH kin groups", right_label = "Incl. single-HH kin groups", x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix D5. Sensitivity: Single-Household Kin Groups", show_title = FALSE),
  "Excl. single-HH kin groups", "Incl. single-HH kin groups")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD5.pdf"),
         sensitivity_gini_c2, width = 14, height = 11)
  message("Saved: appendixD5.pdf")
}

# C3 — Wealth without home equity ─────────────────────────────────────────────
wealth_c3_df <- wealth_wh_all %>%
  left_join(
    wealth_nh_all %>% dplyr::select(year,
                                     r_hh_w_wealth_nh = r_hh_w_wealth,
                                     r_cl_w_wealth_nh = r_cl_w_wealth),
    by = "year")

sensitivity_gini_c3 <- add_sens_legend(make_single_sensitivity_figure(
  make_sensitivity_plot(df = wealth_c3_df,
    main_hh = "r_hh_w_wealth",    main_cl = "r_cl_w_wealth",
    alt_hh  = "r_hh_w_wealth_nh", alt_cl  = "r_cl_w_wealth_nh",
    left_label = "Incl. home equity", right_label = "Excl. home equity", x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix D1. Sensitivity: Wealth With and Without Home Equity", show_title = FALSE),
  "Incl. home equity", "Excl. home equity")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD1.pdf"),
         sensitivity_gini_c3, width = 14, height = 11)
  message("Saved: appendixD1.pdf")
}

# C4 — Negative values ────────────────────────────────────────────────────────
sensitivity_gini_c4 <- add_sens_legend(make_sensitivity_figure(
  make_sensitivity_plot(df = income_all,
    main_hh = "r_hh_w_inc",   main_cl = "r_cl_w_inc",
    alt_hh  = "neg_r_hh_inc", alt_cl  = "neg_r_cl_inc",
    left_label = "Excl. negative values", right_label = "Incl. negative values", x_breaks_fn = make_x_breaks_appendix),
  make_sensitivity_plot(df = wealth_wh_all,
    main_hh = "r_hh_w_wealth",   main_cl = "r_cl_w_wealth",
    alt_hh  = "neg_r_hh_wealth", alt_cl  = "neg_r_cl_wealth",
    left_label = "Excl. negative values", right_label = "Incl. negative values", x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix D2. Sensitivity: Negative Values", show_title = FALSE),
  "Excl. negative values", "Incl. negative values")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD2.pdf"),
         sensitivity_gini_c4, width = 14, height = 11)
  message("Saved: appendixD2.pdf")
}

# C5 — Size standardization ───────────────────────────────────────────────────
r_hh_raw    <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans_raw <- readRDS(here("4_clans",      "output", "robust_clans.rds"))
r_hh_w_raw  <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_cl_w_raw  <- readRDS(here("4_clans",      "output", "robust_clans_wealth.rds"))

inc_current <- income_all %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year)) %>%
  dplyr::select(year, current_hh = r_hh_w_inc, current_clan = r_cl_w_inc)
wealth_current <- wealth_wh_all %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year)) %>%
  dplyr::select(year, current_hh = r_hh_w_wealth, current_clan = r_cl_w_wealth)

inc_unadj <- purrr::reduce(
  list(
    run_gini(r_hh_raw,    "inc_all", "fam_weight",  FALSE, TRUE, "unadj_hh"),
    run_gini(r_clans_raw, "inc_all", "clan_weight", FALSE, TRUE, "unadj_clan")),
  dplyr::full_join, by = "year")

wealth_unadj <- purrr::reduce(
  list(
    run_gini(r_hh_w_raw, "wealth", "fam_weight",  FALSE, TRUE, "unadj_hh"),
    run_gini(r_cl_w_raw, "wealth", "clan_weight", FALSE, TRUE, "unadj_clan")),
  dplyr::full_join, by = "year")

inc_size_tbl    <- inc_current    %>% left_join(inc_unadj,    by = "year") %>% arrange(year)
wealth_size_tbl <- wealth_current %>% left_join(wealth_unadj, by = "year") %>% arrange(year)

gap5_main_inc <- avg_gap(inc_size_tbl,    "current_hh", "current_clan")
gap5_alt_inc  <- avg_gap(inc_size_tbl,    "unadj_hh",   "unadj_clan")
gap5_main_w   <- avg_gap(wealth_size_tbl, "current_hh", "current_clan")
gap5_alt_w    <- avg_gap(wealth_size_tbl, "unadj_hh",   "unadj_clan")

sensitivity_gini_c5 <- add_sens_legend(make_sensitivity_figure(
  make_sensitivity_plot(df = inc_size_tbl,
    main_hh = "current_hh", main_cl = "current_clan",
    alt_hh  = "unadj_hh",   alt_cl  = "unadj_clan",
    left_label = "Size-standardised", right_label = "Unadjusted", x_breaks_fn = make_x_breaks_appendix),
  make_sensitivity_plot(df = wealth_size_tbl,
    main_hh = "current_hh", main_cl = "current_clan",
    alt_hh  = "unadj_hh",   alt_cl  = "unadj_clan",
    left_label = "Size-standardised", right_label = "Unadjusted", x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix D4. Sensitivity: Size Standardization", show_title = FALSE),
  "Size-standardised", "Unadjusted")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD4.pdf"),
         sensitivity_gini_c5, width = 14, height = 11)
  message("Saved: appendixD4.pdf")
}

# C6 — Weighting ──────────────────────────────────────────────────────────────
sensitivity_gini_c6 <- add_sens_legend(make_sensitivity_figure(
  make_sensitivity_plot(df = income_all,
    main_hh = "r_hh_w_inc",   main_cl = "r_cl_w_inc",
    alt_hh  = "r_hh_unw_inc", alt_cl  = "r_cl_unw_inc",
    left_label = "Weighted", right_label = "Unweighted", x_breaks_fn = make_x_breaks_appendix),
  make_sensitivity_plot(df = wealth_wh_all,
    main_hh = "r_hh_w_wealth",   main_cl = "r_cl_w_wealth",
    alt_hh  = "r_hh_unw_wealth", alt_cl  = "r_cl_unw_wealth",
    left_label = "Weighted", right_label = "Unweighted", x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix D3. Sensitivity: Weighting", show_title = FALSE),
  "Weighted", "Unweighted")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD3.pdf"),
         sensitivity_gini_c6, width = 14, height = 11)
  message("Saved: appendixD3.pdf")
}

# C7 — Clan age weighting ──────────────────────────────────────────────────────
# r_cl_agewt_inc / r_cl_agewt_wealth come from 6_calculate_gini, where clan
# weight is downweighted for older clans:
#   mean_clan_age    <- mean(r_clans$clan_age_mean, na.rm = TRUE)
#   clan_weight_age  <- clan_weight * mean_clan_age / clan_age_mean
# Household age weight mirrors the clan one: fam_weight * mean_hh_age / hh_age
# Used in: figure_appendix
add_hh_age_wt <- function(df) {
  df <- df %>% dplyr::mutate(hh_age_mean = ifelse(hh_age_n > 0, hh_age_sum / hh_age_n, NA_real_))
  mean_hh_age <- mean(df$hh_age_mean, na.rm = TRUE)
  # Households with no age data keep their standard weight so the sample matches the main series
  df %>% dplyr::mutate(fam_weight_age = fam_weight * dplyr::coalesce(mean_hh_age / hh_age_mean, 1))
}

income_c7 <- income_all %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year)) %>%
  left_join(run_gini(add_hh_age_wt(r_hh_raw), "inc_all", "fam_weight_age",
                     TRUE, TRUE, "r_hh_agewt_inc"), by = "year")
wealth_wh_c7 <- wealth_wh_all %>%
  filter(year != "ALL") %>% mutate(year = as.numeric(year)) %>%
  left_join(run_gini(add_hh_age_wt(r_hh_w_raw), "wealth", "fam_weight_age",
                     TRUE, TRUE, "r_hh_agewt_wealth"), by = "year")

gap7_main_inc <- avg_gap(income_all,    "r_hh_w_inc",    "r_cl_w_inc")
gap7_alt_inc  <- avg_gap(income_c7,     "r_hh_agewt_inc",    "r_cl_agewt_inc")
gap7_main_w   <- avg_gap(wealth_wh_all, "r_hh_w_wealth", "r_cl_w_wealth")
gap7_alt_w    <- avg_gap(wealth_wh_c7,  "r_hh_agewt_wealth", "r_cl_agewt_wealth")

sensitivity_gini_c7 <- add_sens_legend(make_sensitivity_figure(
  make_sensitivity_plot(df = income_c7,
    main_hh = "r_hh_w_inc",     main_cl = "r_cl_w_inc",
    alt_hh  = "r_hh_agewt_inc", alt_cl  = "r_cl_agewt_inc",
    left_label = "Standard weighting", right_label = "Age-downweighted", x_breaks_fn = make_x_breaks_appendix),
  make_sensitivity_plot(df = wealth_wh_c7,
    main_hh = "r_hh_w_wealth",     main_cl = "r_cl_w_wealth",
    alt_hh  = "r_hh_agewt_wealth", alt_cl  = "r_cl_agewt_wealth",
    left_label = "Standard weighting", right_label = "Age-downweighted", x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix D6. Sensitivity: Household and Clan Age Weighting", show_title = FALSE),
  "Standard weighting", "Age-downweighted")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD6.pdf"),
         sensitivity_gini_c7, width = 14, height = 11)
  message("Saved: appendixD6.pdf")
}


# ── Appendix Table A1: Characteristics by Year (1984 and 2023) ────────────────
appendix_tab_a1_ft <- tab1_make_ft(
  as.character(tab1_years), as.character(tab1_years), paste0("y", tab1_years),
  "Appendix F1. Characteristics of Households and Kin Groups, 1984 and 2023")
if (SAVE_FILES) {
  flextable::save_as_docx(
    appendix_tab_a1_ft,
    path = here("9_figures", "output", "appendix_tableF1.docx"),
    pr_section = officer::prop_section(
      page_size = officer::page_size(orient = "landscape")))
  message("Saved: appendix_tableF1.docx")
}


# ── Appendix D: Alternative Inequality Measures ───────────────────────────────
coef_colors <- c(
  "C1 (Bonferroni)" = PURPLE,
  "C2 (Gini)"       = NAVY,
  "C3 (Upper Tail)" = TEAL)

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
  vals <- c(inc_c123_yr$C1_hh - inc_c123_yr$C1_clan,
            inc_c123_yr$C2_hh - inc_c123_yr$C2_clan,
            inc_c123_yr$C3_hh - inc_c123_yr$C3_clan,
            wnh_c123_yr$C1_hh - wnh_c123_yr$C1_clan,
            wnh_c123_yr$C2_hh - wnh_c123_yr$C2_clan,
            wnh_c123_yr$C3_hh - wnh_c123_yr$C3_clan)
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

# Integer year breaks, matching Figures 1 and 3
x_inc_years <- scale_x_continuous(breaks = seq(1970, 2020, 10))
x_w_years   <- scale_x_continuous(breaks = seq(1985, 2020, 5))

inc_pHH   <- make_c123_panel(inc_c123_yr, shared_y_c123,    "Household", "Inequality Coefficient", x_breaks_fn = make_x_breaks_appendix) + x_inc_years
inc_pClan <- make_c123_panel(inc_c123_yr, shared_y_c123,    "Kin Group", NULL, x_breaks_fn = make_x_breaks_appendix) + x_inc_years
inc_pDiff <- make_c123_panel(inc_c123_yr, shared_diff_c123, "Diff",      NULL, x_breaks_fn = make_x_breaks_appendix) + x_inc_years
wnh_pHH   <- make_c123_panel(wnh_c123_yr, shared_y_c123,    "Household", "Inequality Coefficient", x_breaks_fn = make_x_breaks_appendix) + x_w_years
wnh_pClan <- make_c123_panel(wnh_c123_yr, shared_y_c123,    "Kin Group", NULL, x_breaks_fn = make_x_breaks_appendix) + x_w_years
wnh_pDiff <- make_c123_panel(wnh_c123_yr, shared_diff_c123, "Diff",      NULL, x_breaks_fn = make_x_breaks_appendix) + x_w_years

c123_legend_donor <- ggplot(
  inc_c123_yr %>%
    dplyr::select(year, C1_hh, C2_hh, C3_hh) %>%
    tidyr::pivot_longer(-year, names_to = "s", values_to = "v") %>%
    dplyr::mutate(Coefficient = dplyr::recode(
      s, C1_hh = "C1 (Bonferroni)", C2_hh = "C2 (Gini)", C3_hh = "C3 (Upper Tail)")),
  aes(x = year, y = v, color = Coefficient)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = coef_colors, name = NULL) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)))

appendix_d <- cowplot::plot_grid(
  cowplot::plot_grid(
    make_sub_draw("Panel A: Income"),
    cowplot::plot_grid(inc_pHH, inc_pClan, inc_pDiff, nrow = 1),
    make_sub_draw("Panel B: Wealth (excl. home equity)"),
    cowplot::plot_grid(wnh_pHH, wnh_pClan, wnh_pDiff, nrow = 1),
    ncol = 1, rel_heights = c(0.05, 1, 0.05, 1)),
  get_legend_safe(c123_legend_donor),
  ncol = 1, rel_heights = c(1, 0.06))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixB.pdf"),
         appendix_d, width = 14, height = 13)
  message("Saved: appendixB.pdf")
}


# ── Appendix E1: Mean and Median Wealth Ratios ────────────────────────────────
appendix_e1_med_plot <- make_ratio_plot(
  wealth_ratios %>% select(year,
                           r_hh_w_ratio = r_hh_w_median_ratio,
                           r_cl_w_ratio = r_cl_w_median_ratio),
  ylab = "Black / Non-Black", x_breaks_fn = make_x_breaks_appendix) + x_w_years

appendix_e1_mean_plot <- make_ratio_plot(
  wealth_ratios %>% select(year,
                           r_hh_w_ratio = r_hh_w_mean_ratio,
                           r_cl_w_ratio = r_cl_w_mean_ratio),
  ylab = "Black / Non-Black", x_breaks_fn = make_x_breaks_appendix) + x_w_years

appendix_e1_note <- sprintf(
  paste0(
    "Note: Lines show the ratio of Black to Non-Black wealth for households and kin groups ",
    "(includes home equity). Panel A uses weighted medians; Panel B uses weighted means. ",
    "Weighted median wealth: Black households $%s, Non-Black households $%s, ",
    "Black kin groups $%s, Non-Black kin groups $%s. ",
    "Median ratio changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
    "and %.1f%% for kin groups (%.2f to %.2f). ",
    "On average across all years, the median ratio is %.3f higher for households than kin groups. ",
    "Weighted mean wealth: Black households $%s, Non-Black households $%s, ",
    "Black kin groups $%s, Non-Black kin groups $%s. ",
    "Mean ratio changed by %.1f%% for households (%.2f in %d to %.2f in %d) ",
    "and %.1f%% for kin groups (%.2f to %.2f). ",
    "On average across all years, the mean ratio is %.3f higher for households than kin groups. ",
    "Solid lines = households; dotted lines = kin groups. ",
    "See Figure 3 for median wealth ratios."
  ),
  # Median scalars
  fmt_money0(w_med_hh$black),    fmt_money0(w_med_hh$nonblack),
  fmt_money0(w_med_cl$black),    fmt_money0(w_med_cl$nonblack),
  w_med_ratio_hh_pct, w_med_ratio_hh_first, w_first_yr, w_med_ratio_hh_last, w_last_yr,
  w_med_ratio_cl_pct, w_med_ratio_cl_first, w_med_ratio_cl_last,
  avg_w_med_ratio_gap,
  # Mean scalars
  fmt_money0(w_race_hh$black),   fmt_money0(w_race_hh$nonblack),
  fmt_money0(w_race_cl$black),   fmt_money0(w_race_cl$nonblack),
  w_ratio_hh_pct, w_ratio_hh_first, w_first_yr, w_ratio_hh_last, w_last_yr,
  w_ratio_cl_pct, w_ratio_cl_first, w_ratio_cl_last,
  avg_w_ratio_gap)

appendix_e1 <- arrangeGrob(
  textGrob(
    "Appendix E7. Black / Non-Black Wealth Ratios: Households vs. Kin Groups",
    x = unit(0, "npc"), just = "left",
    gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)),
  arrangeGrob(
    arrangeGrob(
      textGrob("Panel A: Median wealth ratio",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      appendix_e1_med_plot, ncol = 1, heights = c(1, 12)),
    arrangeGrob(
      textGrob("Panel B: Mean wealth ratio",
               x = unit(0, "npc"), just = "left",
               gp = gpar(fontfamily = base_family, fontsize = sub_size)),
      appendix_e1_mean_plot, ncol = 1, heights = c(1, 12)),
    ncol = 2),
  ncol = 1, heights = unit(c(0.5, 10.0), "inches"))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE7.pdf"),
         appendix_e1, width = 14, height = 11)
  message("Saved: appendixE7.pdf")
}


# ── Appendix E: Race Ratio Sensitivity Analyses (E2–E6) ──────────────────────
# Panel A = median wealth ratio, Panel B = mean wealth ratio throughout.
# E2–E6 mirror C2–C6 but for race ratios rather than Gini coefficients.

all_ratio_vals <- c(
  unlist(wealth_ratios %>% select(ends_with("_mean_ratio"))),
  unlist(wealth_ratios %>% select(ends_with("_median_ratio"))),
  unlist(wnh_ratios    %>% select(ends_with("_mean_ratio"))),
  unlist(wnh_ratios    %>% select(ends_with("_median_ratio"))))
all_ratio_vals <- all_ratio_vals[is.finite(all_ratio_vals)]
shared_y_rsens <- c(floor(min(all_ratio_vals) * 20) / 20,
                    ceiling(max(all_ratio_vals) * 20) / 20 + 0.05)


# E2 — Sample: single-household kin groups ────────────────────────────────────
sensitivity_ratio_e2 <- add_sens_legend(make_ratio_sensitivity_figure(
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_median_ratio", main_cl = "r_cl_w_median_ratio",
    alt_hh  = "hh_w_median_ratio",   alt_cl  = "cl_w_median_ratio",
    left_label = "Excl. single-HH kin groups", right_label = "Incl. single-HH kin groups",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_ratios$r_hh_w_median_ratio - wealth_ratios$r_cl_w_median_ratio,
                wealth_ratios$hh_w_median_ratio   - wealth_ratios$cl_w_median_ratio)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_mean_ratio", main_cl = "r_cl_w_mean_ratio",
    alt_hh  = "hh_w_mean_ratio",   alt_cl  = "cl_w_mean_ratio",
    left_label = "Excl. single-HH kin groups", right_label = "Incl. single-HH kin groups",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_ratios$r_hh_w_mean_ratio - wealth_ratios$r_cl_w_mean_ratio,
                wealth_ratios$hh_w_mean_ratio   - wealth_ratios$cl_w_mean_ratio)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix E5. Race Ratios: Single-Household Kin Groups"),
  "Excl. single-HH kin groups", "Incl. single-HH kin groups")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE5.pdf"),
         sensitivity_ratio_e2, width = 14, height = 11)
  message("Saved: appendixE5.pdf")
}


# E3 — Wealth without home equity ─────────────────────────────────────────────
wealth_e3 <- wealth_ratios %>%
  left_join(
    wnh_ratios %>% dplyr::select(year,
                                  r_hh_w_mean_ratio_nh   = r_hh_w_mean_ratio,
                                  r_cl_w_mean_ratio_nh   = r_cl_w_mean_ratio,
                                  r_hh_w_median_ratio_nh = r_hh_w_median_ratio,
                                  r_cl_w_median_ratio_nh = r_cl_w_median_ratio),
    by = "year")

sensitivity_ratio_e3 <- add_sens_legend(make_ratio_sensitivity_figure(
  make_ratio_sensitivity_plot(wealth_e3,
    main_hh = "r_hh_w_median_ratio",    main_cl = "r_cl_w_median_ratio",
    alt_hh  = "r_hh_w_median_ratio_nh", alt_cl  = "r_cl_w_median_ratio_nh",
    left_label = "Incl. home equity", right_label = "Excl. home equity",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_e3$r_hh_w_median_ratio    - wealth_e3$r_cl_w_median_ratio,
                wealth_e3$r_hh_w_median_ratio_nh - wealth_e3$r_cl_w_median_ratio_nh)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  make_ratio_sensitivity_plot(wealth_e3,
    main_hh = "r_hh_w_mean_ratio",    main_cl = "r_cl_w_mean_ratio",
    alt_hh  = "r_hh_w_mean_ratio_nh", alt_cl  = "r_cl_w_mean_ratio_nh",
    left_label = "Incl. home equity", right_label = "Excl. home equity",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_e3$r_hh_w_mean_ratio    - wealth_e3$r_cl_w_mean_ratio,
                wealth_e3$r_hh_w_mean_ratio_nh - wealth_e3$r_cl_w_mean_ratio_nh)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix E1. Race Ratios: Wealth With and Without Home Equity"),
  "Incl. home equity", "Excl. home equity")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE1.pdf"),
         sensitivity_ratio_e3, width = 14, height = 11)
  message("Saved: appendixE1.pdf")
}


# E4 — Negative values ────────────────────────────────────────────────────────
sensitivity_ratio_e4 <- add_sens_legend(make_ratio_sensitivity_figure(
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_median_ratio",     main_cl = "r_cl_w_median_ratio",
    alt_hh  = "neg_r_hh_w_median_ratio", alt_cl  = "neg_r_cl_w_median_ratio",
    left_label = "Excl. negative values", right_label = "Incl. negative values",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_ratios$r_hh_w_median_ratio     - wealth_ratios$r_cl_w_median_ratio,
                wealth_ratios$neg_r_hh_w_median_ratio - wealth_ratios$neg_r_cl_w_median_ratio)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_mean_ratio",     main_cl = "r_cl_w_mean_ratio",
    alt_hh  = "neg_r_hh_w_mean_ratio", alt_cl  = "neg_r_cl_w_mean_ratio",
    left_label = "Excl. negative values", right_label = "Incl. negative values",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_ratios$r_hh_w_mean_ratio     - wealth_ratios$r_cl_w_mean_ratio,
                wealth_ratios$neg_r_hh_w_mean_ratio - wealth_ratios$neg_r_cl_w_mean_ratio)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix E2. Race Ratios: Negative Values"),
  "Excl. negative values", "Incl. negative values")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE2.pdf"),
         sensitivity_ratio_e4, width = 14, height = 11)
  message("Saved: appendixE2.pdf")
}


# E5 — Size standardization ───────────────────────────────────────────────────
sensitivity_ratio_e5 <- add_sens_legend(make_ratio_sensitivity_figure(
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_median_ratio",       main_cl = "r_cl_w_median_ratio",
    alt_hh  = "r_hh_w_unadj_median_ratio", alt_cl  = "r_cl_w_unadj_median_ratio",
    left_label = "Size-standardised", right_label = "Unadjusted",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_ratios$r_hh_w_median_ratio       - wealth_ratios$r_cl_w_median_ratio,
                wealth_ratios$r_hh_w_unadj_median_ratio - wealth_ratios$r_cl_w_unadj_median_ratio)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_mean_ratio",       main_cl = "r_cl_w_mean_ratio",
    alt_hh  = "r_hh_w_unadj_mean_ratio", alt_cl  = "r_cl_w_unadj_mean_ratio",
    left_label = "Size-standardised", right_label = "Unadjusted",
    y_limits    = shared_y_rsens,
    diff_limits = c(-0.05, 0.20), x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix E4. Race Ratios: Size Standardization"),
  "Size-standardised", "Unadjusted")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE4.pdf"),
         sensitivity_ratio_e5, width = 14, height = 11)
  message("Saved: appendixE4.pdf")
}


# E6 — Weighting ──────────────────────────────────────────────────────────────
sensitivity_ratio_e6 <- add_sens_legend(make_ratio_sensitivity_figure(
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_median_ratio", main_cl = "r_cl_w_median_ratio",
    alt_hh  = "r_hh_u_median_ratio", alt_cl  = "r_cl_u_median_ratio",
    left_label = "Weighted", right_label = "Unweighted",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_ratios$r_hh_w_median_ratio - wealth_ratios$r_cl_w_median_ratio,
                wealth_ratios$r_hh_u_median_ratio - wealth_ratios$r_cl_u_median_ratio)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_mean_ratio", main_cl = "r_cl_w_mean_ratio",
    alt_hh  = "r_hh_u_mean_ratio", alt_cl  = "r_cl_u_mean_ratio",
    left_label = "Weighted", right_label = "Unweighted",
    y_limits    = shared_y_rsens,
    diff_limits = {
      vals <- c(wealth_ratios$r_hh_w_mean_ratio - wealth_ratios$r_cl_w_mean_ratio,
                wealth_ratios$r_hh_u_mean_ratio - wealth_ratios$r_cl_u_mean_ratio)
      vals <- vals[is.finite(vals)]
      c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }, x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix E3. Race Ratios: Weighting"),
  "Weighted", "Unweighted")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE3.pdf"),
         sensitivity_ratio_e6, width = 14, height = 11)
  message("Saved: appendixE3.pdf")
}

# Race ratios with household and clan age weights (new age-weighting race-ratio appendix)
# Used in: figure_appendix
add_clan_age_wt <- function(df) {
  mean_clan_age <- mean(df$clan_age_mean, na.rm = TRUE)
  df %>% dplyr::mutate(clan_weight_age = clan_weight * dplyr::coalesce(mean_clan_age / clan_age_mean, 1))
}

# Used in: figure_appendix
race_ratios_by_year <- function(df, value_var, weight_var, race_var, prefix) {
  df %>%
    dplyr::filter(is.finite(.data[[value_var]]), is.finite(.data[[weight_var]]),
                  .data[[weight_var]] > 0, !is.na(.data[[race_var]])) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      median_ratio = wtd_median(.data[[value_var]][.data[[race_var]] == 1], .data[[weight_var]][.data[[race_var]] == 1]) /
                     wtd_median(.data[[value_var]][.data[[race_var]] == 0], .data[[weight_var]][.data[[race_var]] == 0]),
      mean_ratio   = wtd_mean(.data[[value_var]][.data[[race_var]] == 1], .data[[weight_var]][.data[[race_var]] == 1]) /
                     wtd_mean(.data[[value_var]][.data[[race_var]] == 0], .data[[weight_var]][.data[[race_var]] == 0]),
      .groups = "drop") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::rename_with(~ paste0(prefix, "_", .x), -year)
}

r_hh_w_agewt <- add_hh_age_wt(r_hh_w_raw)     %>% dplyr::mutate(wealth_pc = wealth / numfu)
r_cl_w_agewt <- add_clan_age_wt(r_cl_w_raw)   %>% dplyr::mutate(wealth_pc = wealth / numclan)

# Recompute standard-weight ratios with the same code to check it reproduces wealth_ratios
check_std <- race_ratios_by_year(r_hh_w_agewt, "wealth_pc", "fam_weight", "black_head", "r_hh_std") %>%
  dplyr::inner_join(wealth_ratios %>% dplyr::mutate(year = as.numeric(year)), by = "year")
message(sprintf("Race-ratio check: max |recomputed - CSV| HH median ratio = %.4f",
                max(abs(check_std$r_hh_std_median_ratio - check_std$r_hh_w_median_ratio), na.rm = TRUE)))

wealth_ratios_agewt <- wealth_ratios %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::left_join(race_ratios_by_year(r_hh_w_agewt, "wealth_pc", "fam_weight_age",  "black_head", "r_hh_agewt"), by = "year") %>%
  dplyr::left_join(race_ratios_by_year(r_cl_w_agewt, "wealth_pc", "clan_weight_age", "black_clan", "r_cl_agewt"), by = "year")

# Used in: figure_appendix
ratio_diff_limits <- function(df, a_hh, a_cl, b_hh, b_cl) {
  vals <- c(df[[a_hh]] - df[[a_cl]], df[[b_hh]] - df[[b_cl]])
  vals <- vals[is.finite(vals)]
  c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
}

sensitivity_ratio_agewt <- add_sens_legend(make_ratio_sensitivity_figure(
  make_ratio_sensitivity_plot(wealth_ratios_agewt,
    main_hh = "r_hh_w_median_ratio",      main_cl = "r_cl_w_median_ratio",
    alt_hh  = "r_hh_agewt_median_ratio", alt_cl  = "r_cl_agewt_median_ratio",
    left_label = "Standard weighting", right_label = "Age-downweighted",
    y_limits    = shared_y_rsens,
    diff_limits = ratio_diff_limits(wealth_ratios_agewt,
                                    "r_hh_w_median_ratio", "r_cl_w_median_ratio",
                                    "r_hh_agewt_median_ratio", "r_cl_agewt_median_ratio"), x_breaks_fn = make_x_breaks_appendix),
  make_ratio_sensitivity_plot(wealth_ratios_agewt,
    main_hh = "r_hh_w_mean_ratio",      main_cl = "r_cl_w_mean_ratio",
    alt_hh  = "r_hh_agewt_mean_ratio", alt_cl  = "r_cl_agewt_mean_ratio",
    left_label = "Standard weighting", right_label = "Age-downweighted",
    y_limits    = shared_y_rsens,
    diff_limits = ratio_diff_limits(wealth_ratios_agewt,
                                    "r_hh_w_mean_ratio", "r_cl_w_mean_ratio",
                                    "r_hh_agewt_mean_ratio", "r_cl_agewt_mean_ratio"), x_breaks_fn = make_x_breaks_appendix),
  title_str = "Appendix E6. Race Ratios: Household and Clan Age Weighting"),
  "Standard weighting", "Age-downweighted")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE6.pdf"),
         sensitivity_ratio_agewt, width = 14, height = 11)
  message("Saved: appendixE6.pdf")
}


# ── Appendix F2–F4: Kin Group and Household Size, Age, and Sample Size Over Time ──
unit_cols  <- c("Households" = NAVY, "Kin Groups" = PALE_NAVY)
unit_ltys  <- c("Households" = "solid", "Kin Groups" = "dotted")

# Used in: figure_appendix
make_f_plot <- function(df, ylab, color_var = "Unit", color_vals = unit_cols, title = NULL) {
  yrs <- range(df$year, na.rm = TRUE)
  ggplot(df, aes(x = year, y = value, color = .data[[color_var]], linetype = Unit,
                 group = interaction(.data[[color_var]], Unit))) +
    geom_line(linewidth = 1.7) +
    scale_color_manual(values = color_vals, name = NULL) +
    scale_linetype_manual(values = unit_ltys, name = NULL) +
    scale_x_continuous(breaks = make_x_breaks_appendix(yrs[1], yrs[2])) +
    labs(x = NULL, y = ylab, title = title) +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.key.width = unit(1.5, "cm"),
          plot.title = element_text(family = base_family, face = "bold", size = title_size, hjust = 0),
          plot.title.position = "plot")
}

# Used in: figure_appendix
wtd_by_year <- function(df, var, weight_var, unit_label) {
  df %>%
    dplyr::filter(is.finite(.data[[var]]), is.finite(.data[[weight_var]]), .data[[weight_var]] > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(value = wtd_mean(.data[[var]], .data[[weight_var]]), .groups = "drop") %>%
    dplyr::mutate(year = as.numeric(year), Unit = unit_label)
}

# F2 — Size over time, income sample, weighted means (one legend entry per line)
f2_levels <- c("# of people \u2013 households",
               "# of people \u2013 kin groups",
               "# of households \u2013 kin groups")
f2_all_df <- dplyr::bind_rows(
  wtd_by_year(r_hh_raw,    "numfu",           "fam_weight",  "Households") %>%
    dplyr::mutate(Line = f2_levels[1]),
  wtd_by_year(r_clans_raw, "num_clan_people", "clan_weight", "Kin Groups") %>%
    dplyr::mutate(Line = f2_levels[2]),
  wtd_by_year(r_clans_raw, "numclan",         "clan_weight", "Kin Groups") %>%
    dplyr::mutate(Line = f2_levels[3])) %>%
  dplyr::mutate(Line = factor(Line, levels = f2_levels))

f2_yrs <- range(f2_all_df$year, na.rm = TRUE)
appendix_f2 <- ggplot(f2_all_df, aes(x = year, y = value, color = Line, linetype = Line)) +
  geom_line(linewidth = 1.7) +
  scale_color_manual(values = setNames(c(NAVY, PALE_NAVY, CORAL), f2_levels), name = NULL) +
  scale_linetype_manual(values = setNames(c("solid", "dotted", "dotted"), f2_levels), name = NULL) +
  scale_x_continuous(breaks = make_x_breaks_appendix(f2_yrs[1], f2_yrs[2])) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2)) +
  labs(x = NULL, y = "Average size",
       title = "Appendix F2. Household and Kin Group Size Over Time") +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(family = base_family, face = "bold", size = title_size, hjust = 0),
        plot.title.position = "plot")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixF2.pdf"), appendix_f2, width = 14, height = 9)
  message("Saved: appendixF2.pdf")
}

# F3 — Average age over time, from the same demographic summaries used in Table 1
f3_df <- tab1_demo_inc %>%
  dplyr::select(year, Households = hh_age_mean, `Kin Groups` = clan_age_mean) %>%
  tidyr::pivot_longer(-year, names_to = "Unit", values_to = "value") %>%
  dplyr::filter(is.finite(value))
appendix_f3 <- make_f_plot(f3_df, "Average age (years)",
                           title = "Appendix F3. Average Age of Households and Kin Groups Over Time") +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 15))
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixF3.pdf"), appendix_f3, width = 14, height = 9)
  message("Saved: appendixF3.pdf")
}

# F4 — N per year, robust sample (excl. single-HH kin groups) vs full sample (incl.)
F4_FULL_HH_PATH   <- here("3_households", "output", "households.rds")
F4_FULL_CLAN_PATH <- here("4_clans",      "output", "clans.rds")
if (file.exists(F4_FULL_HH_PATH) && file.exists(F4_FULL_CLAN_PATH)) {
  # Used in: figure_appendix
  n_by_year <- function(df, unit_label, sample_label) {
    df %>% dplyr::count(year, name = "value") %>%
      dplyr::mutate(year = as.numeric(year), Unit = unit_label, Sample = sample_label)
  }
  f4_df <- dplyr::bind_rows(
    n_by_year(r_hh_raw,                   "Households", "Excl. single-HH kin groups"),
    n_by_year(r_clans_raw,                "Kin Groups", "Excl. single-HH kin groups"),
    n_by_year(readRDS(F4_FULL_HH_PATH),   "Households", "Incl. single-HH kin groups"),
    n_by_year(readRDS(F4_FULL_CLAN_PATH), "Kin Groups", "Incl. single-HH kin groups"))
  # Two panels: excl. single-HH kin groups (blue) | incl. single-HH kin groups (orange)
  f4_xbr  <- make_x_breaks_appendix(min(f4_df$year), max(f4_df$year))
  f4_ylim <- c(0, max(f4_df$value, na.rm = TRUE) * 1.05)
  # Used in: figure_appendix
  f4_panel <- function(sample_label, cols, ylab) {
    ggplot(dplyr::filter(f4_df, Sample == sample_label),
           aes(x = year, y = value, color = Unit, linetype = Unit)) +
      geom_line(linewidth = 1.7) +
      scale_color_manual(values = cols) +
      scale_linetype_manual(values = unit_ltys) +
      scale_y_continuous(limits = f4_ylim, labels = scales::comma) +
      scale_x_continuous(breaks = f4_xbr) +
      labs(x = NULL, y = ylab, subtitle = sample_label) +
      theme(legend.position = "none", plot.subtitle = element_text(hjust = 0.5))
  }
  f4_p1 <- f4_panel("Excl. single-HH kin groups",
                    c("Households" = NAVY,  "Kin Groups" = PALE_NAVY),  "N (unit-years)")
  f4_p2 <- f4_panel("Incl. single-HH kin groups",
                    c("Households" = CORAL, "Kin Groups" = PALE_CORAL), NULL)

  # Untitled version for the supplement Rmd, which labels figures with headings
  appendix_f4_notitle <- add_sens_legend(
    cowplot::plot_grid(f4_p1, f4_p2, nrow = 1),
    "Excl. single-HH kin groups", "Incl. single-HH kin groups", show_diff = FALSE)
  appendix_f4 <- add_sens_legend(
    cowplot::plot_grid(
      cowplot::ggdraw() +
        cowplot::draw_label("Appendix F4. Sample Size With and Without Single-Household Kin Groups",
                            fontfamily = base_family, fontface = "bold", size = title_size,
                            x = 0, hjust = 0),
      cowplot::plot_grid(f4_p1, f4_p2, nrow = 1),
      ncol = 1, rel_heights = c(0.08, 1)),
    "Excl. single-HH kin groups", "Incl. single-HH kin groups", show_diff = FALSE)
  if (SAVE_FILES) {
    ggsave(here("9_figures", "output", "appendixF4.pdf"), appendix_f4, width = 14, height = 11)
    message("Saved: appendixF4.pdf")
  }
} else {
  message("Appendix F4 skipped: set F4_FULL_HH_PATH / F4_FULL_CLAN_PATH to the full (non-robust) samples.")
}