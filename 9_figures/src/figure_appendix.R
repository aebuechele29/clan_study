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


# ── Appendix C: Gini Sensitivity Analyses (C1–C6) ────────────────────────────

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
  y_limits = shared_y_race, diff_limits = shared_diff_race)
race6_w <- make_race_sensitivity_plot(
  w_race_yr,
  "r_hh_w_wealth_black",    "r_cl_w_wealth_black",
  "r_hh_w_wealth_nonblack", "r_cl_w_wealth_nonblack",
  y_limits = shared_y_race, diff_limits = shared_diff_race)

sensitivity_gini_c1 <- make_sensitivity_figure(
  race6_inc, race6_w,
  title_str = "Appendix C1. Gini Coefficients by Race Subgroup", show_title = FALSE,
  sub_a = "Panel A: Income",
  sub_b = "Panel B: Wealth (incl. home equity)",
  race_legend = TRUE)
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixC1.pdf"),
         sensitivity_gini_c1, width = 14, height = 11)
  message("Saved: appendixC1.pdf")
}

# C2 — Sample: single-household kin groups ────────────────────────────────────
sensitivity_gini_c2 <- make_sensitivity_figure(
  make_sensitivity_plot(df = income_all,
    main_hh = "r_hh_w_inc", main_cl = "r_cl_w_inc",
    alt_hh  = "hh_w_inc",   alt_cl  = "cl_w_inc",
    left_label = "Excl. single-HH kin groups", right_label = "Incl. single-HH kin groups"),
  make_sensitivity_plot(df = wealth_wh_all,
    main_hh = "r_hh_w_wealth", main_cl = "r_cl_w_wealth",
    alt_hh  = "hh_w_wealth",   alt_cl  = "cl_w_wealth",
    left_label = "Excl. single-HH kin groups", right_label = "Incl. single-HH kin groups"),
  title_str = "Appendix C2. Sensitivity: Single-Household Kin Groups", show_title = FALSE)
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixC2.pdf"),
         sensitivity_gini_c2, width = 14, height = 11)
  message("Saved: appendixC2.pdf")
}

# C3 — Wealth without home equity ─────────────────────────────────────────────
wealth_c3_df <- wealth_wh_all %>%
  left_join(
    wealth_nh_all %>% dplyr::select(year,
                                     r_hh_w_wealth_nh = r_hh_w_wealth,
                                     r_cl_w_wealth_nh = r_cl_w_wealth),
    by = "year")

sensitivity_gini_c3 <- make_single_sensitivity_figure(
  make_sensitivity_plot(df = wealth_c3_df,
    main_hh = "r_hh_w_wealth",    main_cl = "r_cl_w_wealth",
    alt_hh  = "r_hh_w_wealth_nh", alt_cl  = "r_cl_w_wealth_nh",
    left_label = "Incl. home equity", right_label = "Excl. home equity"),
  title_str = "Appendix C3. Sensitivity: Wealth With and Without Home Equity", show_title = FALSE)
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixC3.pdf"),
         sensitivity_gini_c3, width = 14, height = 11)
  message("Saved: appendixC3.pdf")
}

# C4 — Negative values ────────────────────────────────────────────────────────
sensitivity_gini_c4 <- make_sensitivity_figure(
  make_sensitivity_plot(df = income_all,
    main_hh = "r_hh_w_inc",   main_cl = "r_cl_w_inc",
    alt_hh  = "neg_r_hh_inc", alt_cl  = "neg_r_cl_inc",
    left_label = "Excl. negative values", right_label = "Incl. negative values"),
  make_sensitivity_plot(df = wealth_wh_all,
    main_hh = "r_hh_w_wealth",   main_cl = "r_cl_w_wealth",
    alt_hh  = "neg_r_hh_wealth", alt_cl  = "neg_r_cl_wealth",
    left_label = "Excl. negative values", right_label = "Incl. negative values"),
  title_str = "Appendix C4. Sensitivity: Negative Values", show_title = FALSE)
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixC4.pdf"),
         sensitivity_gini_c4, width = 14, height = 11)
  message("Saved: appendixC4.pdf")
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

sensitivity_gini_c5 <- make_sensitivity_figure(
  make_sensitivity_plot(df = inc_size_tbl,
    main_hh = "current_hh", main_cl = "current_clan",
    alt_hh  = "unadj_hh",   alt_cl  = "unadj_clan",
    left_label = "Size-standardised", right_label = "Unadjusted"),
  make_sensitivity_plot(df = wealth_size_tbl,
    main_hh = "current_hh", main_cl = "current_clan",
    alt_hh  = "unadj_hh",   alt_cl  = "unadj_clan",
    left_label = "Size-standardised", right_label = "Unadjusted"),
  title_str = "Appendix C5. Sensitivity: Size Standardization", show_title = FALSE)
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixC5.pdf"),
         sensitivity_gini_c5, width = 14, height = 11)
  message("Saved: appendixC5.pdf")
}

# C6 — Weighting ──────────────────────────────────────────────────────────────
sensitivity_gini_c6 <- make_sensitivity_figure(
  make_sensitivity_plot(df = income_all,
    main_hh = "r_hh_w_inc",   main_cl = "r_cl_w_inc",
    alt_hh  = "r_hh_unw_inc", alt_cl  = "r_cl_unw_inc",
    left_label = "Weighted", right_label = "Unweighted"),
  make_sensitivity_plot(df = wealth_wh_all,
    main_hh = "r_hh_w_wealth",   main_cl = "r_cl_w_wealth",
    alt_hh  = "r_hh_unw_wealth", alt_cl  = "r_cl_unw_wealth",
    left_label = "Weighted", right_label = "Unweighted"),
  title_str = "Appendix C6. Sensitivity: Weighting", show_title = FALSE)
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixC6.pdf"),
         sensitivity_gini_c6, width = 14, height = 11)
  message("Saved: appendixC6.pdf")
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

inc_pHH   <- make_c123_panel(inc_c123_yr, shared_y_c123,    "Household", "Inequality Coefficient")
inc_pClan <- make_c123_panel(inc_c123_yr, shared_y_c123,    "Kin Group", NULL)
inc_pDiff <- make_c123_panel(inc_c123_yr, shared_diff_c123, "Diff",      NULL)
wnh_pHH   <- make_c123_panel(wnh_c123_yr, shared_y_c123,    "Household", "Inequality Coefficient")
wnh_pClan <- make_c123_panel(wnh_c123_yr, shared_y_c123,    "Kin Group", NULL)
wnh_pDiff <- make_c123_panel(wnh_c123_yr, shared_diff_c123, "Diff",      NULL)

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
  cowplot::get_legend(c123_legend_donor),
  ncol = 1, rel_heights = c(1, 0.06))

if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixD.pdf"),
         appendix_d, width = 14, height = 13)
  message("Saved: appendixD.pdf")
}


# ── Appendix E1: Mean and Median Wealth Ratios ────────────────────────────────
appendix_e1_med_plot <- make_ratio_plot(
  wealth_ratios %>% select(year,
                           r_hh_w_ratio = r_hh_w_median_ratio,
                           r_cl_w_ratio = r_cl_w_median_ratio),
  ylab = "Black / Non-Black")

appendix_e1_mean_plot <- make_ratio_plot(
  wealth_ratios %>% select(year,
                           r_hh_w_ratio = r_hh_w_mean_ratio,
                           r_cl_w_ratio = r_cl_w_mean_ratio),
  ylab = "Black / Non-Black")

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
    "Appendix E1. Black / Non-Black Wealth Ratios: Households vs. Kin Groups",
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
  ggsave(here("9_figures", "output", "appendixE1.pdf"),
         appendix_e1, width = 14, height = 11)
  message("Saved: appendixE1.pdf")
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
sensitivity_ratio_e2 <- make_ratio_sensitivity_figure(
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
    }),
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
    }),
  title_str = "Appendix E2. Race Ratios: Single-Household Kin Groups")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE2.pdf"),
         sensitivity_ratio_e2, width = 14, height = 11)
  message("Saved: appendixE2.pdf")
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

sensitivity_ratio_e3 <- make_ratio_sensitivity_figure(
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
    }),
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
    }),
  title_str = "Appendix E3. Race Ratios: Wealth With and Without Home Equity")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE3.pdf"),
         sensitivity_ratio_e3, width = 14, height = 11)
  message("Saved: appendixE3.pdf")
}


# E4 — Negative values ────────────────────────────────────────────────────────
sensitivity_ratio_e4 <- make_ratio_sensitivity_figure(
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
    }),
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
    }),
  title_str = "Appendix E4. Race Ratios: Negative Values")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE4.pdf"),
         sensitivity_ratio_e4, width = 14, height = 11)
  message("Saved: appendixE4.pdf")
}


# E5 — Size standardization ───────────────────────────────────────────────────
sensitivity_ratio_e5 <- make_ratio_sensitivity_figure(
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
    }),
  make_ratio_sensitivity_plot(wealth_ratios,
    main_hh = "r_hh_w_mean_ratio",       main_cl = "r_cl_w_mean_ratio",
    alt_hh  = "r_hh_w_unadj_mean_ratio", alt_cl  = "r_cl_w_unadj_mean_ratio",
    left_label = "Size-standardised", right_label = "Unadjusted",
    y_limits    = shared_y_rsens,
    diff_limits = c(-0.05, 0.20)),
  title_str = "Appendix E5. Race Ratios: Size Standardization")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE5.pdf"),
         sensitivity_ratio_e5, width = 14, height = 11)
  message("Saved: appendixE5.pdf")
}


# E6 — Weighting ──────────────────────────────────────────────────────────────
sensitivity_ratio_e6 <- make_ratio_sensitivity_figure(
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
    }),
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
    }),
  title_str = "Appendix E6. Race Ratios: Weighting")
if (SAVE_FILES) {
  ggsave(here("9_figures", "output", "appendixE6.pdf"),
         sensitivity_ratio_e6, width = 14, height = 11)
  message("Saved: appendixE6.pdf")
}
