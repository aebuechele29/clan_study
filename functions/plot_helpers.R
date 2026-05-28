# plot_helpers.R
# All ggplot2 / cowplot figure-building functions.
#
# Color scheme (Option B — Navy / Coral):
#   NAVY       #185FA5  main HH, diff main line, race HH
#   PALE_NAVY  #85B7EB  main KG, race KG
#   CORAL      #D85A30  alt HH, diff alt line
#   PALE_CORAL #F0997B  alt KG
#   PURPLE     #534AB7  C1 coefficient
#   TEAL       #0F6E56  C3 coefficient
#   NAVY                C2 coefficient


make_x_breaks <- function(min_yr, max_yr) {
  br <- seq(ceiling(min_yr / 10) * 10, floor(max_yr / 10) * 10, by = 10)
  br <- br[br > (min_yr + 1) & br < (max_yr - 1)]
  sort(unique(c(min_yr, br, max_yr)))
}

make_sub_draw <- function(txt) {
  cowplot::ggdraw() +
    cowplot::draw_label(txt, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)
}

# Figure 1 style: Gini over time (HH solid NAVY / KG dotted PALE_NAVY)
make_gini_plot <- function(by_year_df, hh_col, cl_col, ylab, y_limits = NULL) {
  dat <- by_year_df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(year = as.numeric(year))
  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  if (is.null(y_limits)) {
    vals <- c(dplyr::pull(dat, {{ hh_col }}), dplyr::pull(dat, {{ cl_col }}))
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  ggplot2::ggplot(dat, ggplot2::aes(x = year)) +
    ggplot2::geom_line(ggplot2::aes(y = {{ hh_col }}, linetype = "Household"),
                       color = NAVY, linewidth = 1.7) +
    ggplot2::geom_line(ggplot2::aes(y = {{ cl_col }}, linetype = "Kin Group"),
                       color = PALE_NAVY, linewidth = 1.7) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Kin Group" = "dotted")) +
    ggplot2::labs(x = NULL, y = ylab, linetype = NULL) +
    ggplot2::theme(
      legend.position  = "bottom",
      plot.title       = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(linetype = ggplot2::guide_legend(
      override.aes = list(color = c(NAVY, PALE_NAVY), linewidth = 1.2)))
}

# Figure 2 style: Lorenz curves
.lorenz_data <- function(df, value_var, weight_var, years, unit_label) {
  df %>%
    dplyr::filter(year %in% years, is.finite(.data[[value_var]])) %>%
    dplyr::transmute(year, value = .data[[value_var]], w = .data[[weight_var]]) %>%
    dplyr::group_split(year) %>%
    purrr::map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble::tibble())
      lorenz_tbl(d$value, d$w) %>%
        dplyr::mutate(year = unique(d$year),
                      Unit = factor(unit_label, levels = c("Household", "Kin Group")))
    })
}

make_lorenz_plot <- function(df_hh, df_cl, value_var, years, ylab, colors) {
  dat <- dplyr::bind_rows(
    .lorenz_data(df_hh, value_var, "fam_weight",  years, "Household"),
    .lorenz_data(df_cl, value_var, "clan_weight", years, "Kin Group")
  ) %>%
    dplyr::mutate(year_chr = as.character(year),
                  series   = paste0(year_chr, ": ", as.character(Unit)))

  yr_labels        <- as.character(sort(unique(years)))
  series_keys      <- c(paste0(yr_labels[1], ": Household"), paste0(yr_labels[1], ": Kin Group"),
                         paste0(yr_labels[2], ": Household"), paste0(yr_labels[2], ": Kin Group"))
  series_colors    <- setNames(c(colors[yr_labels[1]], colors[yr_labels[1]],
                                  colors[yr_labels[2]], colors[yr_labels[2]]), series_keys)
  series_linetypes <- setNames(c("solid", "dotted", "solid", "dotted"), series_keys)

  ggplot2::ggplot(dat, ggplot2::aes(x = p, y = L, color = series,
                                     linetype = series, group = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    ggplot2::scale_color_manual(values = series_colors, name = NULL) +
    ggplot2::scale_linetype_manual(values = series_linetypes, name = NULL) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_blank()) +
    ggplot2::guides(
      color    = ggplot2::guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 2))
}

# Figure 3 style: ratio plot (HH solid NAVY / KG dotted PALE_NAVY)
make_ratio_plot <- function(dat, ylab, y_limits = NULL, hline = 1) {
  d <- dat %>%
    dplyr::filter(is.finite(year), is.finite(r_hh_w_ratio), is.finite(r_cl_w_ratio))
  br <- make_x_breaks(min(d$year, na.rm = TRUE), max(d$year, na.rm = TRUE))

  if (is.null(y_limits)) {
    vals     <- c(d$r_hh_w_ratio, d$r_cl_w_ratio)
    vals     <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                  ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }

  ggplot2::ggplot(d, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = hline, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = r_hh_w_ratio, linetype = "Household"),
                       color = NAVY, linewidth = 1.7) +
    ggplot2::geom_line(ggplot2::aes(y = r_cl_w_ratio, linetype = "Kin Group"),
                       color = PALE_NAVY, linewidth = 1.7) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c("Household" = "solid", "Kin Group" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title      = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(linetype = ggplot2::guide_legend(
      nrow = 1,
      override.aes = list(color = c(NAVY, PALE_NAVY), linewidth = 1.2)))
}

# ── Sensitivity helpers ───────────────────────────────────────────────────────

.sens_base_t <- function() {
  ggplot2::theme(
    legend.position = "none",
    plot.title      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1))
}

.sens_sub_t <- function() {
  ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = sub_size * 0.6, hjust = 0.5))
}

# Legend: main = NAVY/PALE_NAVY, alt = CORAL/PALE_CORAL,
#         diff lines = NAVY (main) and CORAL (alt)
.sens_legend <- function() {
  leg_df <- data.frame(
    x   = 1:2, y = 1:2,
    ser = factor(
      c("Main HH", "Main Kin Group", "Alt HH", "Alt Kin Group",
        "Main HH-Kin Group diff", "Alt HH-Kin Group diff"),
      levels = c("Main HH", "Main Kin Group", "Alt HH", "Alt Kin Group",
                 "Main HH-Kin Group diff", "Alt HH-Kin Group diff")))
  donor <- ggplot2::ggplot(leg_df, ggplot2::aes(x = x, y = y, color = ser, linetype = ser)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(name = NULL, values = c(
      "Main HH"                = NAVY,
      "Main Kin Group"         = PALE_NAVY,
      "Alt HH"                 = CORAL,
      "Alt Kin Group"          = PALE_CORAL,
      "Main HH-Kin Group diff" = NAVY,
      "Alt HH-Kin Group diff"  = CORAL)) +
    ggplot2::scale_linetype_manual(name = NULL, values = c(
      "Main HH"                = "solid",
      "Main Kin Group"         = "dotted",
      "Alt HH"                 = "solid",
      "Alt Kin Group"          = "dotted",
      "Main HH-Kin Group diff" = "dashed",
      "Alt HH-Kin Group diff"  = "dashed")) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      color    = ggplot2::guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 2))
  cowplot::get_legend(donor)
}

# Race sensitivity legend: HH = NAVY, KG = PALE_NAVY for both subgroups
.race_sens_legend <- function() {
  leg_df <- data.frame(
    x   = 1:2, y = 1:2,
    ser = factor(c("Household", "Kin Group"), levels = c("Household", "Kin Group")))
  donor <- ggplot2::ggplot(leg_df, ggplot2::aes(x = x, y = y, color = ser, linetype = ser)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(name = NULL, values = c(
      "Household" = NAVY, "Kin Group" = PALE_NAVY)) +
    ggplot2::scale_linetype_manual(name = NULL, values = c(
      "Household" = "solid", "Kin Group" = "dotted")) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      color    = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 1))
  cowplot::get_legend(donor)
}

# ── Appendix C sensitivity: 3-panel (main | alt | diff) ──────────────────────
# Left panel (main):   NAVY/PALE_NAVY
# Middle panel (alt):  CORAL/PALE_CORAL
# Right panel (diff):  NAVY (main diff), CORAL (alt diff)
make_sensitivity_plot <- function(df, main_hh, main_cl, alt_hh, alt_cl,
                                   left_label  = "Main Results",
                                   right_label = "Alternative",
                                   y_limits    = NULL,
                                   diff_limits = NULL) {
  dat <- df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::rename(
      main_hh_ = dplyr::all_of(main_hh), main_cl_ = dplyr::all_of(main_cl),
      alt_hh_  = dplyr::all_of(alt_hh),  alt_cl_  = dplyr::all_of(alt_cl)) %>%
    dplyr::mutate(main_diff = main_hh_ - main_cl_, alt_diff = alt_hh_ - alt_cl_)

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

  pA <- ggplot2::ggplot(
    dat %>% dplyr::select(year, Household = main_hh_, `Kin Group` = main_cl_) %>%
      tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini"),
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(values = c("Household" = NAVY, "Kin Group" = PALE_NAVY)) +
    ggplot2::scale_linetype_manual(values = c("Household" = "solid", "Kin Group" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "Gini Coefficient", color = NULL, linetype = NULL,
                  subtitle = left_label) +
    .sens_base_t() + .sens_sub_t()

  pB <- ggplot2::ggplot(
    dat %>% dplyr::select(year, Household = alt_hh_, `Kin Group` = alt_cl_) %>%
      tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini"),
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(values = c("Household" = CORAL, "Kin Group" = PALE_CORAL)) +
    ggplot2::scale_linetype_manual(values = c("Household" = "solid", "Kin Group" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = NULL, color = NULL, linetype = NULL,
                  subtitle = right_label) +
    .sens_base_t() + .sens_sub_t()

  long_diff <- dat %>%
    dplyr::select(year, main_diff, alt_diff) %>%
    tidyr::pivot_longer(-year, names_to = "spec", values_to = "Difference") %>%
    dplyr::mutate(spec = dplyr::recode(spec,
      "main_diff" = "Main HH-Kin Group diff", "alt_diff" = "Alt HH-Kin Group diff"))

  pC <- ggplot2::ggplot(long_diff, ggplot2::aes(x = year, y = Difference, color = spec)) +
    ggplot2::geom_line(data = long_diff %>% dplyr::filter(spec == "Main HH-Kin Group diff"),
                       linewidth = 1.7, linetype = "dashed") +
    ggplot2::geom_line(data = long_diff %>% dplyr::filter(spec == "Alt HH-Kin Group diff"),
                       linewidth = 1.7, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(name = NULL,
      values = c("Main HH-Kin Group diff" = NAVY, "Alt HH-Kin Group diff" = CORAL)) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "HH \u2212 Kin Group Gini",
                  subtitle = "HH \u2212 Kin Group Difference") +
    .sens_base_t() + .sens_sub_t()

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

make_sensitivity_figure <- function(plot_inc, plot_w, title_str,
                                     sub_a = "Panel A: Income",
                                     sub_b = "Panel B: Wealth",
                                     show_title = TRUE,
                                     race_legend = FALSE) {
  leg <- if (race_legend) .race_sens_legend() else .sens_legend()
  if (show_title) {
    title_grob <- cowplot::ggdraw() +
      cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                          fontfamily = base_family, size = title_size)
    cowplot::plot_grid(
      title_grob, make_sub_draw(sub_a), plot_inc, make_sub_draw(sub_b), plot_w, leg,
      ncol = 1, rel_heights = c(0.08, 0.05, 1, 0.05, 1, 0.12))
  } else {
    cowplot::plot_grid(
      make_sub_draw(sub_a), plot_inc, make_sub_draw(sub_b), plot_w, leg,
      ncol = 1, rel_heights = c(0.05, 1, 0.05, 1, 0.12))
  }
}

make_single_sensitivity_figure <- function(plot_w, title_str, show_title = TRUE) {
  leg <- .sens_legend()
  if (show_title) {
    title_grob <- cowplot::ggdraw() +
      cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                          fontfamily = base_family, size = title_size)
    cowplot::plot_grid(title_grob, plot_w, leg, ncol = 1, rel_heights = c(0.08, 1, 0.12))
  } else {
    cowplot::plot_grid(plot_w, leg, ncol = 1, rel_heights = c(1, 0.12))
  }
}

# ── Appendix C1: race sensitivity ─────────────────────────────────────────────
# Both Black and Non-Black panels use NAVY (HH) / PALE_NAVY (KG).
# Diff panel uses NAVY for Black diff, CORAL for Non-Black diff.
make_race_sensitivity_plot <- function(dat, hh_black, cl_black, hh_nonblack, cl_nonblack,
                                        y_limits = NULL, diff_limits = NULL,
                                        ylab = "Gini Coefficient") {
  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  if (is.null(y_limits)) {
    vals <- c(dat[[hh_black]], dat[[cl_black]], dat[[hh_nonblack]], dat[[cl_nonblack]])
    y_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                  ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(dat[[hh_black]] - dat[[cl_black]], dat[[hh_nonblack]] - dat[[cl_nonblack]])
    diff_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                     ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }

  pA <- ggplot2::ggplot(
    dat %>% dplyr::transmute(year, Household = .data[[hh_black]],
                              `Kin Group` = .data[[cl_black]]) %>%
      tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini"),
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(values = c("Household" = NAVY, "Kin Group" = PALE_NAVY)) +
    ggplot2::scale_linetype_manual(values = c("Household" = "solid", "Kin Group" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = ylab, subtitle = "Black", color = NULL, linetype = NULL) +
    .sens_base_t() + .sens_sub_t()

  pB <- ggplot2::ggplot(
    dat %>% dplyr::transmute(year, Household = .data[[hh_nonblack]],
                              `Kin Group` = .data[[cl_nonblack]]) %>%
      tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini"),
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(values = c("Household" = CORAL, "Kin Group" = PALE_CORAL)) +
    ggplot2::scale_linetype_manual(values = c("Household" = "solid", "Kin Group" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = "Non-Black", color = NULL, linetype = NULL) +
    .sens_base_t() + .sens_sub_t()

  diff_dat <- dat %>%
    dplyr::transmute(year,
                     black_diff    = .data[[hh_black]]    - .data[[cl_black]],
                     nonblack_diff = .data[[hh_nonblack]] - .data[[cl_nonblack]]) %>%
    tidyr::pivot_longer(-year, names_to = "race", values_to = "Difference") %>%
    dplyr::mutate(race = dplyr::recode(race,
      "black_diff" = "Black", "nonblack_diff" = "Non-Black"))

  pC <- ggplot2::ggplot(diff_dat,
                         ggplot2::aes(x = year, y = Difference, color = race, linetype = race)) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(name = NULL,
      values = c("Black" = NAVY, "Non-Black" = CORAL)) +
    ggplot2::scale_linetype_manual(name = NULL,
      values = c("Black" = "dashed", "Non-Black" = "dashed")) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "HH \u2212 Kin Group Gini",
                  subtitle = "HH \u2212 Kin Group Difference") +
    .sens_base_t() + .sens_sub_t()

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

# ── Appendix D: C1/C2/C3 panel ───────────────────────────────────────────────
# C1 = PURPLE, C2 = NAVY, C3 = TEAL
make_c123_panel <- function(dat, y_limits = NULL, show_unit,
                             ylab = "Inequality Coefficient") {
  coef_colors <- c("C1 (Bonferroni)" = PURPLE, "C2 (Gini)" = NAVY, "C3 (Upper Tail)" = TEAL)
  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  if (show_unit == "Diff") {
    diff_dat <- dat %>%
      dplyr::transmute(year,
                       C1_diff = C1_hh - C1_clan,
                       C2_diff = C2_hh - C2_clan,
                       C3_diff = C3_hh - C3_clan) %>%
      tidyr::pivot_longer(-year, names_to = "coef", values_to = "value") %>%
      dplyr::mutate(coef = dplyr::recode(coef,
        "C1_diff" = "C1 (Bonferroni)", "C2_diff" = "C2 (Gini)", "C3_diff" = "C3 (Upper Tail)"))
    if (is.null(y_limits)) {
      vals     <- diff_dat$value[is.finite(diff_dat$value)]
      y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
    }
    return(
      ggplot2::ggplot(diff_dat,
                      ggplot2::aes(x = year, y = value, color = coef, group = coef)) +
        ggplot2::geom_line(linewidth = 1.7, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
        ggplot2::scale_color_manual(values = coef_colors, name = NULL) +
        ggplot2::scale_y_continuous(limits = y_limits) +
        ggplot2::scale_x_continuous(breaks = br,
                                     expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
        ggplot2::labs(x = NULL, y = "HH \u2212 Kin Group",
                      subtitle = "HH \u2212 Kin Group Difference") +
        .sens_base_t() + .sens_sub_t())
  }

  long <- dat %>%
    dplyr::select(year, C1_hh, C1_clan, C2_hh, C2_clan, C3_hh, C3_clan) %>%
    tidyr::pivot_longer(-year, names_to = "series", values_to = "value") %>%
    dplyr::mutate(
      Coefficient = dplyr::case_when(
        grepl("C1", series) ~ "C1 (Bonferroni)",
        grepl("C2", series) ~ "C2 (Gini)",
        grepl("C3", series) ~ "C3 (Upper Tail)"),
      Unit = ifelse(grepl("_hh", series), "Household", "Kin Group")) %>%
    dplyr::filter(Unit == show_unit)

  if (is.null(y_limits)) {
    vals     <- long$value[is.finite(long$value)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  ggplot2::ggplot(long, ggplot2::aes(x = year, y = value,
                                      color = Coefficient, group = Coefficient)) +
    ggplot2::geom_line(linewidth = 1.7,
                       linetype = if (show_unit == "Kin Group") "dotted" else "solid") +
    ggplot2::scale_color_manual(values = coef_colors, name = NULL) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = ylab,
                  subtitle = if (show_unit == "Household") "Households" else "Kin Groups") +
    .sens_base_t() + .sens_sub_t()
}

# ── Appendix E sensitivity: ratio 3-panel (main | alt | diff) ────────────────
# Left panel (main):   NAVY/PALE_NAVY
# Middle panel (alt):  CORAL/PALE_CORAL
# Right panel (diff):  NAVY (main diff), CORAL (alt diff)
make_ratio_sensitivity_plot <- function(dat, main_hh, main_cl, alt_hh, alt_cl,
                                         left_label  = "Main Results",
                                         right_label = "Alternative",
                                         y_limits    = NULL,
                                         diff_limits = NULL) {
  wide <- dat %>%
    dplyr::filter(is.finite(year)) %>%
    dplyr::mutate(main_diff = .data[[main_hh]] - .data[[main_cl]],
                  alt_diff  = .data[[alt_hh]]  - .data[[alt_cl]])

  if (is.null(y_limits)) {
    vals <- c(wide[[main_hh]], wide[[main_cl]], wide[[alt_hh]], wide[[alt_cl]])
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(wide$main_diff, wide$alt_diff)
    vals <- vals[is.finite(vals)]
    diff_limits <- c(floor(min(vals) * 20) / 20, ceiling(max(vals) * 20) / 20 + 0.05)
  }

  br <- make_x_breaks(min(wide$year, na.rm = TRUE), max(wide$year, na.rm = TRUE))

  pA <- ggplot2::ggplot(wide, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[main_hh]]),
                       color = NAVY,      linewidth = 1.7, linetype = "solid") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[main_cl]]),
                       color = PALE_NAVY, linewidth = 1.7, linetype = "dotted") +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "Black / Non-Black Ratio", subtitle = left_label) +
    .sens_base_t() + .sens_sub_t()

  pB <- ggplot2::ggplot(wide, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[alt_hh]]),
                       color = CORAL,      linewidth = 1.7, linetype = "solid") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[alt_cl]]),
                       color = PALE_CORAL, linewidth = 1.7, linetype = "dotted") +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = right_label) +
    .sens_base_t() + .sens_sub_t()

  long_diff <- wide %>%
    dplyr::select(year, main_diff, alt_diff) %>%
    tidyr::pivot_longer(-year, names_to = "spec", values_to = "Difference") %>%
    dplyr::mutate(spec = dplyr::recode(spec,
      "main_diff" = "Main HH-Kin Group diff", "alt_diff" = "Alt HH-Kin Group diff"))

  pC <- ggplot2::ggplot(long_diff, ggplot2::aes(x = year, y = Difference, color = spec)) +
    ggplot2::geom_line(data = long_diff %>% dplyr::filter(spec == "Main HH-Kin Group diff"),
                       linewidth = 1.7, linetype = "dashed") +
    ggplot2::geom_line(data = long_diff %>% dplyr::filter(spec == "Alt HH-Kin Group diff"),
                       linewidth = 1.7, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(name = NULL,
      values = c("Main HH-Kin Group diff" = NAVY, "Alt HH-Kin Group diff" = CORAL)) +
    ggplot2::scale_y_continuous(limits = c(min(long_diff$Difference, na.rm = TRUE) - 0.05,
                                            max(long_diff$Difference, na.rm = TRUE) + 0.05)) +
    ggplot2::coord_cartesian(ylim = diff_limits) +
    ggplot2::scale_x_continuous(breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "HH \u2212 Kin Group Diff",
                  subtitle = "HH \u2212 Kin Group Difference") +
    .sens_base_t() + .sens_sub_t()

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

make_ratio_sensitivity_figure <- function(plot_a, plot_b, title_str,
                                           sub_a = "Panel A: Median wealth ratio",
                                           sub_b = "Panel B: Mean wealth ratio",
                                           show_title = FALSE) {
  leg <- .sens_legend()
  if (show_title) {
    title_grob <- cowplot::ggdraw() +
      cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                          fontfamily = base_family, size = title_size)
    cowplot::plot_grid(
      title_grob, make_sub_draw(sub_a), plot_a, make_sub_draw(sub_b), plot_b, leg,
      ncol = 1, rel_heights = c(0.08, 0.05, 1, 0.05, 1, 0.12))
  } else {
    cowplot::plot_grid(
      make_sub_draw(sub_a), plot_a, make_sub_draw(sub_b), plot_b, leg,
      ncol = 1, rel_heights = c(0.05, 1, 0.05, 1, 0.12))
  }
}