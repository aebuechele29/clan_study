# plot_helpers.R
# All ggplot2 / cowplot figure-building functions.


make_x_breaks <- function(min_yr, max_yr) {
  br <- seq(ceiling(min_yr / 10) * 10, floor(max_yr / 10) * 10, by = 10)
  br <- br[br > (min_yr + 4) & br < (max_yr - 4)]
  sort(unique(c(min_yr, br, max_yr)))
}

# Thin ggdraw wrapper for panel sub-headings
make_sub_draw <- function(txt) {
  cowplot::ggdraw() +
    cowplot::draw_label(txt, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)
}

# Figure 1 style: Gini over time (HH solid / Clan dotted) ────────────────────

make_gini_plot <- function(by_year_df, hh_col, cl_col, ylab,
                            y_limits = NULL) {
  dat <- by_year_df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(year = as.numeric(year))
  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  if (is.null(y_limits)) {
    vals <- c(dplyr::pull(dat, {{ hh_col }}),
              dplyr::pull(dat, {{ cl_col }}))
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20,
                  ceiling(max(vals) * 20) / 20 + 0.05)
  }

  ggplot2::ggplot(dat, ggplot2::aes(x = year)) +
    ggplot2::geom_line(
      ggplot2::aes(y = {{ hh_col }}, linetype = "Household"),
      color = ORANGE, linewidth = 1.7) +
    ggplot2::geom_line(
      ggplot2::aes(y = {{ cl_col }}, linetype = "Clan"),
      color = PALE_ORANGE, linewidth = 1.7) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::labs(x = NULL, y = ylab, linetype = NULL) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(linetype = ggplot2::guide_legend(
      override.aes = list(color = c(PALE_ORANGE, ORANGE), linewidth = 1.2)
    ))
}

# Figure 2 style: Lorenz curves ───────────────────────────────────────────────

.lorenz_data <- function(df, value_var, weight_var, years, unit_label) {
  df %>%
    dplyr::filter(year %in% years, is.finite(.data[[value_var]])) %>%
    dplyr::transmute(year, value = .data[[value_var]],
                     w = .data[[weight_var]]) %>%
    dplyr::group_split(year) %>%
    purrr::map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble::tibble())
      lorenz_tbl(d$value, d$w) %>%
        dplyr::mutate(
          year = unique(d$year),
          Unit = factor(unit_label, levels = c("Household", "Clan"))
        )
    })
}

make_lorenz_plot <- function(df_hh, df_cl, value_var, years, ylab, colors) {
  dat <- dplyr::bind_rows(
    .lorenz_data(df_hh, value_var, "fam_weight",  years, "Household"),
    .lorenz_data(df_cl, value_var, "clan_weight", years, "Clan")
  ) %>%
    dplyr::mutate(
      year_chr = as.character(year),
      series   = paste0(year_chr, ": ", as.character(Unit))
    )

  yr_labels <- as.character(sort(unique(years)))
  series_keys <- c(
    paste0(yr_labels[1], ": Household"), paste0(yr_labels[1], ": Clan"),
    paste0(yr_labels[2], ": Household"), paste0(yr_labels[2], ": Clan")
  )
  series_colors <- setNames(
    c(colors[yr_labels[1]], colors[yr_labels[1]],
      colors[yr_labels[2]], colors[yr_labels[2]]),
    series_keys
  )
  series_linetypes <- setNames(
    c("solid", "dotted", "solid", "dotted"), series_keys
  )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = p, y = L, color = series,
                 linetype = series, group = series)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "grey50") +
    ggplot2::scale_color_manual(values = series_colors,    name = NULL) +
    ggplot2::scale_linetype_manual(values = series_linetypes, name = NULL) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_blank()) +
    ggplot2::guides(
      color    = ggplot2::guide_legend(
        nrow = 2, override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 2)
    )
}

# Figure 3 style: ratio plot (HH solid / Clan dotted) ────────────────────────

make_ratio_plot <- function(dat, ylab, y_limits = NULL, hline = 1) {
  d <- dat %>%
    dplyr::filter(is.finite(year),
                  is.finite(r_hh_w_ratio),
                  is.finite(r_cl_w_ratio))

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
                       color = ORANGE,      linewidth = 1.7) +
    ggplot2::geom_line(ggplot2::aes(y = r_cl_w_ratio, linetype = "Clan"),
                       color = PALE_ORANGE, linewidth = 1.7) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c("Household" = "solid", "Clan" = "dotted")
    ) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = ylab) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title      = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(color = c(PALE_ORANGE, ORANGE), linewidth = 1.2))
    )
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

.sens_legend <- function() {
  DIFF_LABEL_MAIN <- "Main HH-Clan diff"
  DIFF_LABEL_ALT  <- "Alt HH-Clan diff"
  leg_df <- data.frame(
    x   = 1:2,
    y   = 1:2,
    ser = factor(
      c("Main HH", "Main Clan", "Alt HH", "Alt Clan",
        DIFF_LABEL_MAIN, DIFF_LABEL_ALT),
      levels = c("Main HH", "Main Clan", "Alt HH", "Alt Clan",
                 DIFF_LABEL_MAIN, DIFF_LABEL_ALT)
    )
  )
  donor <- ggplot2::ggplot(leg_df,
                            ggplot2::aes(x = x, y = y,
                                         color = ser, linetype = ser)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = c(
        "Main HH"           = ORANGE,
        "Main Clan"         = PALE_ORANGE,
        "Alt HH"            = BLUE,
        "Alt Clan"          = PALE_BLUE,
        "Main HH-Clan diff" = ORANGE,
        "Alt HH-Clan diff"  = BLUE
      )
    ) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c(
        "Main HH"           = "solid",
        "Main Clan"         = "dotted",
        "Alt HH"            = "solid",
        "Alt Clan"          = "dotted",
        "Main HH-Clan diff" = "dashed",
        "Alt HH-Clan diff"  = "dashed"
      )
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      color    = ggplot2::guide_legend(nrow = 2,
                                        override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 2)
    )
  cowplot::get_legend(donor)
}

.race_sens_legend <- function() {
  leg_df <- data.frame(
    x   = 1:2,
    y   = 1:2,
    ser = factor(
      c("Black HH", "Black Clan", "Non-Black HH", "Non-Black Clan",
        "Black HH-Clan diff", "Non-Black HH-Clan diff"),
      levels = c("Black HH", "Black Clan", "Non-Black HH", "Non-Black Clan",
                 "Black HH-Clan diff", "Non-Black HH-Clan diff")
    )
  )
  donor <- ggplot2::ggplot(leg_df,
                            ggplot2::aes(x = x, y = y,
                                         color = ser, linetype = ser)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = c(
        "Black HH"               = ORANGE,
        "Black Clan"             = PALE_ORANGE,
        "Non-Black HH"           = BLUE,
        "Non-Black Clan"         = PALE_BLUE,
        "Black HH-Clan diff"     = ORANGE,
        "Non-Black HH-Clan diff" = BLUE
      )
    ) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c(
        "Black HH"               = "solid",
        "Black Clan"             = "dotted",
        "Non-Black HH"           = "solid",
        "Non-Black Clan"         = "dotted",
        "Black HH-Clan diff"     = "dashed",
        "Non-Black HH-Clan diff" = "dashed"
      )
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      color    = ggplot2::guide_legend(nrow = 2,
                                        override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 2)
    )
  cowplot::get_legend(donor)
}

# ── Appendix C sensitivity style: 3-panel (main | alt | HH-Clan diff) ────────

make_sensitivity_plot <- function(df, main_hh, main_cl, alt_hh, alt_cl,
                                   left_label  = "Main Results",
                                   right_label = "Alternative",
                                   y_limits    = NULL,
                                   diff_limits = NULL) {
  dat <- df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::rename(
      main_hh_ = dplyr::all_of(main_hh),
      main_cl_ = dplyr::all_of(main_cl),
      alt_hh_  = dplyr::all_of(alt_hh),
      alt_cl_  = dplyr::all_of(alt_cl)
    ) %>%
    dplyr::mutate(
      main_diff = main_hh_ - main_cl_,
      alt_diff  = alt_hh_  - alt_cl_
    )

  if (is.null(y_limits)) {
    vals <- c(dat$main_hh_, dat$main_cl_, dat$alt_hh_, dat$alt_cl_)
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20,
                  ceiling(max(vals) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(dat$main_diff, dat$alt_diff)
    vals <- vals[is.finite(vals)]
    diff_limits <- c(floor(min(vals) * 20) / 20,
                     ceiling(max(vals) * 20) / 20 + 0.05)
  }

  br <- make_x_breaks(min(dat$year, na.rm = TRUE), max(dat$year, na.rm = TRUE))

  long_main <- dat %>%
    dplyr::select(year, Household = main_hh_, Clan = main_cl_) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pA <- ggplot2::ggplot(
    long_main,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = ORANGE, "Clan" = PALE_ORANGE)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "Gini Coefficient",
                  color = NULL, linetype = NULL, subtitle = left_label) +
    .sens_base_t() + .sens_sub_t()

  long_alt <- dat %>%
    dplyr::select(year, Household = alt_hh_, Clan = alt_cl_) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pB <- ggplot2::ggplot(
    long_alt,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = BLUE, "Clan" = PALE_BLUE)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = NULL,
                  color = NULL, linetype = NULL, subtitle = right_label) +
    .sens_base_t() + .sens_sub_t()

  long_diff <- dat %>%
    dplyr::select(year, main_diff, alt_diff) %>%
    tidyr::pivot_longer(-year, names_to = "spec", values_to = "Difference") %>%
    dplyr::mutate(spec = dplyr::recode(spec,
      "main_diff" = "Main HH-Clan diff",
      "alt_diff"  = "Alt HH-Clan diff"
    ))

  pC <- ggplot2::ggplot(
    long_diff,
    ggplot2::aes(x = year, y = Difference, color = spec, linetype = spec)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = c("Main HH-Clan diff" = ORANGE, "Alt HH-Clan diff" = BLUE)) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c("Main HH-Clan diff" = "dashed", "Alt HH-Clan diff" = "dashed")) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "HH \u2212 Clan Gini",
                  subtitle = "HH \u2212 Clan Difference") +
    .sens_base_t() + .sens_sub_t()

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

# Wrapper: title + Panel A + Panel B + single shared legend
make_sensitivity_figure <- function(plot_inc, plot_w, title_str,
                                     sub_a = "Panel A: Income",
                                     sub_b = "Panel B: Wealth",
                                     show_title = TRUE,
                                     race_legend = FALSE) {
  sub_a_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_a, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)
  sub_b_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_b, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)

  leg <- if (race_legend) .race_sens_legend() else .sens_legend()

  if (show_title) {
    title_grob <- cowplot::ggdraw() +
      cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                          fontfamily = base_family, size = title_size)
    cowplot::plot_grid(
      title_grob, sub_a_grob, plot_inc, sub_b_grob, plot_w, leg,
      ncol = 1, rel_heights = c(0.08, 0.05, 1, 0.05, 1, 0.12)
    )
  } else {
    cowplot::plot_grid(
      sub_a_grob, plot_inc, sub_b_grob, plot_w, leg,
      ncol = 1, rel_heights = c(0.05, 1, 0.05, 1, 0.12)
    )
  }
}

# Wealth-only sensitivity figure: single plot + shared legend
make_single_sensitivity_figure <- function(plot_w, title_str, show_title = TRUE) {
  leg <- .sens_legend()
  if (show_title) {
    title_grob <- cowplot::ggdraw() +
      cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                          fontfamily = base_family, size = title_size)
    cowplot::plot_grid(title_grob, plot_w, leg,
                       ncol = 1, rel_heights = c(0.08, 1, 0.12))
  } else {
    cowplot::plot_grid(plot_w, leg,
                       ncol = 1, rel_heights = c(1, 0.12))
  }
}

# ── Appendix C1 style: race sensitivity (Black | Non-Black | diff) ────────────

make_race_sensitivity_plot <- function(dat,
                                        hh_black, cl_black,
                                        hh_nonblack, cl_nonblack,
                                        y_limits    = NULL,
                                        diff_limits = NULL,
                                        ylab        = "Gini Coefficient") {
  br <- make_x_breaks(min(dat$year, na.rm = TRUE),
                       max(dat$year, na.rm = TRUE))

  if (is.null(y_limits)) {
    vals <- c(dat[[hh_black]], dat[[cl_black]], dat[[hh_nonblack]], dat[[cl_nonblack]])
    y_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                  ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(dat[[hh_black]] - dat[[cl_black]],
              dat[[hh_nonblack]] - dat[[cl_nonblack]])
    diff_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                     ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }

  long_black <- dat %>%
    dplyr::transmute(year,
                     Household = .data[[hh_black]],
                     Clan      = .data[[cl_black]]) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pA <- ggplot2::ggplot(
    long_black,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = ORANGE, "Clan" = PALE_ORANGE)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = ylab, subtitle = "Black",
                  color = NULL, linetype = NULL) +
    .sens_base_t() + .sens_sub_t()

  long_nonblack <- dat %>%
    dplyr::transmute(year,
                     Household = .data[[hh_nonblack]],
                     Clan      = .data[[cl_nonblack]]) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pB <- ggplot2::ggplot(
    long_nonblack,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = BLUE, "Clan" = PALE_BLUE)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = "Non-Black",
                  color = NULL, linetype = NULL) +
    .sens_base_t() + .sens_sub_t()

  diff_dat <- dat %>%
    dplyr::transmute(
      year,
      black_diff    = .data[[hh_black]]    - .data[[cl_black]],
      nonblack_diff = .data[[hh_nonblack]] - .data[[cl_nonblack]]
    ) %>%
    tidyr::pivot_longer(-year, names_to = "race", values_to = "Difference") %>%
    dplyr::mutate(race = dplyr::recode(race,
      "black_diff"    = "Black HH-Clan diff",
      "nonblack_diff" = "Non-Black HH-Clan diff"
    ))

  pC <- ggplot2::ggplot(
    diff_dat,
    ggplot2::aes(x = year, y = Difference, color = race, linetype = race)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = c("Black HH-Clan diff"     = ORANGE,
                 "Non-Black HH-Clan diff" = BLUE)) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c("Black HH-Clan diff"     = "dashed",
                 "Non-Black HH-Clan diff" = "dashed")) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "HH \u2212 Clan Gini",
                  subtitle = "HH \u2212 Clan Difference") +
    .sens_base_t() + .sens_sub_t()

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

# ── Appendix D: C1/C2/C3 panel ───────────────────────────────────────────────

make_c123_panel <- function(dat, y_limits = NULL, show_unit,
                             ylab = "Inequality Coefficient") {
  coef_colors <- c(
    "C1 (Bonferroni)" = BLUE,
    "C2 (Gini)"       = ORANGE,
    "C3 (Upper Tail)" = "#33a02c"
  )
  br <- make_x_breaks(min(dat$year, na.rm = TRUE),
                       max(dat$year, na.rm = TRUE))

  if (show_unit == "Diff") {
    diff_dat <- dat %>%
      dplyr::transmute(
        year,
        C1_diff = C1_hh - C1_clan,
        C2_diff = C2_hh - C2_clan,
        C3_diff = C3_hh - C3_clan
      ) %>%
      tidyr::pivot_longer(-year, names_to = "coef", values_to = "value") %>%
      dplyr::mutate(coef = dplyr::recode(coef,
        "C1_diff" = "C1 (Bonferroni)",
        "C2_diff" = "C2 (Gini)",
        "C3_diff" = "C3 (Upper Tail)"
      ))

    if (is.null(y_limits)) {
      vals     <- diff_dat$value[is.finite(diff_dat$value)]
      y_limits <- c(floor(min(vals) * 20) / 20,
                    ceiling(max(vals) * 20) / 20 + 0.05)
    }
    return(
      ggplot2::ggplot(
        diff_dat,
        ggplot2::aes(x = year, y = value, color = coef, group = coef)
      ) +
        ggplot2::geom_line(linewidth = 1.7, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotted",
                            color = "grey60") +
        ggplot2::scale_color_manual(values = coef_colors, name = NULL) +
        ggplot2::scale_y_continuous(limits = y_limits) +
        ggplot2::scale_x_continuous(
          breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
        ggplot2::labs(x = NULL, y = "HH \u2212 Clan",
                      subtitle = "HH \u2212 Clan Difference") +
        .sens_base_t() + .sens_sub_t()
    )
  }

  long <- dat %>%
    dplyr::select(year, C1_hh, C1_clan, C2_hh, C2_clan, C3_hh, C3_clan) %>%
    tidyr::pivot_longer(-year, names_to = "series", values_to = "value") %>%
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
    vals     <- long$value[is.finite(long$value)]
    y_limits <- c(floor(min(vals) * 20) / 20,
                  ceiling(max(vals) * 20) / 20 + 0.05)
  }

  panel_subtitle <- if (show_unit == "Household") "Households" else "Clans"
  line_type      <- if (show_unit == "Clan") "dotted" else "solid"

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = year, y = value, color = Coefficient, group = Coefficient)
  ) +
    ggplot2::geom_line(linewidth = 1.7, linetype = line_type) +
    ggplot2::scale_color_manual(values = coef_colors, name = NULL) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = ylab, subtitle = panel_subtitle) +
    .sens_base_t() + .sens_sub_t()
}

# ── Appendix F style: ratio sensitivity 3-panel (main | alt | diff) ──────────

make_ratio_sensitivity_plot <- function(dat,
                                         main_hh, main_cl,
                                         alt_hh,  alt_cl,
                                         left_label  = "Main Results",
                                         right_label = "Alternative",
                                         y_limits    = NULL,
                                         diff_limits = NULL) {
  wide <- dat %>%
    dplyr::filter(is.finite(year)) %>%
    dplyr::mutate(
      main_diff = .data[[main_hh]] - .data[[main_cl]],
      alt_diff  = .data[[alt_hh]]  - .data[[alt_cl]]
    )

  if (is.null(y_limits)) {
    vals <- c(wide[[main_hh]], wide[[main_cl]], wide[[alt_hh]], wide[[alt_cl]])
    vals <- vals[is.finite(vals)]
    y_limits <- c(floor(min(vals) * 20) / 20,
                  ceiling(max(vals) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(wide$main_diff, wide$alt_diff)
    vals <- vals[is.finite(vals)]
    diff_limits <- c(floor(min(vals) * 20) / 20,
                     ceiling(max(vals) * 20) / 20 + 0.05)
  }

  br <- make_x_breaks(min(wide$year, na.rm = TRUE),
                       max(wide$year, na.rm = TRUE))

  pA <- ggplot2::ggplot(wide, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[main_hh]]),
                       color = ORANGE,      linewidth = 1.7, linetype = "solid") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[main_cl]]),
                       color = PALE_ORANGE, linewidth = 1.7, linetype = "dotted") +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "Black / Non-Black Ratio",
                  subtitle = left_label) +
    .sens_base_t() + .sens_sub_t()

  pB <- ggplot2::ggplot(wide, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[alt_hh]]),
                       color = BLUE,      linewidth = 1.7, linetype = "solid") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[alt_cl]]),
                       color = PALE_BLUE, linewidth = 1.7, linetype = "dotted") +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = right_label) +
    .sens_base_t() + .sens_sub_t()

  long_diff <- wide %>%
    dplyr::select(year, main_diff, alt_diff) %>%
    tidyr::pivot_longer(-year, names_to = "spec", values_to = "Difference") %>%
    dplyr::mutate(spec = dplyr::recode(spec,
      "main_diff" = "Main HH-Clan diff",
      "alt_diff"  = "Alt HH-Clan diff"
    ))

  pC <- ggplot2::ggplot(
    long_diff,
    ggplot2::aes(x = year, y = Difference, color = spec, linetype = spec)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = c("Main HH-Clan diff" = ORANGE, "Alt HH-Clan diff" = BLUE)) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c("Main HH-Clan diff" = "dashed", "Alt HH-Clan diff" = "dashed")) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = NULL, y = "HH \u2212 Clan Diff",
                  subtitle = "HH \u2212 Clan Difference") +
    .sens_base_t() + .sens_sub_t()

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

# Wrapper: title + Panel A + Panel B + single shared legend
make_ratio_sensitivity_figure <- function(plot_a, plot_b,
                                           title_str,
                                           sub_a = "Panel A: Median wealth ratio",
                                           sub_b = "Panel B: Mean wealth ratio",
                                           show_title = FALSE) {
  sub_a_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_a, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)
  sub_b_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_b, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)

  leg <- .sens_legend()

  if (show_title) {
    title_grob <- cowplot::ggdraw() +
      cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                          fontfamily = base_family, size = title_size)
    cowplot::plot_grid(
      title_grob, sub_a_grob, plot_a, sub_b_grob, plot_b, leg,
      ncol = 1, rel_heights = c(0.08, 0.05, 1, 0.05, 1, 0.12)
    )
  } else {
    cowplot::plot_grid(
      sub_a_grob, plot_a, sub_b_grob, plot_b, leg,
      ncol = 1, rel_heights = c(0.05, 1, 0.05, 1, 0.12)
    )
  }
}