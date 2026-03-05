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

# Figure 1 style: Gini over time (HH solid / Clan dotted) ──────────────────

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
    ggplot2::labs(x = "Year", y = ylab, linetype = NULL) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(linetype = ggplot2::guide_legend(
      override.aes = list(color = c(PALE_ORANGE, ORANGE), linewidth = 1.2)
    ))
}

# Figure 2 style: Lorenz curves ────────────────────────────────────────────

# Internal: build Lorenz (p, L) data for one unit type across requested years
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
    ggplot2::labs(x = "Cumulative Proportion of Units", y = ylab) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_blank()) +
    ggplot2::guides(
      color    = ggplot2::guide_legend(
        nrow = 2, override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 2)
    )
}

# ── Appendix D sensitivity style: 3-panel (main | alt | HH-Clan diff) ────────

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

  br           <- make_x_breaks(min(dat$year, na.rm = TRUE),
                                 max(dat$year, na.rm = TRUE))
  main_hh_col  <- ORANGE;      main_cl_col <- PALE_ORANGE
  alt_hh_col   <- BLUE;        alt_cl_col  <- PALE_BLUE

  base_t <- ggplot2::theme(
    legend.position = "none",
    plot.title      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1))
  sub_t <- ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = sub_size * 0.6, hjust = 0.5))

  long_main <- dat %>%
    dplyr::select(year, Household = main_hh_, Clan = main_cl_) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pA <- ggplot2::ggplot(
    long_main,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = main_hh_col, "Clan" = main_cl_col)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = "Gini Coefficient",
                  color = NULL, linetype = NULL, subtitle = left_label) +
    base_t + sub_t

  long_alt <- dat %>%
    dplyr::select(year, Household = alt_hh_, Clan = alt_cl_) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pB <- ggplot2::ggplot(
    long_alt,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = alt_hh_col, "Clan" = alt_cl_col)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = NULL,
                  color = NULL, linetype = NULL, subtitle = right_label) +
    base_t + sub_t

  long_diff <- dat %>%
    dplyr::select(year,
                  `Main (HH-Clan)` = main_diff,
                  `Alt (HH-Clan)`  = alt_diff) %>%
    tidyr::pivot_longer(-year, names_to = "Specification",
                        values_to = "Difference")

  pC <- ggplot2::ggplot(
    long_diff,
    ggplot2::aes(x = year, y = Difference,
                 color = Specification, linetype = Specification)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(
      values = c("Main (HH-Clan)" = main_hh_col,
                 "Alt (HH-Clan)"  = alt_hh_col)) +
    ggplot2::scale_linetype_manual(
      values = c("Main (HH-Clan)" = "solid", "Alt (HH-Clan)" = "solid")) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = "HH \u2212 Clan Gini",
                  color = NULL, linetype = NULL,
                  subtitle = "HH \u2212 Clan Difference") +
    base_t + sub_t

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

# Helper: build the shared HH/Clan legend grob
.hh_clan_legend <- function() {
  donor <- ggplot2::ggplot(
    data.frame(year = 1, value = 1, Unit = c("Household", "Clan")),
    ggplot2::aes(x = year, y = value, color = Unit, linetype = Unit)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(
      values = c("Household" = ORANGE, "Clan" = PALE_ORANGE), name = NULL) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted"), name = NULL) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(
      color    = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)),
      linetype = ggplot2::guide_legend(nrow = 1))
  cowplot::get_legend(donor)
}

# Wrapper: title grob + Panel A row + Panel B row + shared legend
make_sensitivity_figure <- function(plot_inc, plot_w, title_str,
                                     sub_a = "Panel A: Income",
                                     sub_b = "Panel B: Wealth") {
  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                        fontfamily = base_family, size = title_size)
  sub_a_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_a, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)
  sub_b_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_b, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)

  cowplot::plot_grid(
    title_grob, sub_a_grob, plot_inc, sub_b_grob, plot_w, .hh_clan_legend(),
    ncol = 1, rel_heights = c(0.08, 0.05, 1, 0.05, 1, 0.1)
  )
}

# Wealth-only figure: just title + the single plot (no income panel, no panel label)
make_single_sensitivity_figure <- function(plot_w, title_str) {
  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                        fontfamily = base_family, size = title_size)
  cowplot::plot_grid(title_grob, plot_w, .hh_clan_legend(),
                     ncol = 1, rel_heights = c(0.08, 1, 0.1))
}

# ── Appendix D6 style: race sensitivity (Black | Non-Black | diff) ───────────

make_race_sensitivity_plot <- function(dat,
                                        hh_black, cl_black,
                                        hh_nonblack, cl_nonblack,
                                        y_limits    = NULL,
                                        diff_limits = NULL,
                                        ylab        = "Gini Coefficient") {
  # Arguments are plain strings, e.g. "r_hh_w_inc_black"
  hh_b  <- hh_black
  cl_b  <- cl_black
  hh_nb <- hh_nonblack
  cl_nb <- cl_nonblack

  br             <- make_x_breaks(min(dat$year, na.rm = TRUE),
                                   max(dat$year, na.rm = TRUE))
  black_hh_col    <- ORANGE;      black_cl_col    <- PALE_ORANGE
  nonblack_hh_col <- BLUE;        nonblack_cl_col <- PALE_BLUE

  base_t <- ggplot2::theme(
    legend.position = "none",
    plot.title      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1))
  sub_t <- ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = sub_size * 0.6, hjust = 0.5))

  if (is.null(y_limits)) {
    vals <- c(dat[[hh_b]], dat[[cl_b]], dat[[hh_nb]], dat[[cl_nb]])
    y_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                  ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }
  if (is.null(diff_limits)) {
    vals <- c(dat[[hh_b]] - dat[[cl_b]], dat[[hh_nb]] - dat[[cl_nb]])
    diff_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                     ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }

  long_black <- dat %>%
    dplyr::transmute(year,
                     Household = .data[[hh_b]],
                     Clan      = .data[[cl_b]]) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pA <- ggplot2::ggplot(
    long_black,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = black_hh_col, "Clan" = black_cl_col)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = ylab, subtitle = "Black",
                  color = NULL, linetype = NULL) +
    base_t + sub_t

  long_nonblack <- dat %>%
    dplyr::transmute(year,
                     Household = .data[[hh_nb]],
                     Clan      = .data[[cl_nb]]) %>%
    tidyr::pivot_longer(-year, names_to = "Unit", values_to = "Gini")

  pB <- ggplot2::ggplot(
    long_nonblack,
    ggplot2::aes(x = year, y = Gini, color = Unit, linetype = Unit)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(
      values = c("Household" = nonblack_hh_col, "Clan" = nonblack_cl_col)) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = NULL, subtitle = "Non-Black",
                  color = NULL, linetype = NULL) +
    base_t + sub_t

  diff_dat <- dat %>%
    dplyr::transmute(
      year,
      Black       = .data[[hh_b]]  - .data[[cl_b]],
      `Non-Black` = .data[[hh_nb]] - .data[[cl_nb]]
    ) %>%
    tidyr::pivot_longer(-year, names_to = "Race", values_to = "Difference")

  pC <- ggplot2::ggplot(
    diff_dat,
    ggplot2::aes(x = year, y = Difference, color = Race, group = Race)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(
      values = c("Black" = black_hh_col, "Non-Black" = nonblack_hh_col),
      name = NULL) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = "HH \u2212 Clan Gini",
                  subtitle = "HH \u2212 Clan Difference", color = NULL) +
    base_t + sub_t

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

# ── Figure 4 style: race ratio row (HH | Clan | HH-Clan diff) ────────────────

# ── Figure 3 style: race ratio plot (Figure 1 style, HH + Clan on one panel) ──
# dat      : wide ratio tibble with columns year, r_hh_w_ratio, r_cl_w_ratio, etc.
# ylab     : y-axis label
# y_limits : shared limits (pass to keep panels comparable across income/wealth)
# hline    : reference line (default 1 = parity)

make_ratio_plot <- function(dat, ylab, y_limits = NULL, hline = 1) {
  d <- dat %>%
    dplyr::filter(is.finite(year),
                  is.finite(r_hh_w_ratio),
                  is.finite(r_cl_w_ratio))

  br <- make_x_breaks(min(d$year, na.rm = TRUE), max(d$year, na.rm = TRUE))

  if (is.null(y_limits)) {
    vals     <- c(d$r_hh_w_ratio, d$r_cl_w_ratio)
    y_limits <- c(floor(min(vals, na.rm = TRUE) * 20) / 20,
                  ceiling(max(vals, na.rm = TRUE) * 20) / 20 + 0.05)
  }

  ggplot2::ggplot(d, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = hline, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = r_hh_w_ratio, linetype = "Household"),
                       color = ORANGE,      linewidth = 1.7) +
    ggplot2::geom_line(ggplot2::aes(y = r_cl_w_ratio, linetype = "Clan"),
                       color = PALE_ORANGE, linewidth = 1.7) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_linetype_manual(
      values = c("Household" = "solid", "Clan" = "dotted")) +
    ggplot2::labs(x = "Year", y = ylab, linetype = NULL) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title      = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(linetype = ggplot2::guide_legend(
      override.aes = list(color = c(PALE_ORANGE, ORANGE), linewidth = 1.2)
    ))
}

# Ratio sensitivity style: 3-panel (main | alt | difference) ───────────────

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

  br           <- make_x_breaks(min(wide$year, na.rm = TRUE),
                                 max(wide$year, na.rm = TRUE))
  main_hh_col  <- ORANGE;      main_cl_col <- PALE_ORANGE
  alt_hh_col   <- BLUE;        alt_cl_col  <- PALE_BLUE

  base_t <- ggplot2::theme(
    legend.position = "none",
    plot.title      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1))
  sub_t <- ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = sub_size * 0.6, hjust = 0.5))

  pA <- ggplot2::ggplot(wide, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[main_hh]]),
                       color = main_hh_col, linewidth = 1.7) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[main_cl]]),
                       color = main_cl_col, linewidth = 1.7, linetype = "dotted") +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = "Black / Non-Black Ratio",
                  subtitle = left_label) +
    base_t + sub_t

  pB <- ggplot2::ggplot(wide, ggplot2::aes(x = year)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(y = .data[[alt_hh]]),
                       color = alt_hh_col, linewidth = 1.7) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[alt_cl]]),
                       color = alt_cl_col, linewidth = 1.7, linetype = "dotted") +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = NULL, subtitle = right_label) +
    base_t + sub_t

  long_diff <- wide %>%
    dplyr::select(year, `Main (HH-Clan)` = main_diff, `Alt (HH-Clan)` = alt_diff) %>%
    tidyr::pivot_longer(-year, names_to = "Specification", values_to = "Difference")

  pC <- ggplot2::ggplot(
    long_diff,
    ggplot2::aes(x = year, y = Difference,
                 color = Specification, linetype = Specification)) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
    ggplot2::scale_color_manual(
      values = c("Main (HH-Clan)" = main_hh_col, "Alt (HH-Clan)" = alt_hh_col)) +
    ggplot2::scale_linetype_manual(
      values = c("Main (HH-Clan)" = "solid", "Alt (HH-Clan)" = "solid")) +
    ggplot2::scale_y_continuous(limits = diff_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = "HH - Clan Diff",
                  subtitle = "HH - Clan Difference", color = NULL, linetype = NULL) +
    base_t + sub_t

  cowplot::plot_grid(pA, pB, pC, nrow = 1)
}

# Wrapper: title + Panel A (income) + Panel B (wealth) for ratio sensitivity
make_ratio_sensitivity_figure <- function(plot_inc, plot_w, title_str,
                                           sub_a = "Panel A: Income",
                                           sub_b = "Panel B: Wealth") {
  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(title_str, x = 0, hjust = 0, fontface = "bold",
                        fontfamily = base_family, size = title_size)
  sub_a_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_a, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)
  sub_b_grob <- cowplot::ggdraw() +
    cowplot::draw_label(sub_b, x = 0, hjust = 0,
                        fontfamily = base_family, size = sub_size)
  cowplot::plot_grid(
    title_grob, sub_a_grob, plot_inc, sub_b_grob, plot_w, .hh_clan_legend(),
    ncol = 1, rel_heights = c(0.08, 0.05, 1, 0.05, 1, 0.1)
  )
}

# Appendix C: C1/C2/C3 panel ───────────────────────────────────────────────

make_c123_panel <- function(dat, y_limits = NULL, show_unit,
                             ylab = "Inequality Coefficient") {
  coef_colors <- c(
    "C1 (Bonferroni)" = BLUE,
    "C2 (Gini)"       = ORANGE,
    "C3 (Upper Tail)" = "#33a02c"
  )
  br     <- make_x_breaks(min(dat$year, na.rm = TRUE),
                           max(dat$year, na.rm = TRUE))
  base_t <- ggplot2::theme(
    legend.position = "none",
    plot.title      = ggplot2::element_blank(),
    axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1))

  if (show_unit == "Diff") {
    diff_dat <- dat %>%
      dplyr::transmute(
        year,
        `C1 (Bonferroni)` = C1_hh - C1_clan,
        `C2 (Gini)`       = C2_hh - C2_clan,
        `C3 (Upper Tail)` = C3_hh - C3_clan
      ) %>%
      tidyr::pivot_longer(-year, names_to = "Coefficient", values_to = "value")

    if (is.null(y_limits)) {
      vals     <- diff_dat$value[is.finite(diff_dat$value)]
      y_limits <- c(floor(min(vals) * 20) / 20,
                    ceiling(max(vals) * 20) / 20 + 0.05)
    }
    return(
      ggplot2::ggplot(
        diff_dat,
        ggplot2::aes(x = year, y = value,
                     color = Coefficient, group = Coefficient)
      ) +
        ggplot2::geom_line(linewidth = 1.7) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotted",
                            color = "grey60") +
        ggplot2::scale_color_manual(values = coef_colors) +
        ggplot2::scale_y_continuous(limits = y_limits) +
        ggplot2::scale_x_continuous(
          breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
        ggplot2::labs(x = "Year", y = "HH \u2212 Clan", color = NULL) +
        base_t
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

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = year, y = value,
                 color = Coefficient, group = Coefficient)
  ) +
    ggplot2::geom_line(linewidth = 1.7) +
    ggplot2::scale_color_manual(values = coef_colors) +
    ggplot2::scale_y_continuous(limits = y_limits) +
    ggplot2::scale_x_continuous(
      breaks = br, expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Year", y = ylab, color = NULL) +
    base_t
}
