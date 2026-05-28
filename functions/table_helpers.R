# table_helpers.R
# Formatting builders for tables (not plots).

# Number formatting ─────────────────────────────────────────────────────────

fmt_se     <- function(x, se) sprintf("%.3f\n(SE = %.3f)", x, se)
fmt_money0 <- function(x) format(round(x, 0), big.mark = ",")
fmt_int    <- function(x) format(round(as.numeric(x), 0), big.mark = ",")

# Format a monetary value for a table cell: negative values in parentheses,
# NA as an em-dash.
fmt_money_tbl <- function(x) {
  ifelse(
    is.na(x), "\u2014",
    ifelse(
      x < 0,
      paste0("(", format(round(abs(x), 0), big.mark = ",", scientific = FALSE), ")"),
      format(round(x, 0), big.mark = ",", scientific = FALSE)
    )
  )
}

# Figure 3 (appendix): distributional decile table ─────────────────────────

# Compute D1-D9 and N for one data frame / variable / weight combination.
dist_stats <- function(df, value_var, weight_var = NULL) {
  x <- df[[value_var]]
  w <- if (!is.null(weight_var) && weight_var %in% names(df))
    df[[weight_var]]
  else
    rep(1, nrow(df))
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x)
  x_s <- x[ord]
  cdf <- cumsum(w[ord]) / sum(w)
  qs  <- purrr::map_dbl(seq(0.1, 0.9, by = 0.1), ~ x_s[which(cdf >= .x)[1]])
  c(setNames(qs, paste0("D", 1:9)), N = length(x))
}

# Formatted character column (D1-D9 + N)
build_dist_col <- function(df, value_var, weight_var) {
  s <- dist_stats(df, value_var, weight_var)
  c(fmt_money_tbl(s[paste0("D", 1:9)]),
    format(as.integer(s["N"]), big.mark = ","))
}

dist_row_labels <- c(
  "D1 (10th pct)", "D2 (20th pct)", "D3 (30th pct)",
  "D4 (40th pct)", "D5 - Median",   "D6 (60th pct)",
  "D7 (70th pct)", "D8 (80th pct)", "D9 (90th pct)",
  "N (observations)"
)

# Assemble the six-column distributional tibble.
make_dist_panel <- function(hh_all, hh_blk, hh_nb,
                             cl_all, cl_blk, cl_nb,
                             value_var, wt_hh, wt_cl) {
  tibble::tibble(
    Statistic  = dist_row_labels,
    HH1        = build_dist_col(hh_all, value_var, wt_hh),
    Clan1      = build_dist_col(cl_all, value_var, wt_cl),
    HH2        = build_dist_col(hh_blk, value_var, wt_hh),
    Clan2      = build_dist_col(cl_blk, value_var, wt_cl),
    HH3        = build_dist_col(hh_nb,  value_var, wt_hh),
    Clan3      = build_dist_col(cl_nb,  value_var, wt_cl)
  )
}

# Apply flextable styling
style_dist_ft <- function(ft) {
  median_row <- 5L
  n_row      <- 10L

  ft %>%
    flextable::set_header_labels(
      Statistic = "Statistic",
      HH1 = "HH", Clan1 = "Kin Group",
      HH2 = "HH", Clan2 = "Kin Group",
      HH3 = "HH", Clan3 = "Kin Group"
    ) %>%
    flextable::add_header_row(
      values    = c("", "All", "Black", "Non-Black"),
      colwidths = c(1L, 2L, 2L, 2L)
    ) %>%
    flextable::font(fontname = base_family, part = "all") %>%
    flextable::fontsize(size = 9, part = "body") %>%
    flextable::fontsize(size = 9, part = "header") %>%
    flextable::align(align = "left",   j = 1,   part = "all") %>%
    flextable::align(align = "right",  j = 2:7, part = "all") %>%
    flextable::align(align = "center",           part = "header") %>%
    flextable::width(j = 1,   width = 1.6) %>%
    flextable::width(j = 2:7, width = 1.1) %>%
    flextable::bg(bg = GREY_HDR, part = "header") %>%
    flextable::bg(i = 2, j = 2:3, bg = PALE_ORANGE, part = "header") %>%
    flextable::bg(i = 2, j = 4:5, bg = PALE_BLUE,   part = "header") %>%
    flextable::bg(i = 2, j = 6:7, bg = PALE_ORANGE, part = "header") %>%
    flextable::bold(i   = median_row, part = "body") %>%
    flextable::bg(i     = median_row, bg = "#FFF3E0", part = "body") %>%
    flextable::italic(i = n_row,      part = "body") %>%
    flextable::bg(i     = n_row,      bg = GREY_HDR,  part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(
      border = officer::fp_border(color = "black",  width = 1.5),
      part = "header") %>%
    flextable::hline(
      i = 1,
      border = officer::fp_border(color = "black",  width = 0.5),
      part = "header") %>%
    flextable::hline(
      i = 2,
      border = officer::fp_border(color = "black",  width = 0.5),
      part = "header") %>%
    flextable::hline_bottom(
      border = officer::fp_border(color = "black",  width = 1.5),
      part = "header") %>%
    flextable::hline(
      i = median_row - 1L,
      border = officer::fp_border(color = "grey70", width = 0.4),
      part = "body") %>%
    flextable::hline(
      i = median_row,
      border = officer::fp_border(color = "grey70", width = 0.4),
      part = "body") %>%
    flextable::hline(
      i = n_row - 1L,
      border = officer::fp_border(color = "black",  width = 0.8),
      part = "body") %>%
    flextable::hline_bottom(
      border = officer::fp_border(color = "black",  width = 1.5),
      part = "body") %>%
    flextable::bold(part = "header") %>%
    flextable::set_table_properties(layout = "fixed")
}

# Race ratio table builder (7_gini_by_race) ─────────────────────────────────

make_ratio_tbl <- function(hh_all, cl_all, r_hh_all, r_cl_all,
                            value_var,
                            hh_race_var = "black_head",
                            cl_race_var = "black_clan",
                            weight_hh   = "fam_weight",
                            weight_cl   = "clan_weight") {
  specs <- list(
    list(label = "hh_u",   df = hh_all,   wt = NULL,      race = hh_race_var),
    list(label = "hh_w",   df = hh_all,   wt = weight_hh, race = hh_race_var),
    list(label = "r_hh_u", df = r_hh_all, wt = NULL,      race = hh_race_var),
    list(label = "r_hh_w", df = r_hh_all, wt = weight_hh, race = hh_race_var),
    list(label = "cl_u",   df = cl_all,   wt = NULL,      race = cl_race_var),
    list(label = "cl_w",   df = cl_all,   wt = weight_cl, race = cl_race_var),
    list(label = "r_cl_u", df = r_cl_all, wt = NULL,      race = cl_race_var),
    list(label = "r_cl_w", df = r_cl_all, wt = weight_cl, race = cl_race_var)
  )

  purrr::map_dfr(specs, function(s) {
    black_mean <- wtd_mean_by_year(
      s$df %>% dplyr::filter(.data[[s$race]] == 1), value_var, s$wt)
    nonblack_mean <- wtd_mean_by_year(
      s$df %>% dplyr::filter(.data[[s$race]] == 0), value_var, s$wt)

    dplyr::full_join(black_mean, nonblack_mean,
                     by = "year", suffix = c("_black", "_nonblack")) %>%
      dplyr::mutate(
        ratio    = mean_val_black / mean_val_nonblack,
        spec     = s$label,
        unit     = ifelse(grepl("cl",  s$label), "Kin Group",   "Household"),
        weighted = ifelse(grepl("_w",  s$label), "Weighted",    "Unweighted"),
        robust   = ifelse(grepl("^r_", s$label), "Robust",      "All")
      )
  })
}

