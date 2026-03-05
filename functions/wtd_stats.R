# wtd_stats.R
# Weighted summary statistics used across scripts and figures

# Weighted mean
wtd_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  sum(x[keep] * w[keep]) / sum(w[keep])
}

# Weighted median
wtd_median <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  x[which(cumsum(w) / sum(w) >= 0.5)[1]]
}

# Weighted quantiles for a vector of probabilities
wtd_quantile <- function(x, w, probs) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cdf <- cumsum(w) / sum(w)
  purrr::map_dbl(probs, ~ x[which(cdf >= .x)[1]])
}

# Lorenz curve as a tibble(p, L) — used by make_lorenz_plot
lorenz_tbl <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  tibble::tibble(
    p = c(0, cumsum(w) / sum(w)),
    L = c(0, cumsum(x * w) / sum(x * w))
  )
}

# Weighted mean by year — used in race ratio calculations (7_gini_by_race)
wtd_mean_by_year <- function(df, value_var, weight_var = NULL) {
  df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      mean_val = if (is.null(weight_var)) {
        mean(.data[[value_var]], na.rm = TRUE)
      } else {
        keep <- is.finite(.data[[value_var]]) &
                is.finite(.data[[weight_var]]) &
                .data[[weight_var]] > 0
        sum(.data[[value_var]][keep] * .data[[weight_var]][keep]) /
          sum(.data[[weight_var]][keep])
      },
      .groups = "drop"
    )
}

# Vectorised helpers used inside data pipeline scripts (2_clean_panel,
# 3_households) — kept here so they are always available after source().
efficient_max <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(-Inf)
  max(x)
}

efficient_sum <- function(x, na.rm = TRUE) {
  sum(x, na.rm = na.rm)
}
