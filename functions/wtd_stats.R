# Functions defined in this file:
#   wtd_mean
#   wtd_median
#   lorenz_tbl
#   wtd_sd
#   efficient_max
#   efficient_sum


# Weighted mean
# Used in: figure_main, figure_appendix
wtd_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  sum(x[keep] * w[keep]) / sum(w[keep])
}

# Weighted median
# Used in: gini_by_race, figure_main, figure_appendix
wtd_median <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  x[which(cumsum(w) / sum(w) >= 0.5)[1]]
}

# Lorenz curve as a tibble(p, L) — used by make_lorenz_plot
# Used in: plot_helpers (inside .lorenz_data)
lorenz_tbl <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  tibble::tibble(
    p = c(0, cumsum(w) / sum(w)),
    L = c(0, cumsum(x * w) / sum(x * w))
  )
}

# Weighted standard deviation
# Used in: figure_main
wtd_sd <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  mu <- sum(x * w) / sum(w)
  sqrt(sum(w * (x - mu)^2) / sum(w))
}

# Vectorised helpers used inside data pipeline scripts (2_clean_panel,
# 3_households) — kept here so they are always available after source().
# Used in: clean_panel
efficient_max <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(-Inf)
  max(x)
}

# Used in: households
efficient_sum <- function(x, na.rm = TRUE) {
  sum(x, na.rm = na.rm)
}

