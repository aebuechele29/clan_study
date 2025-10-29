# Function to calculate Lorenz curve points (weighted or unweighted)
lorenz_tbl <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x)) else stopifnot(length(x) == length(w))
  keep <- is.finite(x) & is.finite(w) & (w > 0)
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(tibble(p = c(0,1), L = c(0,1)))

  ord <- order(x); x <- x[ord]; w <- w[ord]
  sw  <- sum(w); sxw <- sum(x * w)
  if (sw <= 0 || sxw == 0) return(tibble(p = c(0,1), L = c(0,1)))

  p <- c(0, cumsum(w) / sw)
  L <- c(0, cumsum(x * w) / sxw)
  tibble(p = p, L = L)
}


# Function for trapezoidal integration (based on the Lorenz curve)
trapz <- function(x, y) sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)


# Function to calculate Gini's nuclear family (Aaberge)
C123 <- function(p, L) {
  C2 <- 1 - 2 * trapz(p, L) # Standard Gini = 1 - 2 * ∫ L(u) du
  C3 <- 2 * trapz(p, (1 - p) * L) # Tied to the top of the income distribution
  
  denom <- pmax(p, .Machine$double.eps)
  M <- L / denom
  C1 <- trapz(p, M) # Bonferroni coefficient, tied to the bottom of the income distribution

  tibble(C1 = C1, C2 = C2, C3 = C3)
}


# Function to calculate C1, C2, C3 by year
C123_by_year <- function(df, value_var, weight = TRUE, weight_var = NULL) {
  v <- as_string(ensym(value_var))

  if (weight && is.null(weight_var)) {
    stop("When weight = TRUE, provide weight_var.")
  }

  base <- df %>%
    filter(is.finite(.data[[v]])) %>%
    transmute(year,
              value = .data[[v]],
              w     = if (weight) .data[[weight_var]] else NA_real_)

  base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      Lz <- lorenz_tbl(d$value, if (weight) d$w else NULL)
      C123(Lz$p, Lz$L) %>%
        mutate(year = unique(d$year), .before = 1)
    }) %>%
    arrange(year)
}



# Function to compare C1, C2, and C3 calculations by distribution: all, excluding top X%, excluding bottom X%
C123_by_dist <- function(df, var, weight = TRUE, weight_var = NULL, cutoff = 0.10) {
  v <- as_string(ensym(var))
  pct_label <- gsub("\\.", "_", as.character(cutoff * 100))

  all <- C123_by_year(df, !!sym(v), weight = weight, weight_var = weight_var) %>%
    rename_with(~ paste0(.x, "_all"), c("C1", "C2", "C3"))

  top <- C123_by_year(
    exclude_top(df, !!sym(v), 1 - cutoff),
    !!sym(v), weight = weight, weight_var = weight_var
  ) %>%
    rename_with(~ paste0(.x, "_ex_top_", pct_label), c("C1", "C2", "C3"))

  bottom <- C123_by_year(
    exclude_bottom(df, !!sym(v), cutoff),
    !!sym(v), weight = weight, weight_var = weight_var
  ) %>%
    rename_with(~ paste0(.x, "_ex_bottom_", pct_label), c("C1", "C2", "C3"))

  out <- list(all, top, bottom) %>%
    reduce(full_join, by = "year") %>%
    arrange(year)

  mean_row <- out %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(year = "ALL", .before = 1)
    
  out %>%
    mutate(year = as.character(year)) %>%
    bind_rows(mean_row)
}
