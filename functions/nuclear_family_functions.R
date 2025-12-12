# Functions to go from raw data > Lorenz curve > Scaled conditional mean curve > C1, C2, C3

# Function to calculate the Lorenz curve
# L(u) = 1/u * ∫_0^u F^(-1) * (t) dt
lorenz <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x)) else stopifnot(length(x) == length(w))
  keep <- is.finite(x) & is.finite(w) & (w > 0)
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(tibble(p = c(0, 1), L = c(0, 1)))

  ord <- order(x); x <- x[ord]; w <- w[ord]
  sw  <- sum(w); sxw <- sum(x * w)
  if (sw <= 0 || sxw == 0) return(tibble(p = c(0, 1), L = c(0, 1)))

  p <- c(0, cumsum(w) / sw)
  L <- c(0, cumsum(x * w) / sxw)
  tibble(p = p, L = L)
}


# Function to calculate the scaled conditional mean curve
# M(u) = L(u) / u
scm_points <- function(p, L) {
  stopifnot(length(p) == length(L))

  ord <- order(p); p <- p[ord]; L <- L[ord] # Order data
  p_safe <- pmax(p, .Machine$double.eps) # Set integration limit
  M <- L / p_safe

  M[p == 1] <- 1  # Boundary condition M(1)=1
  tibble(u = p, M = M)
}


# Function to calculate first three  moments on the scaled conditional mean curve (Aaberge Eq. 8, pp. 311)
# Ck(F) = k * ∫_0^1 u^(k-1) * (1 - M(u)) du
scm_moments <- function(u, M, k) {
  stopifnot(length(u) == length(M), k >= 1)

  keep <- is.finite(u) & is.finite(M) # Keep finite values
  u <- u[keep]; M <- M[keep] 
  ord <- order(u); u <- u[ord]; M <- M[ord] # Order values in population

  u <- pmin(pmax(u, 0), 1) # Set integration limites
  u_safe <- pmax(u, .Machine$double.eps)

  integrand <- (u_safe^(k - 1)) * (1 - M)
  k * trapz(u, integrand)
}


# Helper function to calculate C123 (raw data > Lorenz > SCM > C1,C2,C3)
c123 <- function(x, w = NULL) {
  # Raw to Lorenz
  lz <- lorenz(x, w) 

  # Lorenz to SCM
  scm <- scm_points(lz$p, lz$L) 

  # SCM to C123
  tibble(
    C1 = scm_moments(scm$u, scm$M, 1),
    C2 = scm_moments(scm$u, scm$M, 2),
    C3 = scm_moments(scm$u, scm$M, 3)
  )
}


# Helper function to calculate moments by year
C123_by_year <- function(df, value_var, weight = TRUE, weight_var = NULL) {
  v <- as_string(ensym(value_var))

  if (weight && is.null(weight_var)) {
    stop("When weight = TRUE, provide weight_var.")
  }

  base <- df %>%
    filter(is.finite(.data[[v]])) %>%
    transmute(
      year  = year,
      value = .data[[v]],
      w     = if (weight) .data[[weight_var]] else NA_real_
    )

  base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())

      out <- c123(d$value, if (weight) d$w else NULL)

      out %>%
        mutate(year = unique(d$year), .before = 1)
    }) %>%
    arrange(year) %>%
    mutate(
      across(where(is.numeric), ~ round(as.numeric(.x), 3))   # <- NEW: round to 3 decimals
    )
}


# Trapezoidal integration helper
trapz <- function(x, y) sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
