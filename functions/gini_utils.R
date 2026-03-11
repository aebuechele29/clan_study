# gini_utils.R
# Core Gini estimation functions called by 6_calculate_gini and 7_gini_by_race.


# run_gini
# Compute a weighted or unweighted Gini coefficient by year for one spec.
#
# Args:
#   df         : data frame containing the data
#   value_var  : character — column name of the variable to measure
#   weight_var : character or NULL — column name of survey weight
#   clan       : logical — TRUE triggers convey survey design (clan data)
#   weighted   : logical — if FALSE, ignores weight_var entirely
#   col_label  : character — name for the output Gini column

run_gini <- function(df, value_var, weight_var = NULL,
                     clan = FALSE, weighted = TRUE, col_label) {
  years <- sort(unique(df$year))

  purrr::map_dfr(years, function(yr) {
    d <- df %>%
      dplyr::filter(year == yr, is.finite(.data[[value_var]]))

    gini_val <- tryCatch({
      if (!weighted || is.null(weight_var)) {
        ineq::Gini(d[[value_var]])
      } else {
        # "adjust" sets the variance contribution of singleton-PSU strata to
        # zero (treats them as certainty-selected), which is the standard
        # approach for PSID post-1999 where the refresher sample introduces
        # strata with only one PSU.
        old_opt <- getOption("survey.lonely.psu")
        options(survey.lonely.psu = "adjust")
        on.exit(options(survey.lonely.psu = old_opt), add = TRUE)

        des <- survey::svydesign(
          ids     = ~cluster,
          strata  = ~stratum,
          weights = stats::as.formula(paste0("~", weight_var)),
          data    = d,
          nest    = TRUE
        )
        des <- convey::convey_prep(des)
        as.numeric(convey::svygini(
          stats::as.formula(paste0("~", value_var)), des, na.rm = TRUE
        ))
      }
    }, error = function(e) NA_real_)

    tibble::tibble(year = yr, !!col_label := round(gini_val, 3))
  })
}


# run_gini_race
# Compute Gini for Black and Non-Black subgroups by year, with standard errors.
#
# Args:
#   black_df    : data frame pre-filtered to black == 1
#   nonblack_df : data frame pre-filtered to black == 0
#   value_var, weight_var, clan, weighted : same as run_gini
#   col_label   : character — prefix for output columns

run_gini_race <- function(black_df, nonblack_df,
                          value_var, weight_var = NULL,
                          clan = FALSE, weighted = TRUE, col_label) {

  run_one <- function(df, suffix) {
    years <- sort(unique(df$year))
    purrr::map_dfr(years, function(yr) {
      d <- df %>%
        dplyr::filter(year == yr, is.finite(.data[[value_var]]))
      if (nrow(d) < 5) return(tibble::tibble(year = yr))

      tryCatch({
        if (!weighted || is.null(weight_var)) {
          g <- ineq::Gini(d[[value_var]])
          tibble::tibble(
            year = yr,
            !!paste0(col_label, suffix)        := round(g, 3),
            !!paste0(col_label, suffix, "_se") := NA_real_
          )
        } else {
          old_opt <- getOption("survey.lonely.psu")
          options(survey.lonely.psu = "adjust")
          on.exit(options(survey.lonely.psu = old_opt), add = TRUE)

          des <- survey::svydesign(
            ids     = ~cluster,
            strata  = ~stratum,
            weights = stats::as.formula(paste0("~", weight_var)),
            data    = d,
            nest    = TRUE
          )
          des <- convey::convey_prep(des)
          res <- convey::svygini(
            stats::as.formula(paste0("~", value_var)), des, na.rm = TRUE
          )
          tibble::tibble(
            year = yr,
            !!paste0(col_label, suffix)        := round(as.numeric(res), 3),
            !!paste0(col_label, suffix, "_se") := round(
              as.numeric(sqrt(attr(res, "var"))), 4
            )
          )
        }
      }, error = function(e) tibble::tibble(year = yr))
    })
  }

  dplyr::full_join(
    run_one(black_df,    "_black"),
    run_one(nonblack_df, "_nonblack"),
    by = "year"
  ) %>%
    dplyr::arrange(year)
}


# append_mean_row
# Append an "ALL" row equal to the column-wise mean across years.
append_mean_row <- function(df) {
  all_row <- df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric),
                    ~ round(mean(.x, na.rm = TRUE), 3))
    ) %>%
    dplyr::mutate(year = "ALL", .before = 1)

  dplyr::bind_rows(
    df %>% dplyr::mutate(year = as.character(year)),
    all_row
  )
}


# C123_by_year
# Compute Bonferroni (C1), Gini (C2), and upper-tail (C3) inequality coefficients by year.  
# Used in 8_nuclear_family.
# C1 = 1 - 2 * integral of L(p) dp            (weight on lower tail)
# C2 = standard Gini
# C3 = 2 * E[(1-F(x)) * x] / mu - 1           (weight on upper tail)

C123_by_year <- function(df, value_var, weight = TRUE, weight_var = NULL) {
  value_var_str <- deparse(substitute(value_var))
  years <- sort(unique(df$year))

  purrr::map_dfr(years, function(yr) {
    d <- df %>%
      dplyr::filter(year == yr, is.finite(.data[[value_var_str]]))

    tryCatch({
      if (!weight || is.null(weight_var)) {
        x <- sort(d[[value_var_str]])
        n <- length(x)
        w <- rep(1 / n, n)
      } else {
        d   <- d %>%
          dplyr::filter(
            is.finite(.data[[weight_var]]),
            .data[[weight_var]] > 0
          )
        x   <- d[[value_var_str]]
        raw <- d[[weight_var]]
        ord <- order(x); x <- x[ord]; raw <- raw[ord]
        w   <- raw / sum(raw)
      }

      mu  <- sum(x * w)
      cdf <- cumsum(w)
      L   <- cumsum(x * w) / mu

      # C1: Bonferroni — double integral weighting towards the bottom
      C1 <- 1 - 2 * sum(w * L / cdf)

      # C2: standard Gini via trapezoid approximation of Lorenz area
      C2 <- 1 - sum(w * (c(0, head(L, -1)) + L))

      # C3: upper-tail weight
      C3 <- 2 * sum(w * (1 - cdf) * x) / mu - 1

      tibble::tibble(year = yr,
                     C1 = round(C1, 3),
                     C2 = round(C2, 3),
                     C3 = round(C3, 3))
    }, error = function(e) {
      tibble::tibble(year = yr, C1 = NA_real_, C2 = NA_real_, C3 = NA_real_)
    })
  })
}
