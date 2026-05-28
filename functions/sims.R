# sims.R
# Functions for Appendix A to simulate potential results.

run_many_sims_sums <- function(n_sims = 100, ..., debug_every = NULL) {
  sims <- vector("list", n_sims)
  for (i in seq_len(n_sims)) {
    sims[[i]] <- simulate_ginis_sums(..., debug = !is.null(debug_every) && (i %% debug_every == 0))
    if (nrow(sims[[i]]$data) == 0L) stop(sprintf("Sim %d produced empty data (unexpected).", i))
  }
  sims
}

results_from_sims_sums <- function(sims) {
  g_hh <- vapply(sims, function(x) gini_safe_simple(x$data$value), numeric(1))
  g_cs <- vapply(sims, function(x) {
    cs <- x$data |>
      dplyr::group_by(clan) |>
      dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(is.finite(s), !is.na(s)) |>
      dplyr::pull(s)
    if (length(cs) == 0L) return(NA_real_)
    gini_safe_simple(cs)
  }, numeric(1))

  tibble::tibble(
    gini_households = g_hh,
    gini_clan_sums  = g_cs
  ) |>
    dplyr::mutate(diff_sums = gini_households - gini_clan_sums)
}

extreme_indices_sums <- function(df_res) {
  oks <- which(is.finite(df_res$diff_sums))
  if (length(oks) == 0L) stop("No finite values to find extremes.")
  list(
    idx_min_sums = oks[which.min(df_res$diff_sums[oks])],
    idx_max_sums = oks[which.max(df_res$diff_sums[oks])]
  )
}

plot_lorenz_for_sim_sums <- function(sim) {
  df <- sim$data

  x_house <- df$value
  x_clan <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(val = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(is.finite(val)) |>
    dplyr::pull(val)

  ld <- dplyr::bind_rows(
    lorenz_df_safe(x_house, "Households"),
    lorenz_df_safe(x_clan,  "Kin Groups")
  )

  ggplot2::ggplot(ld, ggplot2::aes(x = p, y = L, linetype = group)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linewidth = 0.4, linetype = "dashed") +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = "Cumulative share of units",
      y = "Cumulative share of income/wealth",
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank()
    )
}

mekko_df_sums <- function(sim) {
  df <- sim$data

  cs <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(
      n_hh    = dplyr::n(),
      sum_val = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(sum_val) |>
    dplyr::mutate(
      w_share = n_hh / sum(n_hh),
      x_max   = cumsum(w_share),
      x_min   = x_max - w_share,
      y_min   = 0,
      y_max   = sum_val
    )

  g_house <- gini_safe_simple(df$value)
  g_clan  <- gini_safe_simple(cs$sum_val)

  list(df = cs, diff = g_house - g_clan)
}

make_two_mekko_sums <- function(sims, idx_min_sums, idx_max_sums) {
  cases <- list(
    list(idx = idx_min_sums, facet = "Lowest simulated difference"),
    list(idx = idx_max_sums, facet = "Highest simulated difference")
  )

  long_df <- purrr::map_dfr(cases, function(case) {
    out <- mekko_df_sums(sims[[case$idx]])
    out$df |>
      dplyr::mutate(
        facet = case$facet,
        diff  = out$diff
      )
  })

  label_df <- long_df |>
    dplyr::group_by(facet) |>
    dplyr::summarise(
      x = 0.01,
      y = max(y_max) * 1.02,
      lab = sprintf("Household Gini - Kin Group Gini = %.3f", dplyr::first(diff)),
      .groups = "drop"
    )

  ggplot2::ggplot(long_df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
                       color = "grey30", fill = "grey70", alpha = 0.85) +
    ggplot2::facet_wrap(~ facet, scales = "free_y") +
    ggplot2::geom_text(data = label_df, ggplot2::aes(x = x, y = y, label = lab),
                       hjust = 0, vjust = 0, size = 3.6) +
    ggplot2::labs(
      x = "Cumulative share of households (by kin group width)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 12, r = 8, b = 8, l = 8))
}

simulate_ginis_sums <- function(
  n_clans = 10,
  hh_per_clan = 20,
  dist = "lognormal",
  dist_params = list(meanlog = 0, sdlog = 1),
  clan_size_scenario = "equal",
  debug = FALSE
) {
  if (!is.numeric(n_clans) || length(n_clans) != 1L || n_clans < 1) stop("n_clans must be a positive scalar.")
  if (!is.numeric(hh_per_clan) || length(hh_per_clan) != 1L || hh_per_clan < 1) stop("hh_per_clan must be a positive scalar.")

  if (clan_size_scenario == "equal") {
    clan_sizes <- rep(hh_per_clan, n_clans)
  } else if (clan_size_scenario == "random") {
    clan_sizes <- pmax(1L, rpois(n_clans, lambda = hh_per_clan))
  } else if (clan_size_scenario == "rich_big") {
    clan_sizes <- round(seq(1, hh_per_clan * 2, length.out = n_clans))
  } else if (clan_size_scenario == "rich_small") {
    clan_sizes <- round(seq(hh_per_clan * 2, 1, length.out = n_clans))
  } else {
    stop("Unknown clan_size_scenario")
  }

  clan_sizes <- as.integer(clan_sizes)
  clan_sizes[!is.finite(clan_sizes) | clan_sizes < 1] <- 1L
  if (length(clan_sizes) != n_clans) stop(sprintf("clan_sizes length (%d) != n_clans (%d)", length(clan_sizes), n_clans))

  n_households <- sum(clan_sizes)
  if (!is.finite(n_households) || n_households < 1) {
    if (debug) print(list(n_clans=n_clans, hh_per_clan=hh_per_clan, clan_size_scenario=clan_size_scenario, clan_sizes=clan_sizes))
    stop("Computed n_households < 1 — clan_sizes is invalid.")
  }

  values <- generate_values_simple(n_households, dist, dist_params)
  if (length(values) != n_households) stop("Generator returned wrong length.")

  df <- tibble::tibble(
    clan  = rep(seq_len(n_clans), times = clan_sizes),
    value = values
  )

  list(
    gini_households = gini_safe_simple(df$value),
    gini_clan_sums  = gini_safe_simple(df |>
                                        dplyr::group_by(clan) |>
                                        dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") |>
                                        dplyr::pull(s)),
    data = df
  )
}

generate_values_simple <- function(n, dist = "lognormal", dist_params = list(meanlog = 0, sdlog = 1)) {
  if (dist == "lognormal") {
    return(rlnorm(n, meanlog = dist_params$meanlog %||% 0, sdlog = dist_params$sdlog %||% 1))
  }
  if (dist == "normal") {
    return(rnorm(n, mean = dist_params$mean %||% 0, sd = dist_params$sd %||% 1))
  }
  if (dist == "two_sided_lognormal") {
    assets <- rlnorm(n, meanlog = dist_params$meanlog_pos %||% 0, sdlog = dist_params$sdlog_pos %||% 1)
    p_debt <- dist_params$p_debt %||% 0.3
    has_debt <- runif(n) < p_debt
    debts <- numeric(n)
    if (any(has_debt)) {
      debts[has_debt] <- rlnorm(
        sum(has_debt),
        meanlog = dist_params$meanlog_debt %||% 0,
        sdlog   = dist_params$sdlog_debt %||% 1
      )
    }
    return(assets - debts)
  }
  stop("Distribution not implemented. Use 'lognormal', 'normal', or 'two_sided_lognormal'.")
}

gini_safe_simple <- function(x, eps = 1e-8) {
  x <- as_num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  if (length(unique(x)) == 1L) return(0)
  xmin <- min(x)
  if (!is.finite(xmin)) return(NA_real_)
  shift <- if (xmin < 0) (-xmin + eps) else 0
  ineq::ineq(x + shift, type = "Gini")
}

as_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  as.numeric(x)
}

lorenz_df_safe <- function(x, label) {
  tmp <- lorenz_coords_safe(x)
  data.frame(p = tmp$lc$p, L = tmp$lc$L, group = label)
}

lorenz_coords_safe <- function(x, eps = 1e-8) {
  x <- as_num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(list(lc = list(p = c(0, 1), L = c(0, 1)), shift = NA_real_))
  xmin <- min(x)
  if (!is.finite(xmin)) return(list(lc = list(p = c(0, 1), L = c(0, 1)), shift = NA_real_))
  shift <- if (xmin < 0) (-xmin + eps) else 0
  lc <- ineq::Lc(x + shift)
  list(lc = lc, shift = shift)
}

lorenz_df_safe <- function(x, label) {
  tmp <- lorenz_coords_safe(x)
  data.frame(p = tmp$lc$p, L = tmp$lc$L, group = label)
}