# ===============================================================
# Packages
# ===============================================================
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(
  tidyverse,  # dplyr, ggplot2, purrr, etc.
  ineq,
  glue
)

# ===============================================================
# Helpers
# ===============================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------- Value generators ----------
# dist options:
#   - "lognormal" (income-like, >0)
#   - "normal" (toy wealth; can be <0)
#   - "two_sided_lognormal" (assets - debts; can be <0)
#     dist_params = list(meanlog_pos, sdlog_pos, p_debt, meanlog_debt, sdlog_debt)
generate_values <- function(n, dist = "lognormal", dist_params = list(meanlog = 0, sdlog = 1)) {
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
      debts[has_debt] <- rlnorm(sum(has_debt),
                                meanlog = dist_params$meanlog_debt %||% 0,
                                sdlog   = dist_params$sdlog_debt %||% 1)
    }
    return(assets - debts)
  }
  stop("Distribution not implemented. Use 'lognormal', 'normal', or 'two_sided_lognormal'.")
}

# ---------- Robust numeric coercion ----------
as_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  as.numeric(x)
}

# ---------- Gini / Lorenz safe wrappers (handle negatives & empties) ----------
gini_safe <- function(x, eps = 1e-8) {
  x <- as_num(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  if (length(unique(x)) == 1L) return(0)
  xmin <- min(x)
  if (!is.finite(xmin)) return(NA_real_)
  shift <- if (xmin < 0) (-xmin + eps) else 0
  ineq::ineq(x + shift, type = "Gini")
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

lorenz_y_at_safe <- function(x, p = 0.7) {
  tmp <- lorenz_coords_safe(x)
  approx(tmp$lc$p, tmp$lc$L, xout = p, ties = "ordered")$y
}

lorenz_df <- function(x, label) {
  tmp <- lorenz_coords_safe(x)
  data.frame(p = tmp$lc$p, L = tmp$lc$L, group = label)
}

# ===============================================================
# Simulation
# ===============================================================

# ---------- Single simulation with guards ----------
# clan_size_scenario: "equal", "random", "rich_big", "rich_small"
simulate_ginis <- function(
  n_clans = 2500,
  hh_per_clan = 4,
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
  if (debug) cat("DEBUG: clan_sizes =", paste(clan_sizes, collapse = ","), " | n_households =", n_households, "\n")

  values <- generate_values(n_households, dist, dist_params)
  if (length(values) != n_households) stop("Generator returned wrong length.")

  df <- tibble::tibble(
    clan  = rep(seq_len(n_clans), times = clan_sizes),
    value = values
  )
  if (nrow(df) != n_households) stop("Row count mismatch constructing df.")

  gini_households <- gini_safe(df$value)

  clan_means <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(is.finite(mean_value), !is.na(mean_value))

  clan_sums <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(sum_value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(is.finite(sum_value), !is.na(sum_value))

  gini_clan_means <- gini_safe(clan_means$mean_value)
  gini_clan_sums  <- gini_safe(clan_sums$sum_value)

  list(
    gini_households = gini_households,
    gini_clan_means = gini_clan_means,
    gini_clan_sums  = gini_clan_sums,
    data = df
  )
}

# ---------- Run many simulations (safer than replicate) ----------
run_many_sims <- function(n_sims = 100, ..., debug_every = NULL) {
  sims <- vector("list", n_sims)
  for (i in seq_len(n_sims)) {
    sims[[i]] <- simulate_ginis(..., debug = !is.null(debug_every) && (i %% debug_every == 0))
    if (nrow(sims[[i]]$data) == 0L) stop(sprintf("Sim %d produced empty data (unexpected).", i))
  }
  sims
}

# ---------- Results frame ----------
results_from_sims <- function(sims) {
  g_hh <- vapply(sims, function(x) gini_safe(x$data$value), numeric(1))
  g_cm <- vapply(sims, function(x) {
    cm <- x$data |>
      dplyr::group_by(clan) |>
      dplyr::summarise(m = mean(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(is.finite(m), !is.na(m)) |>
      dplyr::pull(m)
    if (length(cm) == 0L) return(NA_real_)
    gini_safe(cm)
  }, numeric(1))
  g_cs <- vapply(sims, function(x) {
    cs <- x$data |>
      dplyr::group_by(clan) |>
      dplyr::summarise(s = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(is.finite(s), !is.na(s)) |>
      dplyr::pull(s)
    if (length(cs) == 0L) return(NA_real_)
    gini_safe(cs)
  }, numeric(1))

  tibble::tibble(
    gini_households = g_hh,
    gini_clan_means = g_cm,
    gini_clan_sums  = g_cs
  ) |>
    dplyr::mutate(
      diff_means = gini_households - gini_clan_means,
      diff_sums  = gini_households - gini_clan_sums
    )
}

# ---------- Extremes (NA-safe) ----------
extreme_indices <- function(df_res) {
  get_idx_min <- function(x) {
    oks <- which(is.finite(x))
    if (length(oks) == 0L) stop("No finite values to find minimum.")
    oks[ which.min(x[oks]) ]
  }
  get_idx_max <- function(x) {
    oks <- which(is.finite(x))
    if (length(oks) == 0L) stop("No finite values to find maximum.")
    oks[ which.max(x[oks]) ]
  }
  list(
    idx_min_means = get_idx_min(df_res$diff_means),
    idx_max_means = get_idx_max(df_res$diff_means),
    idx_min_sums  = get_idx_min(df_res$diff_sums),
    idx_max_sums  = get_idx_max(df_res$diff_sums)
  )
}

# ===============================================================
# Plotting
# ===============================================================

plot_lorenz_for_sim <- function(sim, mode = c("means", "sums"), title_prefix = "Difference case") {
  mode <- match.arg(mode)
  df <- sim$data

  x_house <- df$value
  g_house <- gini_safe(x_house)

  if (mode == "means") {
    x_clan <- df |>
      dplyr::group_by(clan) |>
      dplyr::summarise(val = mean(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(is.finite(val)) |>
      dplyr::pull(val)
    mode_label <- "Clan means"
  } else {
    x_clan <- df |>
      dplyr::group_by(clan) |>
      dplyr::summarise(val = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(is.finite(val)) |>
      dplyr::pull(val)
    mode_label <- "Clan sums"
  }
  g_clan <- gini_safe(x_clan)

  ld_house <- lorenz_df(x_house, "Households")
  ld_clan  <- lorenz_df(x_clan,  mode_label)
  ld <- dplyr::bind_rows(ld_house, ld_clan)

  y_house <- lorenz_y_at_safe(x_house, 0.7)
  y_clan  <- lorenz_y_at_safe(x_clan,  0.7)

  ggplot(ld, aes(x = p, y = L, linetype = group)) +
    geom_line(linewidth = 1) +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.4, linetype = "dashed") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Cumulative share of units",
      y = "Cumulative share of income/wealth",
      title = paste0(title_prefix, " — ", mode_label),
      linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom") +
    annotate("text", x = 0.72, y = y_house + 0.05,
             label = sprintf("Gini (Households) = %.3f", g_house),
             hjust = 0, size = 4) +
    annotate("text", x = 0.72, y = y_clan + 0.02,
             label = sprintf("Gini (%s) = %.3f", mode_label, g_clan),
             hjust = 0, size = 4)
}

# ---- Mekko (variable-width) ----
mekko_df <- function(sim, mode = c("means", "sums")) {
  mode <- match.arg(mode)
  df <- sim$data

  cs <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(
      n_hh    = dplyr::n(),
      mean_val = mean(value, na.rm = TRUE),
      sum_val  = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(stat = if (mode == "means") mean_val else sum_val) |>
    dplyr::arrange(stat) |>
    dplyr::mutate(
      width   = n_hh,
      w_share = width / sum(width),
      x_max   = cumsum(w_share),
      x_min   = x_max - w_share,
      y_min   = 0,
      y_max   = stat,
      clan_id = dplyr::row_number()
    )

  g_house <- gini_safe(df$value)
  g_clan  <- if (mode == "means") gini_safe(cs$mean_val) else gini_safe(cs$sum_val)

  list(df = cs, g_house = g_house, g_clan = g_clan)
}

plot_mekko_for_sim <- function(sim, mode = c("means", "sums"), title = "Scenario summary") {
  mode <- match.arg(mode)
  out  <- mekko_df(sim, mode)
  cs   <- out$df

  subtitle <- glue(
    "Gini (Households) = {sprintf('%.3f', out$g_house)}   |   ",
    "Gini (Clans - {if (mode=='means') 'means' else 'sums'}) = {sprintf('%.3f', out$g_clan)}   |   ",
    "Difference = {sprintf('%.3f', out$g_house - out$g_clan)}"
  )

  ggplot(cs) +
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
              color = "grey30", fill = "grey70", alpha = 0.8) +
    labs(
      x = "Cumulative share of households (by clan width)",
      y = if (mode == "means") "Clan mean income/wealth" else "Clan total income/wealth",
      title = title,
      subtitle = subtitle
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

make_four_mekko <- function(sims, df_res,
                            idx_min_means, idx_max_means,
                            idx_min_sums,  idx_max_sums) {
  cases <- list(
    list(idx = idx_min_means, mode = "means",
         label = glue("Lowest diff (household − clan means) [sim {idx_min_means}]")),
    list(idx = idx_max_means, mode = "means",
         label = glue("Highest diff (household − clan means) [sim {idx_max_means}]")),
    list(idx = idx_min_sums, mode = "sums",
         label = glue("Lowest diff (household − clan sums) [sim {idx_min_sums}]")),
    list(idx = idx_max_sums, mode = "sums",
         label = glue("Highest diff (household − clan sums) [sim {idx_max_sums}]"))
  )

  build_case_df <- function(case) {
    out <- mekko_df(sims[[case$idx]], case$mode)
    out$df |>
      dplyr::mutate(
        facet   = case$label,
        y_label = if (case$mode == "means") "Clan mean income/wealth" else "Clan total income/wealth",
        g_house = out$g_house,
        g_clan  = out$g_clan,
        mode    = case$mode
      )
  }

  long_df <- purrr::map_dfr(cases, build_case_df)

  long_df <- long_df |>
    dplyr::group_by(facet, mode, y_label) |>
    dplyr::mutate(
      subtitle = glue(
        "Gini (Households) = {sprintf('%.3f', dplyr::first(g_house))}   |   ",
        "Gini (Clans - {if (dplyr::first(mode)=='means') 'means' else 'sums'}) = {sprintf('%.3f', dplyr::first(g_clan))}   |   ",
        "Difference = {sprintf('%.3f', dplyr::first(g_house) - dplyr::first(g_clan))}"
      )
    ) |>
    dplyr::ungroup()

  ggplot(long_df) +
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
              color = "grey30", fill = "grey70", alpha = 0.85) +
    facet_wrap(~ facet, scales = "free_y") +
    labs(
      x = "Cumulative share of households (by clan width)",
      y = NULL,
      title = "Scenario summaries (variable-width by clan size)",
      subtitle = "Each panel orders clans by the plotted statistic; width = households per clan; height = clan mean (means panels) or clan total (sums panels)."
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold")) +
    geom_text(
      data = long_df %>% dplyr::group_by(facet) %>% dplyr::summarise(
        x = 0.98, y = max(y_max) * 0.98, subtitle = dplyr::first(subtitle), .groups = "drop"
      ),
      aes(x = x, y = y, label = subtitle),
      hjust = 1, vjust = 1, size = 3.2
    )
}

# ===============================================================
# Main (minimal, editable)
# ===============================================================

set.seed(123)

# Example A: Income-like
sims <- run_many_sims(
  n_sims = 1000,
  n_clans = 2500,
  hh_per_clan = 4,
  dist = "lognormal", # 'lognormal', 'normal', or 'two_sided_lognormal'
  dist_params = list(meanlog = 0, sdlog = 1),
  clan_size_scenario = "rich_big"   # try "equal", "random", "rich_small"
  # , debug_every = 10               # uncomment to print clan sizes every 10 sims
)

# Build results and locate extremes
df_res <- results_from_sims(sims)
print(summary(df_res))

ext <- extreme_indices(df_res)  # <-- you were missing this

# Lorenz curves (four extreme cases)
p_means_min <- plot_lorenz_for_sim(
  sims[[ext$idx_min_means]], mode = "means",
  title_prefix = sprintf("Lowest diff (household − clan means) [sim %d]", ext$idx_min_means)
)
p_means_max <- plot_lorenz_for_sim(
  sims[[ext$idx_max_means]], mode = "means",
  title_prefix = sprintf("Highest diff (household − clan means) [sim %d]", ext$idx_max_means)
)
p_sums_min <- plot_lorenz_for_sim(
  sims[[ext$idx_min_sums]], mode = "sums",
  title_prefix = sprintf("Lowest diff (household − clan sums) [sim %d]", ext$idx_min_sums)
)
p_sums_max <- plot_lorenz_for_sim(
  sims[[ext$idx_max_sums]], mode = "sums",
  title_prefix = sprintf("Highest diff (household − clan sums) [sim %d]", ext$idx_max_sums)
)

print(p_means_min); print(p_means_max); print(p_sums_min); print(p_sums_max)

# Mekko (four extreme cases)
p_mekko_four <- make_four_mekko(
  sims, df_res,
  ext$idx_min_means, ext$idx_max_means,
  ext$idx_min_sums,  ext$idx_max_sums
)
print(p_mekko_four) # <-- This is what you look at for understanding which variables create extreme values!

# ---- Example B: Wealth with negatives ----
sims_w <- run_many_sims(
  n_sims = 100,
  n_clans = 5,
  hh_per_clan = 4,
  dist = "two_sided_lognormal",
  dist_params = list(
    meanlog_pos = 1.0, sdlog_pos = 1.0,  # assets
    p_debt = 0.35,                        # share with debt
    meanlog_debt = 0.8, sdlog_debt = 0.9 # debt magnitude
  ),
  clan_size_scenario = "rich_big"
)
df_res_w <- results_from_sims(sims_w)
print(summary(df_res_w))

# Lorenz curves (four extreme cases)
p_means_min_w <- plot_lorenz_for_sim(
  sims_w[[ext$idx_min_means]], mode = "means",
  title_prefix = sprintf("Lowest diff (household − clan means) [sim %d]", ext$idx_min_means)
)
p_means_max_w <- plot_lorenz_for_sim(
  sims_w[[ext$idx_max_means]], mode = "means",
  title_prefix = sprintf("Highest diff (household − clan means) [sim %d]", ext$idx_max_means)
)
p_sums_min_w <- plot_lorenz_for_sim(
  sims_w[[ext$idx_min_sums]], mode = "sums",
  title_prefix = sprintf("Lowest diff (household − clan sums) [sim %d]", ext$idx_min_sums)
)
p_sums_max_w <- plot_lorenz_for_sim(
  sims_w[[ext$idx_max_sums]], mode = "sums",
  title_prefix = sprintf("Highest diff (household − clan sums) [sim %d]", ext$idx_max_sums)
)

print(p_means_min_w); print(p_means_max_w); print(p_sums_min_w); print(p_sums_max_w)

# Mekko (four extreme cases)
p_mekko_four_w <- make_four_mekko(
  sims_w, df_res,
  ext$idx_min_means, ext$idx_max_means,
  ext$idx_min_sums,  ext$idx_max_sums
)
print(p_mekko_four_w) # <-- This is what you look at for understanding which variables create extreme values!











