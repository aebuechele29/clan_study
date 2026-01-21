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
# Simulation (SUMS ONLY)
# ===============================================================

# ---------- Single simulation with guards ----------
# clan_size_scenario: "equal", "random", "rich_big", "rich_small"
simulate_ginis <- function(
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
  if (debug) cat("DEBUG: clan_sizes =", paste(clan_sizes, collapse = ","), " | n_households =", n_households, "\n")

  values <- generate_values(n_households, dist, dist_params)
  if (length(values) != n_households) stop("Generator returned wrong length.")

  df <- tibble::tibble(
    clan  = rep(seq_len(n_clans), times = clan_sizes),
    value = values
  )
  if (nrow(df) != n_households) stop("Row count mismatch constructing df.")

  gini_households <- gini_safe(df$value)

  clan_sums <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(sum_value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(is.finite(sum_value), !is.na(sum_value))

  gini_clan_sums <- gini_safe(clan_sums$sum_value)

  list(
    gini_households = gini_households,
    gini_clan_sums  = gini_clan_sums,
    data = df
  )
}

# ---------- Run many simulations ----------
run_many_sims <- function(n_sims = 100, ..., debug_every = NULL) {
  sims <- vector("list", n_sims)
  for (i in seq_len(n_sims)) {
    sims[[i]] <- simulate_ginis(..., debug = !is.null(debug_every) && (i %% debug_every == 0))
    if (nrow(sims[[i]]$data) == 0L) stop(sprintf("Sim %d produced empty data (unexpected).", i))
  }
  sims
}

# ---------- Results frame (SUMS ONLY) ----------
results_from_sims <- function(sims) {
  g_hh <- vapply(sims, function(x) gini_safe(x$data$value), numeric(1))
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
    gini_clan_sums  = g_cs
  ) |>
    dplyr::mutate(diff_sums = gini_households - gini_clan_sums)
}

# ---------- Extremes (NA-safe) ----------
extreme_indices <- function(df_res) {
  oks <- which(is.finite(df_res$diff_sums))
  if (length(oks) == 0L) stop("No finite values to find extremes.")
  list(
    idx_min_sums = oks[which.min(df_res$diff_sums[oks])],
    idx_max_sums = oks[which.max(df_res$diff_sums[oks])]
  )
}

# ===============================================================
# Plotting (SUMS ONLY)
# ===============================================================

plot_lorenz_for_sim <- function(sim, title_prefix = "Difference case") {
  df <- sim$data

  x_house <- df$value
  g_house <- gini_safe(x_house)

  x_clan <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(val = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(is.finite(val)) |>
    dplyr::pull(val)

  g_clan <- gini_safe(x_clan)

  ld_house <- lorenz_df(x_house, "Households")
  ld_clan  <- lorenz_df(x_clan,  "Clan sums")
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
      title = paste0(title_prefix, " — Clan sums"),
      linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom") +
    annotate("text", x = 0.72, y = y_house + 0.05,
             label = sprintf("Gini (Households) = %.3f", g_house),
             hjust = 0, size = 4) +
    annotate("text", x = 0.72, y = y_clan + 0.02,
             label = sprintf("Gini (Clan sums) = %.3f", g_clan),
             hjust = 0, size = 4)
}

# ---- Mekko (variable-width; SUMS ONLY) ----
mekko_df <- function(sim) {
  df <- sim$data

  cs <- df |>
    dplyr::group_by(clan) |>
    dplyr::summarise(
      n_hh   = dplyr::n(),
      sum_val = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(sum_val) |>
    dplyr::mutate(
      width   = n_hh,
      w_share = width / sum(width),
      x_max   = cumsum(w_share),
      x_min   = x_max - w_share,
      y_min   = 0,
      y_max   = sum_val,
      clan_id = dplyr::row_number()
    )

  g_house <- gini_safe(df$value)
  g_clan  <- gini_safe(cs$sum_val)

  list(df = cs, g_house = g_house, g_clan = g_clan)
}

plot_mekko_for_sim <- function(sim, title = "Scenario summary") {
  out <- mekko_df(sim)
  cs  <- out$df

  subtitle <- glue(
    "Gini (Households) = {sprintf('%.3f', out$g_house)}   |   ",
    "Gini (Clans - sums) = {sprintf('%.3f', out$g_clan)}   |   ",
    "Difference = {sprintf('%.3f', out$g_house - out$g_clan)}"
  )

  ggplot(cs) +
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
              color = "grey30", fill = "grey70", alpha = 0.8) +
    labs(
      x = "Cumulative share of households (by clan width)",
      y = "Clan total income/wealth",
      title = title,
      subtitle = subtitle
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

make_two_mekko <- function(sims, df_res, idx_min_sums, idx_max_sums) {
  cases <- list(
    list(idx = idx_min_sums, label = glue("Lowest diff (household − clan sums) [sim {idx_min_sums}]")),
    list(idx = idx_max_sums, label = glue("Highest diff (household − clan sums) [sim {idx_max_sums}]"))
  )

  long_df <- purrr::map_dfr(cases, function(case) {
    out <- mekko_df(sims[[case$idx]])
    out$df |>
      dplyr::mutate(
        facet   = case$label,
        g_house = out$g_house,
        g_clan  = out$g_clan
      )
  })

  long_df <- long_df |>
    dplyr::group_by(facet) |>
    dplyr::mutate(
      subtitle = glue(
        "Gini (Households) = {sprintf('%.3f', dplyr::first(g_house))}   |   ",
        "Gini (Clans - sums) = {sprintf('%.3f', dplyr::first(g_clan))}   |   ",
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
      subtitle = "Each panel orders clans by clan total; width = households per clan; height = clan total."
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
# Main (minimal, editable) — SUMS ONLY
# ===============================================================

set.seed(123)

# Example A: Income-like
sims <- run_many_sims(
  n_sims = 100,
  n_clans = 5,
  hh_per_clan = 10,
  dist = "lognormal",
  dist_params = list(meanlog = 0, sdlog = 1),
  clan_size_scenario = "rich_big"
)

df_res <- results_from_sims(sims)
print(summary(df_res))

ext <- extreme_indices(df_res)

# Lorenz curves (two extreme cases)
p_sums_min <- plot_lorenz_for_sim(
  sims[[ext$idx_min_sums]],
  title_prefix = sprintf("Lowest diff (household − clan sums) [sim %d]", ext$idx_min_sums)
)
p_sums_max <- plot_lorenz_for_sim(
  sims[[ext$idx_max_sums]],
  title_prefix = sprintf("Highest diff (household − clan sums) [sim %d]", ext$idx_max_sums)
)

# Export these!!!
print(p_sums_min); print(p_sums_max)

# Mekko (two extreme cases)
p_mekko_two <- make_two_mekko(sims, df_res, ext$idx_min_sums, ext$idx_max_sums)
# Export this!
print(p_mekko_two)


# install.packages(c("officer","rvg"))
library(officer)
library(rvg)

out_docx <- "appendixa.docx"

doc <- read_docx()

# --- Lorenz plots ---
doc <- doc |>
  body_add_par("Lorenz curves — extreme cases (clan sums)", style = "heading 1") |>
  body_add_par(sprintf("Lowest diff (sim %d)", ext$idx_min_sums), style = "heading 2") |>
  body_add(dml(ggobj = p_sums_min), width = 6.5, height = 4.5) |>
  body_add_par("") |>
  body_add_par(sprintf("Highest diff (sim %d)", ext$idx_max_sums), style = "heading 2") |>
  body_add(dml(ggobj = p_sums_max), width = 6.5, height = 4.5) |>
  body_add_par("")

# --- Mekko panel ---
doc <- doc |>
  body_add_par("Mekko summaries — extreme cases (clan sums)", style = "heading 1") |>
  body_add(dml(ggobj = p_mekko_two), width = 6.5, height = 6)

print(doc, target = out_docx)
message("Saved: ", out_docx)


# TOGGLING THIS SECTION OFF FOR NOW BECAUSE OUR MAIN RESULTS DON'T HAVE NEGATIVE VALUES IN THEM
# # ---- Example B: Wealth with negatives ----
# sims_w <- run_many_sims(
#   n_sims = 100,
#   n_clans = 5,
#   hh_per_clan = 10,
#   dist = "two_sided_lognormal",
#   dist_params = list(
#     meanlog_pos = 1.0, sdlog_pos = 1.0,
#     p_debt = 0.35,
#     meanlog_debt = 0.8, sdlog_debt = 0.9
#   ),
#   clan_size_scenario = "rich_big"
# )

# df_res_w <- results_from_sims(sims_w)
# print(summary(df_res_w))

# ext_w <- extreme_indices(df_res_w)

# p_sums_min_w <- plot_lorenz_for_sim(
#   sims_w[[ext_w$idx_min_sums]],
#   title_prefix = sprintf("Lowest diff (household − clan sums) [sim %d]", ext_w$idx_min_sums)
# )
# p_sums_max_w <- plot_lorenz_for_sim(
#   sims_w[[ext_w$idx_max_sums]],
#   title_prefix = sprintf("Highest diff (household − clan sums) [sim %d]", ext_w$idx_max_sums)
# )

# print(p_sums_min_w); print(p_sums_max_w)

# p_mekko_two_w <- make_two_mekko(sims_w, df_res_w, ext_w$idx_min_sums, ext_w$idx_max_sums)
# print(p_mekko_two_w)
