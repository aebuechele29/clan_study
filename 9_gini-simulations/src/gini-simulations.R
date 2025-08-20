if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")

pacman::p_load(
  tidyverse,
  ineq,
  ggplot2,
  glue
)

# Function to simulate incomes and compute ginis
simulate_ginis <- function(
  n_clans = 10, 
  hh_per_clan = 20, 
  dist = "lognormal", 
  dist_params = list(meanlog = 0, sdlog = 1),
  clan_size_scenario = "equal" # options: "equal", "random", "rich_big", "rich_small"
) {
  # Step 1: generate household incomes
  n_households <- n_clans * hh_per_clan * 2  # oversample, we'll trim later
  if (dist == "lognormal") {
    incomes <- rlnorm(n_households, 
                      meanlog = dist_params$meanlog, 
                      sdlog   = dist_params$sdlog)
  } else if (dist == "pareto") {
    alpha <- dist_params$alpha
    xm <- dist_params$xm
    incomes <- xm / runif(n_households)^(1/alpha)
  } else {
    stop("Distribution not implemented")
  }
  
  # Step 2: decide clan sizes
  if (clan_size_scenario == "equal") {
    clan_sizes <- rep(hh_per_clan, n_clans)
  } else if (clan_size_scenario == "random") {
    clan_sizes <- pmax(1, rpois(n_clans, lambda = hh_per_clan))
  } else if (clan_size_scenario == "rich_big") {
    # richest clan gets most households, poorest gets least
    clan_sizes <- round(seq(1, hh_per_clan * 2, length.out = n_clans))
  } else if (clan_size_scenario == "rich_small") {
    # richest clan gets least households, poorest gets most
    clan_sizes <- round(seq(hh_per_clan * 2, 1, length.out = n_clans))
  } else {
    stop("Unknown clan_size_scenario")
  }
  
  # Step 3: assign households to clans
  clan_ids <- rep(1:n_clans, times = clan_sizes)
  incomes <- incomes[1:length(clan_ids)]
  
  df <- data.frame(clan = clan_ids, income = incomes)
  
  # Step 4: Ginis
  gini_households <- ineq::ineq(df$income, type = "Gini")
  clan_means <- df %>% dplyr::group_by(clan) %>% dplyr::summarise(mean_income = mean(income))
  clan_sums  <- df %>% dplyr::group_by(clan) %>% dplyr::summarise(sum_income  = sum(income))
  
  gini_clan_means <- ineq::ineq(clan_means$mean_income, type = "Gini")
  gini_clan_sums  <- ineq::ineq(clan_sums$sum_income, type = "Gini")
  
  list(
    gini_households = gini_households,
    gini_clan_means = gini_clan_means,
    gini_clan_sums  = gini_clan_sums,
    data = df
  )
}

# ---- Helper: build Lorenz data frame for ggplot ----
lorenz_df <- function(x, label) {
    lc <- ineq::Lc(x)
    data.frame(p = lc$p, L = lc$L, group = label)
}

# Helper: nice Gini annotation y-position (grab Lorenz value at p = 0.7)
lorenz_y_at <- function(x, p = 0.7) {
    lc <- ineq::Lc(x)
    approx(lc$p, lc$L, xout = p, ties = "ordered")$y
}

############################## Main ##############################

set.seed(42)

# Equal sizes
# equal <- simulate_ginis(n_clans = 5, hh_per_clan = 10, clan_size_scenario = "equal")

# # Richest clans are largest
# left_tail <- simulate_ginis(n_clans = 5, hh_per_clan = 10, clan_size_scenario = "rich_big")

# # Richest clans are smallest
# right_tail  <- simulate_ginis(n_clans = 5, hh_per_clan = 10, clan_size_scenario = "rich_small")


# Example: run many simulations and compare averages
set.seed(123)
sims_rich_big <- replicate(100, simulate_ginis(
  n_clans = 5, hh_per_clan = 10, clan_size_scenario = "rich_big"
), simplify = FALSE)

df_res <- data.frame(
  gini_households = sapply(sims_rich_big, function(x) x$gini_households),
  gini_clan_means = sapply(sims_rich_big, function(x) x$gini_clan_means),
  gini_clan_sums  = sapply(sims_rich_big, function(x) x$gini_clan_sums)
)

df_res <- df_res %>%
  mutate(
    diff_means = gini_households - gini_clan_means,
    diff_sums  = gini_households - gini_clan_sums
  )

summary(df_res)

# ---- Build df_res with differences from your sims_rich_big list ----
df_res <- data.frame(
  gini_households = sapply(sims_rich_big, function(x) ineq(x$data$income, type = "Gini")),
  gini_clan_means = sapply(sims_rich_big, function(x) {
    x$data %>% group_by(clan) %>% summarise(mean_income = mean(income), .groups = "drop") %>%
      pull(mean_income) %>% ineq(type = "Gini")
  }),
  gini_clan_sums = sapply(sims_rich_big, function(x) {
    x$data %>% group_by(clan) %>% summarise(sum_income = sum(income), .groups = "drop") %>%
      pull(sum_income) %>% ineq(type = "Gini")
  })
) %>%
  mutate(
    diff_means = gini_households - gini_clan_means,
    diff_sums  = gini_households - gini_clan_sums
  )

# ---- Core plotting function ----
# mode = "means" or "sums"
plot_lorenz_for_sim <- function(sim, mode = c("means", "sums"),
                                title_prefix = "Difference case") {
  mode <- match.arg(mode)
  df <- sim$data
  
  # Household distribution
  x_house <- df$income
  g_house <- ineq(x_house, type = "Gini")
  
  # Clan distribution
  if (mode == "means") {
    x_clan <- df %>% group_by(clan) %>% summarise(val = mean(income), .groups = "drop") %>% pull(val)
    mode_label <- "Clan means"
  } else {
    x_clan <- df %>% group_by(clan) %>% summarise(val = sum(income), .groups = "drop") %>% pull(val)
    mode_label <- "Clan sums"
  }
  g_clan <- ineq(x_clan, type = "Gini")
  
  # Lorenz data
  ld_house <- lorenz_df(x_house, "Households")
  ld_clan  <- lorenz_df(x_clan,  mode_label)
  ld <- bind_rows(ld_house, ld_clan)
  
  # Annotation positions
  y_house <- lorenz_y_at(x_house, 0.7)
  y_clan  <- lorenz_y_at(x_clan,  0.7)
  
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

# ---- Find indices for extreme differences ----
idx_min_means <- which.min(df_res$diff_means)
idx_max_means <- which.max(df_res$diff_means)
idx_min_sums  <- which.min(df_res$diff_sums)
idx_max_sums  <- which.max(df_res$diff_sums)

# ---- Make the four figures ----
p_means_min <- plot_lorenz_for_sim(
  sims_rich_big[[idx_min_means]], mode = "means",
  title_prefix = sprintf("Lowest diff (household - clan means) [sim %d]", idx_min_means)
)

p_means_max <- plot_lorenz_for_sim(
  sims_rich_big[[idx_max_means]], mode = "means",
  title_prefix = sprintf("Highest diff (household - clan means) [sim %d]", idx_max_means)
)

p_sums_min <- plot_lorenz_for_sim(
  sims_rich_big[[idx_min_sums]], mode = "sums",
  title_prefix = sprintf("Lowest diff (household - clan sums) [sim %d]", idx_min_sums)
)

p_sums_max <- plot_lorenz_for_sim(
  sims_rich_big[[idx_max_sums]], mode = "sums",
  title_prefix = sprintf("Highest diff (household - clan sums) [sim %d]", idx_max_sums)
)

# ---- Print to viewer ----
print(p_means_min)
print(p_means_max)
print(p_sums_min)
print(p_sums_max)

# (Optional) Save to files
# ggsave("lorenz_means_lowest_diff.png", p_means_min, width = 7, height = 5, dpi = 300)
# ggsave("lorenz_means_highest_diff.png", p_means_max, width = 7, height = 5, dpi = 300)
# ggsave("lorenz_sums_lowest_diff.png",  p_sums_min,  width = 7, height = 5, dpi = 300)
# ggsave("lorenz_sums_highest_diff.png", p_sums_max, width = 7, height = 5, dpi = 300)

# ---- Helper: compute clan stats + rectangles for a Mekko plot ----
mekko_df <- function(sim, mode = c("means", "sums")) {
  mode <- match.arg(mode)
  df <- sim$data
  
  cs <- df %>%
    group_by(clan) %>%
    summarise(
      n_hh = n(),
      mean_val = mean(income),
      sum_val  = sum(income),
      .groups = "drop"
    ) %>%
    mutate(
      stat = if (mode == "means") mean_val else sum_val
    ) %>%
    arrange(stat) %>%
    mutate(
      width   = n_hh,
      w_share = width / sum(width),
      x_max   = cumsum(w_share),
      x_min   = x_max - w_share,
      y_min   = 0,
      y_max   = stat,
      clan_id = row_number()
    )
  
  # Ginis for subtitle
  g_house <- ineq(df$income, type = "Gini")
  g_clan  <- if (mode == "means") {
    ineq(cs$mean_val, type = "Gini")
  } else {
    ineq(cs$sum_val, type = "Gini")
  }
  
  list(df = cs, g_house = g_house, g_clan = g_clan)
}

# ---- Plot one Mekko panel for a given sim/mode ----
plot_mekko_for_sim <- function(sim, mode = c("means", "sums"),
                               title = "Scenario summary") {
  mode <- match.arg(mode)
  out  <- mekko_df(sims_rich_big, mode)
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

# ---- Build a combined four‑panel figure for your extreme cases ----
# Assumes: df_res and sims exist, and indices computed previously:
# idx_min_means, idx_max_means, idx_min_sums, idx_max_sums

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
  
  # Build a long df of rectangles with a facet label
  build_case_df <- function(case) {
    out <- mekko_df(sims_rich_big[[case$idx]], case$mode)
    out$df %>%
      mutate(
        facet = case$label,
        y_label = if (case$mode == "means") "Clan mean income/wealth" else "Clan total income/wealth",
        g_house = out$g_house,
        g_clan  = out$g_clan,
        mode    = case$mode
      )
  }
  
  long_df <- do.call(bind_rows, lapply(cases, build_case_df))
  
  # Pretty subtitles per facet
  long_df <- long_df %>%
    group_by(facet, mode, y_label) %>%
    mutate(
      subtitle = glue(
        "Gini (Households) = {sprintf('%.3f', first(g_house))}   |   ",
        "Gini (Clans - {if (first(mode)=='means') 'means' else 'sums'}) = {sprintf('%.3f', first(g_clan))}   |   ",
        "Difference = {sprintf('%.3f', first(g_house) - first(g_clan))}"
      )
    ) %>%
    ungroup()
  
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
    # Add per-facet subtitles by placing text at the top-right corner
    # (optional: remove if you prefer cleaner panels)
    geom_text(
      data = long_df %>% group_by(facet) %>% summarise(
        x = 0.98, y = max(y_max) * 0.98, subtitle = first(subtitle), .groups = "drop"
      ),
      aes(x = x, y = y, label = subtitle),
      hjust = 1, vjust = 1, size = 3.2
    )
}

# ---- Create & display the four‑panel Mekko figure ----
p_mekko_four <- make_four_mekko(
  sims_rich_big, df_res,
  idx_min_means, idx_max_means,
  idx_min_sums,  idx_max_sums
)
print(p_mekko_four)

# If you also want single‑panel versions:
# print(plot_mekko_for_sim(sims[[idx_min_means]], "means",
#       title = sprintf("Lowest diff (household − clan means) [sim %d]", idx_min_means)))
# print(plot_mekko_for_sim(sims[[idx_max_sums]],  "sums",
#       title = sprintf("Highest diff (household − clan sums) [sim %d]",  idx_max_sums)))
