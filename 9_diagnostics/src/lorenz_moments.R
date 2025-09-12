library(tidyverse)
library(ineq)
library(here)

# LOAD DATA ------------------------------------------------------------------
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

r_hh_wealth <- r_hh %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

r_clans_wealth <- r_clans %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

# Get all available years
years_income <- sort(unique(r_hh$year))
years_wealth <- sort(unique(r_hh$year))  # or r_hh_wealth$year if wealth is subset

# FUNCTIONS ------------------------------------------------------------------

# Lorenz curve table
lorenz_tbl <- function(x, w = NULL) {
  L <- if (is.null(w)) Lc(x) else Lc(x, n = w)  # Apply weights
  tibble(p = L$p, L = L$L)
}

# --- scaled conditional mean M(u) = L(u) / u  --------------------------
scmc_from_lorenz <- function(p, L) {
  # safe divide, define M(0) from right-hand limit
  denom <- pmax(p, .Machine$double.eps)
  M <- L / denom
  if (!is.na(p[1]) && p[1] == 0) {
    first_pos <- which(p > 0)[1]
    if (!is.na(first_pos)) M[1] <- M[first_pos]
  }
  tibble(p = p, L = L, M = M)
}

# Build Lorenz curves (weighted + unweighted)
get_lorenz_both <- function(df, value_var, weight_var, years, unit_label) {
  base <- df %>%
    filter(year %in% years, is.finite(.data[[value_var]])) %>%
    transmute(year, value = .data[[value_var]], w = .data[[weight_var]])

  # Unweighted
  unw <- base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      lorenz_tbl(d$value) %>%
        { scmc_from_lorenz(.$p, .$L) } %>%
        mutate(year = unique(d$year), weighting = "Unweighted")
    })

  # Weighted
  wtd <- base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      lorenz_tbl(d$value, d$w) %>%
        { scmc_from_lorenz(.$p, .$L) } %>%
        mutate(year = unique(d$year), weighting = "Weighted")
    })

  bind_rows(unw, wtd) %>%
    mutate(Unit = factor(unit_label, levels = c("Household", "Clan")))
}

# Numerical integration (trapezoidal rule)
trapz <- function(x, y) {
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
}

# Aaberge’s D1, D2, D3 (first three moments of the Lorenz curve)
calc_D123 <- function(p, L) {
  D1 <- trapz(p, L)                 # ∫ L(u) du
  D2 <- 2 * trapz(p, (1 - p) * L)   # 2∫ (1-u)L(u) du
  D3 <- 3 * trapz(p, (1 - p)^2 * L) # 3∫ (1-u)^2L(u) du
  tibble(D1 = D1, D2 = D2, D3 = D3)
}

# Gini from Lorenz curve
calc_gini_from_lorenz <- function(p, L) {
  1 - 2 * trapz(p, L)
}

# --- Bonferroni index from Lorenz (via SCMC) ---------------------------
# B = 1 - ∫_0^1 [ L(u) / u ] du   (area under M(u))
calc_bonf_from_lorenz <- function(p, L) {
  denom <- pmax(p, .Machine$double.eps)
  M <- L / denom
  # right-limit at 0 already handled by denom; the first segment contributes ~0
  B <- trapz(p, M)
  B
}

# INCOME ---------------------------------------------------------------------
inc_hh   <- get_lorenz_both(r_hh,    "inc_all",     "fam_weight",  years_income, "Household")
inc_clan <- get_lorenz_both(r_clans, "inc_all",     "clan_weight", years_income, "Clan")
inc_all  <- bind_rows(inc_hh, inc_clan)

write_csv(inc_all, here("9_diagnostics", "output", "income_L_and_SCMC_curves.csv"))

inc_unw <- inc_all %>%
  filter(weighting == "Unweighted") %>%
  group_by(Unit, year) %>%
  summarise(
    calc_D123(p, L),
    gini = calc_gini_from_lorenz(p, L),
    bonferroni = calc_bonf_from_lorenz(p, L),
    .groups = "drop"
  ) %>%
  mutate(valid_d1 = 1 - 2*D1)

write_csv(inc_unw, here("9_diagnostics", "output", "income_D123.csv"))


# WEALTH ---------------------------------------------------------------------
w_hh   <- get_lorenz_both(r_hh_wealth,    "wealth_nohouse",     "fam_weight",  years_wealth, "Household")
w_clan <- get_lorenz_both(r_clans_wealth, "wealth_nohouse",     "clan_weight", years_wealth, "Clan")
w_all <- bind_rows(w_hh, w_clan)

write_csv(w_all, here("9_diagnostics", "output", "wealth_L_and_SCMC_curves.csv"))

w_unw <- w_all %>%
  filter(weighting == "Unweighted") %>%
  group_by(Unit, year) %>%
  summarise(
    calc_D123(p, L),
    gini = calc_gini_from_lorenz(p, L),
    bonferroni = calc_bonf_from_lorenz(p, L),
    .groups = "drop"
  ) %>%
  mutate(valid_d1 = 1 - 2*D1)

write_csv(w_unw, here("9_diagnostics", "output", "wealth_D123.csv"))

# ---- Single-panel overlay for a chosen Unit/weighting/year set ----------
plot_scmc_vs_lorenz <- function(curves_df,
                                unit = "Household",
                                weighting = "Unweighted",
                                years = NULL,
                                ylim = NULL) {
  df <- curves_df %>%
    filter(Unit == unit, weighting == weighting) %>%
    { if (!is.null(years)) filter(., year %in% years) else . } %>%
    select(Unit, weighting, year, p, L, M) %>%
    pivot_longer(cols = c(L, M), names_to = "curve", values_to = "value") %>%
    mutate(curve = recode(curve, L = "Lorenz  L(u)", M = "SCMC  M(u)=L(u)/u"))

  ggplot(df, aes(x = p, y = value, color = curve)) +
    geom_line(linewidth = 1) +
    # reference guides: diagonal for Lorenz; horizontal 1 for SCMC
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", linewidth = 0.4, alpha = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
    facet_wrap(~ year, scales = "free_y") +
    coord_cartesian(ylim = ylim) +
    scale_color_manual(values = c("Lorenz  L(u)" = "#1b9e77", "SCMC  M(u)=L(u)/u" = "#d95f02")) +
    labs(x = "u (population share, poorest first)",
         y = "Value",
         color = NULL,
         title = sprintf("Lorenz vs. Scaled Conditional Mean — %s (%s)", unit, weighting)) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top")
}

# households, unweighted, all years
p1 <- plot_scmc_vs_lorenz(inc_all, unit = "Household", weighting = "Unweighted")
# print(p1)

# clans, unweighted, all years
p2 <- plot_scmc_vs_lorenz(inc_all, unit = "Clan", weighting = "Unweighted")

# Batch: make one plot per (Unit, weighting) and save ----------------
# save_all_scmc_lorenz_plots <- function(curves_df, out_dir = "9_diagnostics/output",
#                                        file_stub = "Lorenz_vs_SCMC") {
#   dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#   combos <- curves_df %>%
#     distinct(Unit, weighting) %>%
#     arrange(Unit, weighting)

#   pwalk(combos, function(Unit, weighting) {
#     g <- plot_scmc_vs_lorenz(curves_df, unit = Unit, weighting = weighting)
#     fn <- file.path(out_dir,
#                     sprintf("%s_%s_%s.png",
#                             file_stub,
#                             gsub("\\s+", "", as.character(Unit)),
#                             gsub("\\s+", "", as.character(weighting))))
#     ggsave(fn, g, width = 10, height = 6, dpi = 300)
#     message("Saved: ", fn)
#   })
# }

