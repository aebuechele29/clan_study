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
years_wealth <- sort(unique(r_hh_wealth$year))  

# FUNCTIONS ------------------------------------------------------------------

# Lorenz curve table
lorenz_tbl <- function(x, w = NULL) {
  L <- if (is.null(w)) Lc(x) else Lc(x, n = w)  # Apply weights
  tibble(p = L$p, L = L$L)
}

<<<<<<< HEAD
# Scaled conditional mean M(u) = L(u) / u  
=======
# --- scaled conditional mean M(u) = L(u) / u  --------------------------
>>>>>>> c3f72331da13891e251385797078402f9226ba81
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

# Aaberge (2000) D1, D2, D3 (first three moments of the Lorenz curve) 
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

<<<<<<< HEAD
# Bonferroni index from Lorenz (via SCMC) ---------------------------
=======
# --- Bonferroni index from Lorenz (via SCMC) ---------------------------
>>>>>>> c3f72331da13891e251385797078402f9226ba81
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

<<<<<<< HEAD
# write_csv(inc_all, here("9_diagnostics", "output", "inc_curve_inputs.csv"))

# Compute weighted Lorenz-based indices
inc_w <- inc_all %>%
  filter(weighting == "Weighted") %>%
=======
write_csv(inc_all, here("9_diagnostics", "output", "income_L_and_SCMC_curves.csv"))

inc_unw <- inc_all %>%
  filter(weighting == "Unweighted") %>%
>>>>>>> c3f72331da13891e251385797078402f9226ba81
  group_by(Unit, year) %>%
  summarise(
    calc_D123(p, L),
    gini = calc_gini_from_lorenz(p, L),
    bonferroni = calc_bonf_from_lorenz(p, L),
    .groups = "drop"
  ) %>%
  # Aaberge (2007) transforms: D1=C2, D2=C3, D3 dropped
  rename(C3 = D2) %>%
  transmute(
    Unit, year,
    C1 = bonferroni,
    C2 = gini,
    C3 = C3
  )

# Define year bands (same structure as wealth)
year_bands <- tibble(
  year = c(1969:1997, 1999, 2001, 2003, 2005, 2007, 2009,
           2011, 2013, 2015, 2017, 2019, 2021),
  band = case_when(
    year >= 1969 & year <= 1980 ~ "1969–1980",
    year >= 1981 & year <= 1990 ~ "1981–1990",
    year >= 1991 & year <= 2000 ~ "1991–2000",
    year >= 2001 & year <= 2010 ~ "2001–2010",
    year >= 2011 & year <= 2021 ~ "2011–2021",
    TRUE ~ NA_character_
  )
)

# Compute banded + overall averages
inc_summary <- inc_w %>%
  left_join(year_bands, by = "year") %>%
  mutate(band = coalesce(band, "Other")) %>%
  group_by(Unit, band) %>%
  summarise(across(c(C1, C2, C3), mean, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    inc_w %>%
      group_by(Unit) %>%
      summarise(across(c(C1, C2, C3), mean, na.rm = TRUE), .groups = "drop") %>%
      mutate(band = "All")
  ) %>%
  pivot_wider(
    names_from = Unit,
    values_from = c(C1, C2, C3),
    names_glue = "{.value}_{Unit}"
  ) %>%
  relocate(band, .before = 1)

# Format for export
inc_export <- inc_summary %>%
  rename(
    Band             = band,
    `C1 (Bonf.) HH`  = C1_Household,
    `C1 (Bonf.) Clan`= C1_Clan,
    `C2 (Gini) HH`   = C2_Household,
    `C2 (Gini) Clan` = C2_Clan,
    `C3 HH`          = C3_Household,
    `C3 Clan`        = C3_Clan
  ) %>%
  mutate(across(-Band, ~ format(round(.x, 3), nsmall = 3)))

# Build flextable
ft <- flextable(inc_export) |>
  set_caption("Table X. Income Inequality Indices by Period (Weighted)") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

# Add note + export
note_style <- fp_text(font.size = 9, italic = TRUE)

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(
      ftext("Note: C1 = Bonferroni index, C2 = Gini coefficient, C3 = third Lorenz moment. Based on weighted household and clan data.", 
            prop = note_style),
      fp_p = fp_par(text.align = "center")
    )
  )

print(doc, target = here("9_diagnostics", "output", "income_family.docx"))

<<<<<<< HEAD
=======
write_csv(inc_unw, here("9_diagnostics", "output", "income_D123.csv"))
>>>>>>> c3f72331da13891e251385797078402f9226ba81


# WEALTH ---------------------------------------------------------------------
w_hh   <- get_lorenz_both(r_hh_wealth,    "wealth_nohouse",     "fam_weight",  years_wealth, "Household")
w_clan <- get_lorenz_both(r_clans_wealth, "wealth_nohouse",     "clan_weight", years_wealth, "Clan")
w_all <- bind_rows(w_hh, w_clan)
<<<<<<< HEAD
=======

write_csv(w_all, here("9_diagnostics", "output", "wealth_L_and_SCMC_curves.csv"))
>>>>>>> c3f72331da13891e251385797078402f9226ba81

# write_csv(w_all, here("9_diagnostics", "output", "wealth_curve_inputs.csv"))

# Aaberge (2007) transforms the Lorenz curve to the SCMC and defines the moments of the SCMC
# D1 is C2 = Gini
# D2 is C3 
# D3 is replaced by the Bonferroni index, called C1

# Compute weighted Lorenz-based indices
w_w <- w_all %>%
  filter(weighting == "Weighted") %>%
  group_by(Unit, year) %>%
  summarise(
    calc_D123(p, L),
    gini = calc_gini_from_lorenz(p, L),
    bonferroni = calc_bonf_from_lorenz(p, L),
    .groups = "drop"
  ) %>%
  # Aaberge (2007) transforms: D1=C2, D2=C3, D3 -> dropped
  rename(C3 = D2) %>%
  transmute(
    Unit, year,
    C1 = bonferroni,
    C2 = gini,
    C3 = C3
  )

<<<<<<< HEAD
# Define year bands
year_bands <- tibble(
  year = c(1984, 1989, 1994,
           1999, 2001, 2003, 2005, 2007, 2009,
           2011, 2013, 2015, 2017, 2019, 2021),
  band = c(rep("1984–1994", 3),
           rep("1999–2009", 6),
           rep("2011–2021", 6))
)

# Compute banded and overall averages
w_summary <- w_w %>%
  left_join(year_bands, by = "year") %>%
  mutate(band = coalesce(band, "Other")) %>%
  group_by(Unit, band) %>%
  summarise(across(c(C1, C2, C3), mean, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    w_w %>%
      group_by(Unit) %>%
      summarise(across(c(C1, C2, C3), mean, na.rm = TRUE), .groups = "drop") %>%
      mutate(band = "All")
  ) %>%
  pivot_wider(
    names_from = Unit,
    values_from = c(C1, C2, C3),
    names_glue = "{.value}_{Unit}"
  ) %>%
  relocate(band, .before = 1)

# Format for export
w_export <- w_summary %>%
  rename(
    Band             = band,
    `C1 (Bonf.) HH`  = C1_Household,
    `C1 (Bonf.) Clan`= C1_Clan,
    `C2 (Gini) HH`   = C2_Household,
    `C2 (Gini) Clan` = C2_Clan,
    `C3 HH`          = C3_Household,
    `C3 Clan`        = C3_Clan
  ) %>%
  mutate(across(-Band, ~ format(round(.x, 3), nsmall = 3)))

# Build flextable
ft <- flextable(w_export) |>
  set_caption("Table X. Wealth Inequality Indices by Period (Weighted)") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

# Add note + export
note_style <- fp_text(font.size = 9, italic = TRUE)

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(
      ftext("Note: C1 = Bonferroni index, C2 = Gini coefficient, C3 = third Lorenz moment. Based on weighted household and clan data.", 
            prop = note_style),
      fp_p = fp_par(text.align = "center")
    )
  )

print(doc, target = here("9_diagnostics", "output", "wealth_family.docx"))
=======
write_csv(w_unw, here("9_diagnostics", "output", "wealth_D123.csv"))
>>>>>>> c3f72331da13891e251385797078402f9226ba81

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

<<<<<<< HEAD
# write_csv(w_unw, here("9_diagnostics", "output", "wealth_moments.csv"))

# # ---- Single-panel overlay for a chosen Unit/weighting/year set ----------
# plot_scmc_vs_lorenz <- function(curves_df,
#                                 unit = "Household",
#                                 weighting = "Unweighted",
#                                 years = NULL,
#                                 ylim = NULL) {
#   df <- curves_df %>%
#     filter(Unit == unit, weighting == weighting) %>%
#     { if (!is.null(years)) filter(., year %in% years) else . } %>%
#     select(Unit, weighting, year, p, L, M) %>%
#     pivot_longer(cols = c(L, M), names_to = "curve", values_to = "value") %>%
#     mutate(curve = recode(curve, L = "Lorenz  L(u)", M = "SCMC  M(u)=L(u)/u"))

#   ggplot(df, aes(x = p, y = value, color = curve)) +
#     geom_line(linewidth = .8) +
#     # reference guides: diagonal for Lorenz; horizontal 1 for SCMC
#     geom_abline(slope = 1, intercept = 0, linetype = "dotted", linewidth = 0.4, alpha = 0.7) +
#     geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
#     facet_wrap(~ year, scales = "free_y") +
#     coord_cartesian(ylim = ylim) +
#     scale_color_manual(values = c("Lorenz  L(u)" = "#1b9e77", "SCMC  M(u)=L(u)/u" = "#d95f02")) +
#     labs(x = "u (population share, poorest first)",
#          y = "Value",
#          color = NULL,
#          title = sprintf("Lorenz vs. Scaled Conditional Mean Curves — %s (%s)", unit, weighting)) +
#     theme_minimal(base_size = 12) +
#     theme(legend.position = "top")
# }

# # households, weighted, selected years
# p1 <- plot_scmc_vs_lorenz(
#   inc_all,
#   unit = "Household",
#   weighting = "Weighted",
#   years = c(1979, 2019)
# )

# # clans, weighted, selected years
# p2 <- plot_scmc_vs_lorenz(
#   inc_all,
#   unit = "Clan",
#   weighting = "Weighted",
#   years = c(1979, 2019)
# )

# # households, weighted, selected years
# p3 <- plot_scmc_vs_lorenz(
#   w_all,
#   unit = "Household",
#   weighting = "Weighted",
#   years = c(1989, 2019)
# )

# # clans, weighted, selected years
# p4 <- plot_scmc_vs_lorenz(
#   w_all,
#   unit = "Clan",
#   weighting = "Weighted",
#   years = c(1989, 2019)
# )

# # Combine income plots side by side
# inc_curves <- p1 + p2
# ggsave(here("9_diagnostics", "output", "inc_curves.png"),
#        inc_curves, width = 12, height = 6, dpi = 300)

# # Combine wealth plots side by side
# wealth_curves <- p3 + p4
# ggsave(here("9_diagnostics", "output", "wealth_curves.png"),
#        wealth_curves, width = 12, height = 6, dpi = 300)
=======
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
>>>>>>> c3f72331da13891e251385797078402f9226ba81

