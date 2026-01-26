library(here)
library(tidyverse)
library(ineq)
library(glue)
library(officer)
library(flextable)

out_base <- here("10_appendix", "output")
dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

appendix_out <- file.path(out_base, "appendices.docx")
plots_dir <- file.path(out_base, "appendix_plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

income         <- read_gini(here("6_calculate_gini", "output", "income.csv"))
wealth_nohouse <- read_gini(here("6_calculate_gini", "output", "wealth_nohouse.csv"))
wealth_withhome<- read_gini(here("6_calculate_gini", "output", "wealth_withhome.csv"))

doc <- read_docx()

# APPENDIX A
# Run simulation
set.seed(123)
sims <- run_many_sims_sums(
  n_sims = 100,
  n_clans = 5,
  hh_per_clan = 10,
  dist = "lognormal",
  dist_params = list(meanlog = 0, sdlog = 1),
  clan_size_scenario = "rich_big"
)

df_res <- results_from_sims_sums(sims)
ext <- extreme_indices_sums(df_res)

# Build plots
p_lorenz_low  <- plot_lorenz_for_sim_sums(sims[[ext$idx_min_sums]])
p_lorenz_high <- plot_lorenz_for_sim_sums(sims[[ext$idx_max_sums]])
p_mekko_two   <- make_two_mekko_sums(sims, ext$idx_min_sums, ext$idx_max_sums)

# Save plots
lorenz_low_png  <- file.path(plots_dir, "appendixA_lorenz_low.png")
lorenz_high_png <- file.path(plots_dir, "appendixA_lorenz_high.png")
mekko_png       <- file.path(plots_dir, "appendixA_mekko.png")

write_plot_png(p_lorenz_low,  lorenz_low_png,  width = 6.5, height = 4.5)
write_plot_png(p_lorenz_high, lorenz_high_png, width = 6.5, height = 4.5)
write_plot_png(p_mekko_two,   mekko_png,       width = 6.5, height = 6.0)

# Write Appendix A
doc <- add_heading(doc, "Appendix A: Extreme Cases Lorenz Curves", level = 1)
doc <- add_heading(doc, "A1.1: Lowest simulated difference", level = 2)
doc <- add_img(doc, lorenz_low_png, width = 6.5, height = 4.5)
doc <- add_spacer(doc)

doc <- add_heading(doc, "A1.2: Highest simulated difference", level = 2)
doc <- add_img(doc, lorenz_high_png, width = 6.5, height = 4.5)
doc <- add_spacer(doc)

doc <- add_heading(doc, "Appendix A2: Extreme Case Summaries", level = 1)
doc <- add_img(doc, mekko_png, width = 6.5, height = 6.0)
doc <- add_spacer(doc)

# APPENDIX B
doc <- add_heading(doc, "Appendix B", level = 1)

# Grab Ginis from years where external estimates are available
# Use Ginis from full sample - all households
income_path  <- "/Users/amanda/Desktop/clan_project/6_calculate_gini/output/income.csv"
wealth_path  <- "/Users/amanda/Desktop/clan_project/6_calculate_gini/output/wealth_nohouse.csv"

income_all   <- readr::read_csv(income_path, show_col_types = FALSE) |>
  dplyr::mutate(year = suppressWarnings(as.integer(as.character(year))))

wealth_all   <- readr::read_csv(wealth_path, show_col_types = FALSE) |>
  dplyr::mutate(year = suppressWarnings(as.integer(as.character(year))))

get_our_gini <- function(df, year_pick, col_name) {
  val <- df |>
    dplyr::filter(year == year_pick) |>
    dplyr::pull({{ col_name }})
  if (length(val) == 0) return(NA_real_)
  as.numeric(val[1])
}

# Table B1 (Income) 
b1_years <- c(1969L, 1979L, 1989L, 1999L, 2009L, 2019L)

b1_hard <- tibble::tibble(
  Year       = b1_years,
  FRED       = c(0.391, 0.404, 0.431, 0.458, 0.468, 0.484),
  `U.S. Census` = c(NA, NA, NA, 0.460, 0.470, 0.480)
)

b1_our <- income_all |>
  dplyr::filter(year %in% b1_years) |>
  dplyr::select(year, hh_w_inc) |>
  dplyr::rename(Year = year, `Our Results (All HH)` = hh_w_inc)

b1 <- b1_hard |>
  dplyr::left_join(b1_our, by = "Year") |>
  dplyr::mutate(
    dplyr::across(c(FRED, `U.S. Census`, `Our Results (All HH)`), ~ round(as.numeric(.x), 3))
  ) |>
  dplyr::arrange(Year)

ft_b1 <- flextable::flextable(b1) |>
  flextable::set_header_labels(
    Year = "Year",
    FRED = "FRED",
    `U.S. Census` = "U.S. Census",
    `Our Results (All HH)` = "Our Results\n(All HH)"
  ) |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::autofit()

doc <- officer::body_add_par(
  doc,
  "Table B1: Comparison of Gini coefficient for income with other Gini coefficients in available studies",
  style = "Normal"
)
doc <- officer::body_add_par(doc, "", style = "Normal")
doc <- flextable::body_add_flextable(doc, value = ft_b1)
doc <- officer::body_add_par(doc, "Sources: FRED data and Census data", style = "Normal")
doc <- add_spacer(doc)

# Table B2 (Wealth)
b2_rows <- tibble::tibble(
  Year_label = c("1989", "1998 / 1999", "2003", "2007", "2009", "2011", "2021 / 2022"),
  Year_pick  = c(1989L, 1999L, 2003L, 2007L, 2009L, 2011L, 2022L),
  `Gini from the SCF or PSID` = c(0.8321, 0.8032, 0.8130, 0.8330, 0.8930, 0.8830, 0.8340)
)

b2_our <- wealth_all |>
  dplyr::filter(year %in% b2_rows$Year_pick) |>
  dplyr::select(year, hh_w_wealth) |>
  dplyr::rename(Year_pick = year, `Our Results (All HH)` = hh_w_wealth)

b2 <- b2_rows |>
  dplyr::left_join(b2_our, by = "Year_pick") |>
  dplyr::transmute(
    Year = Year_label,
    `Gini from the SCF or PSID` = round(as.numeric(`Gini from the SCF or PSID`), 3),
    `Our Results (All HH)`      = round(as.numeric(`Our Results (All HH)`), 3)
  )

ft_b2 <- flextable::flextable(b2) |>
  flextable::set_header_labels(
    Year = "Year",
    `Gini from the SCF or PSID` = "Gini from the SCF or PSID",
    `Our Results (All HH)`      = "Our Results\n(All HH)"
  ) |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::autofit()

doc <- officer::body_add_par(
  doc,
  "Table B2: Comparison of Gini coefficient for wealth with other Gini coefficients in available studies",
  style = "Normal"
)
doc <- officer::body_add_par(doc, "", style = "Normal")
doc <- flextable::body_add_flextable(doc, value = ft_b2)
doc <- officer::body_add_par(
  doc,
  "Sources: SCF Wolff (2006); SCF Budria et al. (2002); PSID Pfeffer et al. (2013); SCF Rios-Rull and Kuhn (2025)",
  style = "Normal"
)
doc <- add_spacer(doc)


# APPENDIX C
doc <- add_heading(doc, "Appendix C", level = 1)

doc <- make_two_panel_table(
  doc = doc,
  df = income,
  title = "Table C1: Comparison of Gini coefficients with and without negative values",
  year_col = year,
  left_hh = r_hh_w_inc,
  left_clans = r_cl_w_inc,
  right_hh = neg_r_hh_inc,
  right_clans = neg_r_cl_inc,
  left_label = "Excluding Negative Values",
  right_label = "Including Negative Values"
)
doc <- add_spacer(doc)

doc <- make_two_panel_table(
  doc = doc,
  df = wealth_nohouse,
  title = "Table C2: Comparison of Gini coefficients with and without negative values (wealth excludes home equity)",
  year_col = year,
  left_hh = r_hh_w_wealth,
  left_clans = r_cl_w_wealth,
  right_hh = neg_r_hh_wealth,
  right_clans = neg_r_cl_wealth,
  left_label = "Excluding Negative Values",
  right_label = "Including Negative Values"
)
doc <- add_spacer(doc)

# APPENDIX D
doc <- add_heading(doc, "Appendix D", level = 1)

appendix_d_df <- wealth_nohouse |>
  select(
    year,
    r_hh_w_wealth_nohouse = r_hh_w_wealth,
    r_cl_w_wealth_nohouse = r_cl_w_wealth
  ) |>
  full_join(
    wealth_withhome |>
      select(
        year,
        r_hh_w_wealth_withhome = r_hh_w_wealth,
        r_cl_w_wealth_withhome = r_cl_w_wealth
      ),
    by = "year"
  )

doc <- make_two_panel_table(
  doc = doc,
  df = appendix_d_df,
  title = "Table D1: Comparison of Gini coefficients for wealth with and without home equity",
  year_col = year,
  left_hh = r_hh_w_wealth_nohouse,
  left_clans = r_cl_w_wealth_nohouse,
  right_hh = r_hh_w_wealth_withhome,
  right_clans = r_cl_w_wealth_withhome,
  left_label = "Excluding Home Equity",
  right_label = "Including Home Equity",
  overall_label = "Difference Overall"
)
doc <- add_spacer(doc)

# APPENDIX E
doc <- add_heading(doc, "Appendix E", level = 1)

doc <- make_two_panel_table(
  doc = doc,
  df = income,
  title = "Table E1: Comparison of Gini coefficients with and without single-household clans",
  year_col = year,
  left_hh = hh_w_inc,
  left_clans = cl_w_inc,
  right_hh = r_hh_w_inc,
  right_clans = r_cl_w_inc,
  left_label = "Including Single-HH Clans",
  right_label = "Excluding Single-HH Clans (Robust)"
)
doc <- add_spacer(doc)

doc <- make_two_panel_table(
  doc = doc,
  df = wealth_nohouse,
  title = "Table E2: Comparison of Gini coefficients with and without single-household clans (wealth excludes home equity)",
  year_col = year,
  left_hh = hh_w_wealth,
  left_clans = cl_w_wealth,
  right_hh = r_hh_w_wealth,
  right_clans = r_cl_w_wealth,
  left_label = "Including Single-HH Clans",
  right_label = "Excluding Single-HH Clans (Robust)"
)
doc <- add_spacer(doc)

# APPENDIX F
doc <- add_heading(doc, "Appendix F", level = 1)

need_inc_cols <- c("r_hh_w_inc", "r_cl_w_inc", "r_hh_unw_inc", "r_cl_unw_inc")
if (all(need_inc_cols %in% names(income))) {
  doc <- make_two_panel_table(
    doc = doc,
    df = income,
    title = "Table F1: Comparison of Gini coefficients for income with and without weights",
    year_col = year,
    left_hh = r_hh_unw_inc,
    left_clans = r_cl_unw_inc,
    right_hh = r_hh_w_inc,
    right_clans = r_cl_w_inc,
    left_label = "Unweighted",
    right_label = "Weighted"
  )
  doc <- add_spacer(doc)
}

need_w_cols <- c("r_hh_w_wealth", "r_cl_w_wealth", "r_hh_unw_wealth", "r_cl_unw_wealth")
if (all(need_w_cols %in% names(wealth_nohouse))) {
  doc <- make_two_panel_table(
    doc = doc,
    df = wealth_nohouse,
    title = "Table F2: Comparison of Gini coefficients for wealth with and without weights",
    year_col = year,
    left_hh = r_hh_unw_wealth,
    left_clans = r_cl_unw_wealth,
    right_hh = r_hh_w_wealth,
    right_clans = r_cl_w_wealth,
    left_label = "Unweighted",
    right_label = "Weighted"
  )
  doc <- add_spacer(doc)
}

# APPENDIX G
doc <- add_heading(doc, "Appendix G", level = 1)

c123_dir <- here("8_nuclear_family", "output")
inc_path <- file.path(c123_dir, "income_C123.csv")
wnh_path <- file.path(c123_dir, "wealth_nohouse_C123.csv")

inc <- read_csv(inc_path, show_col_types = FALSE)
wnh <- read_csv(wnh_path, show_col_types = FALSE)

get_all_row <- function(df) {
  df |>
    filter(year == "ALL") |>
    select(ends_with("_hh"), ends_with("_clan")) |>
    distinct() |>
    slice(1)
}

inc_all <- get_all_row(inc)
wnh_all <- get_all_row(wnh)

C123_tbl <- bind_rows(
  inc_all |> mutate(Measure = "Income"),
  wnh_all |> mutate(Measure = "Wealth (No House)")
) |>
  select(
    Measure,
    C1_hh, C1_clan,
    C2_hh, C2_clan,
    C3_hh, C3_clan
  )

ft <- flextable(C123_tbl)
ft <- add_header_row(ft, values = c("", "C1", "C2", "C3"), colwidths = c(1, 2, 2, 2))
ft <- set_header_labels(
  ft,
  Measure = "",
  C1_hh = "HH", C1_clan = "Clans",
  C2_hh = "HH", C2_clan = "Clans",
  C3_hh = "HH", C3_clan = "Clans"
)
ft <- theme_booktabs(ft)
ft <- bold(ft, part = "header")
ft <- align(ft, align = "center", part = "all")
ft <- autofit(ft)

doc <- body_add_par(doc, "Table G1: Average nuclear family indices (C1, C2, C3) for income and wealth", style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft)

print(doc, target = appendix_out)
message("Saved: ", appendix_out)
