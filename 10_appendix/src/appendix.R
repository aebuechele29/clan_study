library(here)
library(dplyr)
library(readr)
library(officer)
library(flextable)
library(rlang)
library(tidyverse)
library(ineq)
library(glue)
library(rvg)

out_base <- here("10_appendix", "output")
dir.create(out_base, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(out_base, full.names = TRUE, recursive = TRUE, include.dirs = FALSE), force = TRUE)

# Set output
dirs <- c("appendix_a", "appendix_b", "appendix_c", "appendix_d", "appendix_e", "appendix_f", "appendix_g")
invisible(lapply(file.path(out_base, dirs), dir.create, recursive = TRUE, showWarnings = FALSE))


# Helper function: read gini CSV, drop ALL row, sort by year
read_gini <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    filter(year != "ALL") %>%
    mutate(year = as.integer(year)) %>%
    arrange(year)
}

add_gg <- function(doc, gg, width = 6.5, height = 4.5) {
  body_add(doc, rvg::dml(ggobj = gg), width = width, height = height)
}

# Load data
income <- read_gini(here("6_calculate_gini", "output", "income.csv"))
wealth_nohouse <- read_gini(here("6_calculate_gini", "output", "wealth_nohouse.csv"))
wealth_withhome <- read_gini(here("6_calculate_gini", "output", "wealth_withhome.csv"))

# APPENDIX A: Simulation results

# (A1) Run sims (your sums-only simulation code should already be defined above)
set.seed(123)

sims <- run_many_sims(
  n_sims = 100,
  n_clans = 5,
  hh_per_clan = 10,
  dist = "lognormal",
  dist_params = list(meanlog = 0, sdlog = 1),
  clan_size_scenario = "rich_big"
)

df_res <- results_from_sims(sims)
ext <- extreme_indices(df_res)

# (A2) Build plots
p_sums_min <- plot_lorenz_for_sim(
  sims[[ext$idx_min_sums]],
  title_prefix = sprintf("Lowest diff (household − clan sums) [sim %d]", ext$idx_min_sums)
)

p_sums_max <- plot_lorenz_for_sim(
  sims[[ext$idx_max_sums]],
  title_prefix = sprintf("Highest diff (household − clan sums) [sim %d]", ext$idx_max_sums)
)

p_mekko_two <- make_two_mekko(sims, df_res, ext$idx_min_sums, ext$idx_max_sums)

# (A3) Export to docx (match appendix style + paths)
appendix_a_out <- file.path(out_base, "appendix_a", "appendix_a.docx")

doc <- read_docx()

doc <- doc |>
  body_add_par("Appendix A: Simulation results", style = "heading 1") |>
  body_add_par(
    "This appendix illustrates that clan-level inequality (using clan sums) can be lower or higher than household-level inequality depending on how clan size relates to the underlying distribution.",
    style = "Normal"
  ) |>
  body_add_par("Lorenz curves — extreme cases (clan sums)", style = "heading 2") |>
  body_add_par(sprintf("Lowest difference (sim %d)", ext$idx_min_sums), style = "heading 3") |>
  body_add(value = dml(ggobj = p_sums_min), width = 6.5, height = 4.5) |>
  body_add_par(sprintf("Highest difference (sim %d)", ext$idx_max_sums), style = "heading 3") |>
  body_add(value = dml(ggobj = p_sums_max), width = 6.5, height = 4.5) |>
  body_add_par("Mekko summaries — extreme cases (clan sums)", style = "heading 2") |>
  body_add(value = dml(ggobj = p_mekko_two), width = 6.5, height = 6)

print(doc, target = appendix_a_out)
message("Saved: ", appendix_a_out)


# APPENDIX B: Compares Gini coefficients with other studies

# APPENDIX C: Compare Gini coefficients when using negative values for income and wealth
# Uses: baseline robust vs neg robust
# Income 
appendix_c_income <- income %>%
  transmute(
    year,
    r_hh_w_inc,
    neg_r_hh_inc,
    r_cl_w_inc,
    neg_r_cl_inc,
    diff_hh = neg_r_hh_inc - r_hh_w_inc,
    diff_cl = neg_r_cl_inc - r_cl_w_inc
  )

# write_csv(appendix_c_income,
#           file.path(out_base, "appendix_c", "income.csv"))

# Income
make_two_panel_table(
  df          = income,
  title       = "Table C1: Comparison of Gini coefficients with and without negative values",
  outfile     = file.path(out_base, "appendix_c", "income_negatives.docx"),
  year_col    = year,
  left_hh     = r_hh_w_inc,
  left_clans  = r_cl_w_inc,
  right_hh    = neg_r_hh_inc,
  right_clans = neg_r_cl_inc,
  left_label  = "Excluding Negative Values",
  right_label = "Including Negative Values"
)

# Wealth (excluding home equity)
make_two_panel_table(
  df          = wealth_nohouse,
  title       = "Table C2: Comparison of Gini coefficients with and without negative values (wealth excludes home equity)",
  outfile     = file.path(out_base, "appendix_c", "wealth_negatives.docx"),
  year_col    = year,
  left_hh     = r_hh_w_wealth,
  left_clans  = r_cl_w_wealth,
  right_hh    = neg_r_hh_wealth,
  right_clans = neg_r_cl_wealth,
  left_label  = "Excluding Negative Values",
  right_label = "Including Negative Values"
)


# APPENDIX D: Compare wealth measures with and without home equity
appendix_d_wealth_measures <- wealth_nohouse %>%
  select(year,
         r_hh_w_wealth_nohouse = r_hh_w_wealth,
         r_cl_w_wealth_nohouse = r_cl_w_wealth) %>%
  full_join(
    wealth_withhome %>%
      select(year,
             r_hh_w_wealth_withhome = r_hh_w_wealth,
             r_cl_w_wealth_withhome = r_cl_w_wealth),
    by = "year"
  ) %>%
  mutate(
    diff_nohouse  = r_hh_w_wealth_nohouse  - r_cl_w_wealth_nohouse,
    diff_withhome = r_hh_w_wealth_withhome - r_cl_w_wealth_withhome,
    diff_home_equity_effect = diff_withhome - diff_nohouse
  ) %>%
  arrange(year)

write_csv(appendix_d_wealth_measures,
          file.path(out_base, "appendix_d", "wealth_compare.csv"))

appendix_d_df <- wealth_nohouse %>%
  select(year,
         r_hh_w_wealth_nohouse = r_hh_w_wealth,
         r_cl_w_wealth_nohouse = r_cl_w_wealth) %>%
  full_join(
    wealth_withhome %>%
      select(year,
             r_hh_w_wealth_withhome = r_hh_w_wealth,
             r_cl_w_wealth_withhome = r_cl_w_wealth),
    by = "year"
  )

make_two_panel_table(
  df         = appendix_d_df,
  title      = "Table D1: Comparison of Gini coefficients for wealth with and without home equity",
  outfile    = file.path(out_base, "appendix_d", "wealth_compare.docx"),
  year_col   = year,
  left_hh    = r_hh_w_wealth_nohouse,
  left_clans = r_cl_w_wealth_nohouse,
  right_hh   = r_hh_w_wealth_withhome,
  right_clans= r_cl_w_wealth_withhome,
  left_label = "Excluding Home Equity",
  right_label= "Including Home Equity"
)



# APPENDIX E: Compare samples with vs without single-HH clans
# Income
appendix_e_income <- income %>%
  transmute(
    year,
    hh_w_inc,
    r_hh_w_inc,
    cl_w_inc,
    r_cl_w_inc,
    hh_change = r_hh_w_inc - hh_w_inc,
    cl_change = r_cl_w_inc - cl_w_inc,
    diff_change = (r_hh_w_inc - r_cl_w_inc) - (hh_w_inc - cl_w_inc)
  )

write_csv(appendix_e_income,
          file.path(out_base, "appendix_e", "income.csv"))

# Wealth
appendix_e_wealth_nohouse <- wealth_nohouse %>%
  transmute(
    year,
    hh_w_wealth,
    r_hh_w_wealth,
    cl_w_wealth,
    r_cl_w_wealth,
    hh_change = r_hh_w_wealth - hh_w_wealth,
    cl_change = r_cl_w_wealth - cl_w_wealth,
    diff_change = (r_hh_w_wealth - r_cl_w_wealth) - (hh_w_wealth - cl_w_wealth)
  )

write_csv(appendix_e_wealth_nohouse,
          file.path(out_base, "appendix_e", "wealth.csv"))


make_two_panel_table(
  df         = income,
  title      = "Table E1: Comparison of Gini coefficients with and without single-household clans",
  outfile    = file.path(out_base, "appendix_e", "income_sample.docx"),
  year_col   = year,
  left_hh    = hh_w_inc,
  left_clans = cl_w_inc,
  right_hh   = r_hh_w_inc,
  right_clans= r_cl_w_inc,
  left_label = "Including Single-HH Clans",
  right_label= "Excluding Single-HH Clans (Robust)"
)

make_two_panel_table(
  df         = wealth_nohouse,
  title      = "Table E2: Comparison of Gini coefficients with and without single-household clans (wealth excludes home equity)",
  outfile    = file.path(out_base, "appendix_e", "wealth_sample.docx"),
  year_col   = year,
  left_hh    = hh_w_wealth,
  left_clans = cl_w_wealth,
  right_hh   = r_hh_w_wealth,
  right_clans= r_cl_w_wealth,
  left_label = "Including Single-HH Clans",
  right_label= "Excluding Single-HH Clans (Robust)"
)


# APPENDIX F: Compare Ginis with vs without weights
# Income
need_inc_cols <- c("r_hh_w_inc", "r_cl_w_inc", "r_hh_unw_inc", "r_cl_unw_inc")
missing_inc <- setdiff(need_inc_cols, names(income))

if (length(missing_inc) > 0) {
  message("Appendix F income skipped (missing columns): ", paste(missing_inc, collapse = ", "))
} else {
  appendix_f_income <- income %>%
    transmute(
      year,
      r_hh_unw_inc,
      r_hh_w_inc,
      r_cl_unw_inc,
      r_cl_w_inc,
      hh_weight_effect = r_hh_w_inc - r_hh_unw_inc,
      cl_weight_effect = r_cl_w_inc - r_cl_unw_inc
    )
  write_csv(appendix_f_income,
            file.path(out_base, "appendix_f", "income.csv"))
}

# Wealth
need_w_cols <- c("r_hh_w_wealth", "r_cl_w_wealth", "r_hh_unw_wealth", "r_cl_unw_wealth")
missing_w <- setdiff(need_w_cols, names(wealth_nohouse))

if (length(missing_w) > 0) {
  message("Appendix F wealth_nohouse skipped (missing columns): ", paste(missing_w, collapse = ", "))
} else {
  appendix_f_wealth_nohouse <- wealth_nohouse %>%
    transmute(
      year,
      r_hh_unw_wealth,
      r_hh_w_wealth,
      r_cl_unw_wealth,
      r_cl_w_wealth,
      hh_weight_effect = r_hh_w_wealth - r_hh_unw_wealth,
      cl_weight_effect = r_cl_w_wealth - r_cl_unw_wealth
    )
  write_csv(appendix_f_wealth_nohouse,
            file.path(out_base, "appendix_f", "wealth.csv"))
}

# Income (robust) — weighted vs unweighted
need_inc_cols <- c("r_hh_w_inc", "r_cl_w_inc", "r_hh_unw_inc", "r_cl_unw_inc")
missing_inc <- setdiff(need_inc_cols, names(income))

if (length(missing_inc) > 0) {
  message("Appendix F income skipped (missing columns): ", paste(missing_inc, collapse = ", "))
} else {
  make_two_panel_table(
    df         = income,
    title      = "Table F1: Comparison of Gini coefficients for income with and without weights",
    outfile    = file.path(out_base, "appendix_f", "income_weights.docx"),
    year_col   = year,
    left_hh    = r_hh_unw_inc,
    left_clans = r_cl_unw_inc,
    right_hh   = r_hh_w_inc,
    right_clans= r_cl_w_inc,
    left_label = "Unweighted",
    right_label= "Weighted"
  )
}

# Wealth no-house (robust) — weighted vs unweighted
need_w_cols <- c("r_hh_w_wealth", "r_cl_w_wealth", "r_hh_unw_wealth", "r_cl_unw_wealth")
missing_w <- setdiff(need_w_cols, names(wealth_nohouse))

if (length(missing_w) > 0) {
  message("Appendix F wealth skipped (missing columns): ", paste(missing_w, collapse = ", "))
} else {
  make_two_panel_table(
    df         = wealth_nohouse,
    title      = "Table F2: Comparison of Gini coefficients for wealth with and without weights",
    outfile    = file.path(out_base, "appendix_f", "wealth_weights.docx"),
    year_col   = year,
    left_hh    = r_hh_unw_wealth,
    left_clans = r_cl_unw_wealth,
    right_hh   = r_hh_w_wealth,
    right_clans= r_cl_w_wealth,
    left_label = "Unweighted",
    right_label= "Weighted"
  )
}

# APPENDIX G: Nuclear family indices (C1, C2, C3)
c123_dir <- here("8_nuclear_family", "output")

inc_path <- file.path(c123_dir, "income_C123.csv")
wnh_path <- file.path(c123_dir, "wealth_nohouse_C123.csv")

inc <- read_csv(inc_path, show_col_types = FALSE)
wnh <- read_csv(wnh_path, show_col_types = FALSE)

get_all_row <- function(df) {
  df %>%
    filter(year == "ALL") %>%
    select(ends_with("_hh"), ends_with("_clan")) %>%
    distinct() %>%
    slice(1)
}

inc_all <- get_all_row(inc)
wnh_all <- get_all_row(wnh)

C123_tbl <- bind_rows(
  inc_all %>% mutate(Measure = "Income"),
  wnh_all %>% mutate(Measure = "Wealth (No House)")
)

C123_tbl <- C123_tbl %>%
  select(
    Measure,
    C1_hh, C1_clan,
    C2_hh, C2_clan,
    C3_hh, C3_clan
  )

ft <- flextable(C123_tbl)

# Top header row: C1 / C2 / C3 groups
ft <- add_header_row(
  ft,
  values    = c("", "C1", "C2", "C3"),
  colwidths = c(1, 2, 2, 2)
)

# Second header row: HH / Clans within each measure group
ft <- set_header_labels(
  ft,
  Measure = "",
  C1_hh = "HH",   C1_clan = "Clans",
  C2_hh = "HH",   C2_clan = "Clans",
  C3_hh = "HH",   C3_clan = "Clans"
)

ft <- theme_booktabs(ft)
ft <- bold(ft, part = "header")
ft <- align(ft, align = "center", part = "all")
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_par(
  doc,
  "Table G1: Average nuclear family indices (C1, C2, C3) for income and wealth",
  style = "heading 2"
)
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft)

print(doc, target = file.path(out_base, "appendix_g", "c123.docx"))