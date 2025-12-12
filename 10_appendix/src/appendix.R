library(here)
library(dplyr)
library(readr)
library(officer)
library(flextable)
library(rlang)

# Clear all previous files
unlink(list.files(out_base, full.names = TRUE, recursive = TRUE, include.dirs = FALSE), force = TRUE)

# Set output
out_base <- here("10_appendix", "output")
dirs <- c("appendix_a", "appendix_b", "appendix_c", "appendix_d", "appendix_e", "appendix_f", "appendix_g")
invisible(lapply(file.path(out_base, dirs), dir.create, recursive = TRUE, showWarnings = FALSE))


# Helper function: read gini CSV, drop ALL row, sort by year
read_gini <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    filter(year != "ALL") %>%
    mutate(year = as.integer(year)) %>%
    arrange(year)
}

# Load data
income <- read_gini(here("6_calculate_gini", "output", "income.csv"))
wealth_nohouse <- read_gini(here("6_calculate_gini", "output", "wealth_nohouse.csv"))
wealth_withhome <- read_gini(here("6_calculate_gini", "output", "wealth_withhome.csv"))

# APPENDIX A: Simulation results

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

C123_tbl <- C123_tbl %>%
  select(
    Measure,
    C1_HH, C1_Clan,
    C2_HH, C2_Clan,
    C3_HH, C3_Clan
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
  C1_HH = "HH",   C1_Clan = "Clans",
  C2_HH = "HH",   C2_Clan = "Clans",
  C3_HH = "HH",   C3_Clan = "Clans"
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