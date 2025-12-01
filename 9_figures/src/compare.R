library(here)
library(dplyr)
library(readr)
library(flextable)
library(officer)

# Helper function 
make_docx_table <- function(df, caption, file_name, note = NULL) {
  ft <- flextable(df) |>
    set_caption(caption) |>
    autofit() |>
    theme_vanilla() |>
    fontsize(size = 10, part = "all") |>
    align(align = "center", part = "all")

  doc <- read_docx() |>
    body_add_flextable(ft)

  if (!is.null(note)) {
    doc <- doc |>
      body_add_par(note, style = "Normal")
  }

  print(doc, target = here("9_figures", "output", "compare", file_name))
}

# COMPARE: BASE vs "WITH NEGATIVE VALUES" -----------------------------------
# Base files (all calculations set negative values for income and wealth to 0)
inc_base <- read_csv(here("6_calculate_gini", "output", "income.csv"))
w_base   <- read_csv(here("6_calculate_gini", "output", "wealth_nohouse.csv"))

# Files with negative values
inc_neg <- read_csv(here("6_calculate_gini", "output", "compare_neg_vals", "income.csv"))
w_neg   <- read_csv(here("6_calculate_gini", "output", "compare_neg_vals", "wealth_nohouse.csv"))

# Income
inc_neg_compare <- inc_base |>
  select(
    year,
    Base_HH    = r_hh_w_inc,
    Base_Clans = r_cl_w_inc
  ) |>
  left_join(
    inc_neg |>
      select(
        year,
        With_Neg_HH    = r_hh_w_inc,
        With_Neg_Clans = r_cl_w_inc
      ),
    by = "year"
  ) |>
  mutate(
    Base_Diff        = Base_HH    - Base_Clans,        # HH - Clans (base spec)
    Neg_Diff         = With_Neg_HH - With_Neg_Clans,   # HH - Clans (neg-values spec)
    Diff_Between_Diffs = Base_Diff - Neg_Diff          # (base gap) - (neg gap)
  ) |>
  relocate(
    year,
    Base_HH, Base_Clans, Base_Diff,
    With_Neg_HH, With_Neg_Clans, Neg_Diff,
    Diff_Between_Diffs,
    .before = everything()
  ) |>
  rename(Year = year)


# Wealth
w_neg_compare <- w_base |>
  select(
    year,
    Base_HH    = r_hh_w_wealth,
    Base_Clans = r_cl_w_wealth
  ) |>
  left_join(
    w_neg |>
      select(
        year,
        With_Neg_HH    = r_hh_w_wealth,
        With_Neg_Clans = r_cl_w_wealth
      ),
    by = "year"
  ) |>
  mutate(
    Base_Diff          = Base_HH    - Base_Clans,
    Neg_Diff           = With_Neg_HH - With_Neg_Clans,
    Diff_Between_Diffs = Base_Diff - Neg_Diff
  ) |>
  relocate(
    year,
    Base_HH, Base_Clans, Base_Diff,
    With_Neg_HH, With_Neg_Clans, Neg_Diff,
    Diff_Between_Diffs,
    .before = everything()
  ) |>
  rename(Year = year)

make_docx_table(w_neg_compare, "Wealth (No Home Equity): Base vs Negatives", "negvals_wealth_nohouse.docx")
make_docx_table(inc_neg_compare, "Income: Base vs Negatives", "negvals_income.docx")


# COMPARE: FULL vs ROBUST (single-HH clans removed) -------------------------
# Income
inc_r <- read_csv(here("6_calculate_gini", "output", "compare_r", "income.csv"))

inc_r_table <- inc_r |>
  transmute(
    Year                 = year,
    `All HH`             = hh_w_inc,
    `All Clans`          = cl_w_inc,
    `Diff (All)`         = all_hh_diff,
    `Robust HH`          = r_hh_w_inc,
    `Robust Clans`       = r_cl_w_inc,
    `Diff (Robust)`      = robust_hh_diff,
    `Diff Between Samples` = diff_samples
  )

make_docx_table(
  inc_r_table,
  caption   = "Income Ginis: All vs. Robust Sample (Excluding Single-HH Clans)",
  file_name = "compare_r_income.docx"
)

# Wealth_nohouse
w_r <- read_csv(here("6_calculate_gini", "output", "compare_r", "wealth_nohouse.csv"))

w_r_table <- w_r |>
  transmute(
    Year                 = year,
    `All HH`             = hh_w_wealth,
    `All Clans`          = cl_w_wealth,
    `Diff (All)`         = all_hh_diff,
    `Robust HH`          = r_hh_w_wealth,
    `Robust Clans`       = r_cl_w_wealth,
    `Diff (Robust)`      = robust_hh_diff,
    `Diff Between Samples` = diff_samples
  )

make_docx_table(
  w_r_table,
  caption   = "Wealth (No Home Equity) Ginis: All vs. Robust Sample (Excluding Single-HH Clans)",
  file_name = "compare_r_wealth_nohouse.docx"
)

# COMPARE WEALTH DEFINITIONS: NO HOME vs WITH HOME EQUITY -------------------
comp_wealth <- read_csv(
  here("6_calculate_gini", "output", "compare_wealth", "wealth_ginis_comparison.csv")
)

comp_wealth_table <- comp_wealth |>
  transmute(
    Year                               = year,
    `HH Wealth No Home Equity`        = r_hh_w_wealth_nohouse,
    `Clan Wealth No Home Equity`      = r_cl_w_wealth_nohouse,
    `Diff (No Home Equity)`           = diff_nohouse,
    `HH Wealth With Home Equity`      = r_hh_w_wealth_withhome,
    `Clan Wealth With Home Equity`    = r_cl_w_wealth_withhome,
    `Diff (With Home Equity)`         = diff_withhome,
    `Diff: (No Home – With Home)`     = diff_wealth
  )

make_docx_table(
  comp_wealth_table,
  caption   = "Wealth Ginis: Comparing Definitions With and Without Home Equity",
  file_name = "compare_wealth_measure.docx"
)
