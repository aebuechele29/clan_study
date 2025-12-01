library(tidyverse)
library(flextable)
library(officer)
library(here)

## 1. Load data ----
inc <- read_csv("6_calculate_gini/output/race_ginis/income_race.csv")
wealth <- read_csv("6_calculate_gini/output/race_ginis/wealth_withhome_race.csv")
# if you actually want wealth excluding home equity, swap to wealth_nohouse instead

## 2. Build comparison table ----
income_row <- inc %>%
  summarise(
    black_hh       = mean(r_hh_w_inc_black,       na.rm = TRUE),
    black_clan     = mean(r_cl_w_inc_black,       na.rm = TRUE),
    black_diff     = black_hh - black_clan,
    nonblack_hh    = mean(r_hh_w_inc_nonblack,    na.rm = TRUE),
    nonblack_clan  = mean(r_cl_w_inc_nonblack,    na.rm = TRUE),
    nonblack_diff  = nonblack_hh - nonblack_clan
  ) %>%
  mutate(type = "Income")

wealth_row <- wealth %>%
  summarise(
    black_hh       = mean(r_hh_w_wealth_black,       na.rm = TRUE),
    black_clan     = mean(r_cl_w_wealth_black,       na.rm = TRUE),
    black_diff     = black_hh - black_clan,
    nonblack_hh    = mean(r_hh_w_wealth_nonblack,    na.rm = TRUE),
    nonblack_clan  = mean(r_cl_w_wealth_nonblack,    na.rm = TRUE),
    nonblack_diff  = nonblack_hh - nonblack_clan
  ) %>%
  mutate(type = "Wealth")

comparison_table <- bind_rows(income_row, wealth_row) %>%
  select(type, black_hh, black_clan, black_diff,
         nonblack_hh, nonblack_clan, nonblack_diff) %>%
  rename(
    Measure        = type,
    `Black HH`     = black_hh,
    `Black clans`  = black_clan,
    `Black diff`   = black_diff,
    `Non-Black HH`    = nonblack_hh,
    `Non-Black clans` = nonblack_clan,
    `Non-Black diff`  = nonblack_diff
  )

comparison_table <- comparison_table %>%
  mutate(across(-Measure, ~ round(.x, 3)))

## 3. Flextable + DOCX export ----

# optional: a style for the note text
note_style <- fp_text(font.size = 9)


ft <- flextable(comparison_table) |>
  set_caption("Figure 4: Differences in Inequality by Race") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(
      ftext(
        "Note: Gini coefficients and distributions based on weighted data using PSID family and clan weights. Wealth is measured exclusive of home equity. ",
        prop = note_style
      ),
      fp_p = fp_par(text.align = "center")
    )
  )

print(
  doc,
  target = here("X_figures", "output", "figure4", "Figure4_race.docx")
)

