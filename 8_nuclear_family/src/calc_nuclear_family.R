library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(flextable)
library(officer)

# Load data
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

# Output paths
out_dir  <- here("8_nuclear_family", "output", "all_nuclear_family")
plot_dir <- here("8_nuclear_family", "output", "plots")
dir.create(out_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Adds an "ALL" row = mean across years for each numeric column.
join_hh_clan <- function(hh_df, clan_df, var_label) {
  out <- hh_df %>%
    rename_with(~ paste0(.x, "_hh"), starts_with("C")) %>%
    full_join(
      clan_df %>% rename_with(~ paste0(.x, "_clan"), starts_with("C")),
      by = "year"
    ) %>%
    arrange(year) %>%
    mutate(year = as.character(year))

  all_row <- out %>%
    filter(year != "ALL") %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(year = "ALL", .before = 1)

  out <- bind_rows(out, all_row)

  write_csv(out, file.path(out_dir, paste0(var_label, "_C123.csv")))
  out
}

# Compute C123 by year
# Income
hh_inc   <- C123_by_year(r_hh, value_var = inc_all,  weight = TRUE, weight_var = "fam_weight")
clan_inc <- C123_by_year(r_clans, value_var = inc_all, weight = TRUE, weight_var = "clan_weight")
inc      <- join_hh_clan(hh_inc, clan_inc, "income")

# Wealth (no home equity)
hh_wnh   <- C123_by_year(r_hh_wealth, value_var = wealth_nohouse, weight = TRUE, weight_var = "fam_weight")
clan_wnh <- C123_by_year(r_clans_wealth, value_var = wealth_nohouse, weight = TRUE, weight_var = "clan_weight")
wealth_nohouse <- join_hh_clan(hh_wnh, clan_wnh, "wealth_nohouse")

# Wealth (incl home equity)
hh_w   <- C123_by_year(r_hh_wealth, value_var = wealth, weight = TRUE, weight_var = "fam_weight")
clan_w <- C123_by_year(r_clans_wealth, value_var = wealth, weight = TRUE, weight_var = "clan_weight")
wealth <- join_hh_clan(hh_w, clan_w, "wealth")

# # Plot
# prep_plot_diff_simple <- function(df, comp = c("C1", "C2", "C3")) {
#   comp <- match.arg(comp)
#   hh_col   <- paste0(comp, "_hh")
#   clan_col <- paste0(comp, "_clan")

#   df %>%
#     filter(year != "ALL") %>%
#     mutate(year = suppressWarnings(as.numeric(year))) %>%
#     transmute(
#       year = year,
#       diff = .data[[clan_col]] - .data[[hh_col]]
#     )
# }

# plot_diff_simple <- function(df, comp = c("C1", "C2", "C3"), title = NULL, ylim = NULL) {
#   comp <- match.arg(comp)
#   dd <- prep_plot_diff_simple(df, comp)

#   if (is.null(title)) title <- paste0(comp, " — Clan minus Household")

#   p <- ggplot(dd, aes(x = year, y = diff)) +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     geom_line(linewidth = 0.9) +
#     labs(
#       title = title,
#       x = "Year",
#       y = "Clan − Household"
#     ) +
#     theme_minimal(base_size = 12)

#   if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
#   p
# }

# get_global_ylim_diff_simple <- function(df) {
#   comps <- c("C1", "C2", "C3")
#   diffs <- map_dfr(comps, ~ prep_plot_diff_simple(df, .x)) %>% pull(diff)
#   range(diffs, na.rm = TRUE)
# }

# ylim_inc  <- get_global_ylim_diff_simple(inc)
# ylim_wnh  <- get_global_ylim_diff_simple(wealth_nohouse)
# ylim_w    <- get_global_ylim_diff_simple(wealth)

# # Income plots
# pdf(file = file.path(plot_dir, "income_C123_diff_plots.pdf"), width = 8, height = 5)
# print(plot_diff_simple(inc, "C1", "Income — C1 (Clan − Household)", ylim = ylim_inc))
# print(plot_diff_simple(inc, "C2", "Income — C2 (Clan − Household)", ylim = ylim_inc))
# print(plot_diff_simple(inc, "C3", "Income — C3 (Clan − Household)", ylim = ylim_inc))
# dev.off()

# # Wealth (no house) plots
# pdf(file = file.path(plot_dir, "wealth_nohouse_C123_diff_plots.pdf"), width = 8, height = 5)
# print(plot_diff_simple(wealth_nohouse, "C1", "Wealth (No Home Equity) — C1 (Clan − Household)", ylim = ylim_wnh))
# print(plot_diff_simple(wealth_nohouse, "C2", "Wealth (No Home Equity) — C2 (Clan − Household)", ylim = ylim_wnh))
# print(plot_diff_simple(wealth_nohouse, "C3", "Wealth (No Home Equity) — C3 (Clan − Household)", ylim = ylim_wnh))
# dev.off()

# # Wealth (incl home equity) plots
# pdf(file = file.path(plot_dir, "wealth_C123_diff_plots.pdf"), width = 8, height = 5)
# print(plot_diff_simple(wealth, "C1", "Wealth — C1 (Clan − Household)", ylim = ylim_w))
# print(plot_diff_simple(wealth, "C2", "Wealth — C2 (Clan − Household)", ylim = ylim_w))
# print(plot_diff_simple(wealth, "C3", "Wealth — C3 (Clan − Household)", ylim = ylim_w))
# dev.off()


# Summary table
get_all_row <- function(df) {
  df %>%
    filter(year == "ALL") %>%
    select(ends_with("_hh"), ends_with("_clan")) %>%
    distinct() %>%
    slice(1)
}

inc_all <- get_all_row(inc)
wnh_all <- get_all_row(wealth_nohouse)
w_all   <- get_all_row(wealth)

C123_summary <- bind_rows(
  inc_all %>% transmute(
    measure = "Income",
    C1_hh = C1_hh, C1_clan = C1_clan,
    C2_hh = C2_hh, C2_clan = C2_clan,
    C3_hh = C3_hh, C3_clan = C3_clan
  ),
  wnh_all %>% transmute(
    measure = "Wealth (no home equity)",
    C1_hh = C1_hh, C1_clan = C1_clan,
    C2_hh = C2_hh, C2_clan = C2_clan,
    C3_hh = C3_hh, C3_clan = C3_clan
  ),
  w_all %>% transmute(
    measure = "Wealth",
    C1_hh = C1_hh, C1_clan = C1_clan,
    C2_hh = C2_hh, C2_clan = C2_clan,
    C3_hh = C3_hh, C3_clan = C3_clan
  )
)

C123_summary_fmt <- C123_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  rename(
    `C1 (HH)`   = C1_hh,
    `C1 (Clan)` = C1_clan,
    `C2 (HH)`   = C2_hh,
    `C2 (Clan)` = C2_clan,
    `C3 (HH)`   = C3_hh,
    `C3 (Clan)` = C3_clan
  )

ft <- flextable(C123_summary_fmt) %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  bg(part = "header", bg = "#EEEEEE")

doc <- read_docx()
doc <- body_add_par(
  doc,
  "Table X. Average nuclear family indices (C1, C2, C3) for income and wealth",
  style = "heading 2"
)
doc <- body_add_flextable(doc, ft)

print(doc, target = file.path(out_dir, "C123_income_wealth_summary.docx"))

