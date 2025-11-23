# LOAD DATA ------------------------------------------------------------------
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

# Function to join HH and Clan results for C1, C2, C3
join_hh_clan <- function(hh_df, clan_df, var_label) {
  hh_df %>%
    rename_with(~ paste0(.x, "_hh"), starts_with("C")) %>%
    full_join(
      clan_df %>% rename_with(~ paste0(.x, "_clan"), starts_with("C")),
      by = "year"
    ) %>%
    arrange(year) %>%
    mutate(year = as.character(year)) %>%
    bind_rows(
      summarise(.,
        across(where(is.numeric), ~ mean(.x, na.rm = TRUE))
      ) %>%
        mutate(year = "ALL", .before = 1)
    ) %>%
    write_csv(here("7_nuclear_family", "output", "all_nuclear_family",
                   paste0(var_label, "_C123.csv")))
}


# Income by distribution
hh_inc <- C123_by_dist(r_hh, var = inc_all, weight = TRUE, weight_var = "fam_weight", cutoff = 0.10)
clan_inc <- C123_by_dist(r_clans, var = inc_all, weight = TRUE, weight_var = "clan_weight", cutoff = 0.10)
inc <- join_hh_clan(hh_inc, clan_inc, "income")

# Wealth by distribution
hh_wealth <- C123_by_dist(r_hh_wealth, var = wealth_nohouse, weight = TRUE, weight_var = "fam_weight", cutoff = 0.10)
clan_wealth <- C123_by_dist(r_clans_wealth, var = wealth_nohouse, weight = TRUE, weight_var = "clan_weight", cutoff = 0.10)
wealth_nohouse <- join_hh_clan(hh_wealth, clan_wealth, "wealth_nohouse")

hh_wealth <- C123_by_dist(r_hh_wealth, var = wealth, weight = TRUE, weight_var = "fam_weight", cutoff = 0.10)
clan_wealth <- C123_by_dist(r_clans_wealth, var = wealth, weight = TRUE, weight_var = "clan_weight", cutoff = 0.10)
wealth <- join_hh_clan(hh_wealth, clan_wealth, "wealth")


# PLOTS 
# Function to prep data for plotting
prep_plot_diff <- function(df, comp = c("C1", "C2", "C3")) {
  comp <- match.arg(comp)
  prefix <- paste0(comp, "_")

  df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(year = suppressWarnings(as.numeric(year))) %>%
    dplyr::select(year, dplyr::starts_with(prefix)) %>%
    tidyr::pivot_longer(
      cols      = -year,
      names_to  = "metric_group",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      group  = ifelse(grepl("_clan$", metric_group), "clan", "hh"),
      metric = sub("_(hh|clan)$", "", metric_group),
      metric = factor(
        metric,
        levels = c(
          paste0(prefix, "all"),
          paste0(prefix, "ex_top_10"),
          paste0(prefix, "ex_bottom_10")
        )
      )
    ) %>%
    dplyr::select(year, metric, group, value) %>%
    tidyr::pivot_wider(
      names_from  = group,
      values_from = value
    ) %>%
    dplyr::mutate(
      diff = clan - hh   # <--- this is the key
    ) %>%
    dplyr::select(year, metric, diff)
}

# Function to plot data with custom y-limits
plot_diff <- function(df, comp = c("C1", "C2", "C3"),
                      title = NULL, ylim = NULL) {
  comp <- match.arg(comp)
  dd   <- prep_plot_diff(df, comp)

  if (is.null(title)) {
    title <- paste0(comp, " — Clan minus Household")
  }

  p <- ggplot(dd, aes(x = year, y = diff, color = metric)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(size = 0.9) +
    labs(
      title = title,
      x = "Year",
      y = "Clan − Household",
      color = paste0(comp, " variant")
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }

  p
}

# Creates global y-axis limits
get_global_ylim_diff <- function(df) {
  comps <- c("C1", "C2", "C3")

  diffs <- purrr::map_dfr(comps, ~ prep_plot_diff(df, .x)) %>%
    dplyr::pull(diff)

  range(diffs, na.rm = TRUE)
}

ylim_inc_diff  <- get_global_ylim_diff(inc)
ylim_wnh_diff  <- get_global_ylim_diff(wealth_nohouse)


# Income differences (clan − hh)
c1_inc_diff <- plot_diff(inc, "C1", "Income — C1 (Clan − Household)", ylim = ylim_inc_diff)
c2_inc_diff <- plot_diff(inc, "C2", "Income — C2 / Gini (Clan − Household)", ylim = ylim_inc_diff)
c3_inc_diff <- plot_diff(inc, "C3", "Income — C3 (Clan − Household)", ylim = ylim_inc_diff)

pdf(file = file.path(here("7_nuclear_family", "output", "plots"),
                     "income_C123_diff_plots.pdf"),
    width = 8, height = 5)
print(c1_inc_diff)
print(c2_inc_diff)
print(c3_inc_diff)
dev.off()

# Wealth (no house) differences (clan − hh)
c1_wnh_diff <- plot_diff(wealth_nohouse, "C1",
                         "Wealth (No Home Equity) — C1 (Clan − Household)",
                         ylim = ylim_wnh_diff)
c2_wnh_diff <- plot_diff(wealth_nohouse, "C2",
                         "Wealth (No Home Equity) — C2 / Gini (Clan − Household)",
                         ylim = ylim_wnh_diff)
c3_wnh_diff <- plot_diff(wealth_nohouse, "C3",
                         "Wealth (No Home Equity) — C3 (Clan − Household)",
                         ylim = ylim_wnh_diff)

pdf(file = file.path(here("7_nuclear_family", "output", "plots"),
                     "wealth_nohouse_C123_diff_plots.pdf"),
    width = 8, height = 5)
print(c1_wnh_diff)
print(c2_wnh_diff)
print(c3_wnh_diff)
dev.off()



# Create summary table
inc_all <- inc %>%
  filter(year == "ALL") %>%
  select(starts_with("C1_all"), starts_with("C2_all"), starts_with("C3_all")) %>%
  distinct() %>%
  slice(1)

wnh_all <- wealth_nohouse %>%
  filter(year == "ALL") %>%
  select(starts_with("C1_all"), starts_with("C2_all"), starts_with("C3_all")) %>%
  distinct() %>%
  slice(1)

C123_summary <- bind_rows(
  inc_all %>%
    transmute(
      measure   = "Income",
      C1_hh     = C1_all_hh,
      C1_clan   = C1_all_clan,
      C2_hh     = C2_all_hh,
      C2_clan   = C2_all_clan,
      C3_hh     = C3_all_hh,
      C3_clan   = C3_all_clan
    ),
  wnh_all %>%
    transmute(
      measure   = "Wealth (no home equity)",
      C1_hh     = C1_all_hh,
      C1_clan   = C1_all_clan,
      C2_hh     = C2_all_hh,
      C2_clan   = C2_all_clan,
      C3_hh     = C3_all_hh,
      C3_clan   = C3_all_clan
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

ft <- flextable(C123_summary_fmt)
ft <- autofit(ft)
ft <- align(ft, align = "center", part = "all")
ft <- bold(ft, part = "header")
ft <- bg(ft, part = "header", bg = "#EEEEEE")

doc <- read_docx()
doc <- body_add_par(doc, "Table X. Average nuclear family indices (C1, C2, C3) for income and wealth", style = "heading 2")
doc <- body_add_flextable(doc, ft)

print(
  doc,
  target = here("7_nuclear_family", "output", "all_nuclear_family",
                "C123_income_wealth_summary.docx")
)
