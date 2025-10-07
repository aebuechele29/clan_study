# The purpose of this file is to create tables and figures for the IPR presentation. Below is a list of figures:
    # 1. TABLE 1: AVERAGE GINI COEFFICIENTS OVER TIME
    # 2. TABLE 2: DESCRIPTIVE STATISTICS 
    # 3. FIGURE 1A: INCOME GINI COEFFICIENTS OVER TIME
    # 4. FIGURE 1B: INCOME QUINTILES IN 1979, 1999, AND 2019
    # 5. FIGURE 2A: WEALTH GINI COEFFICIENTS OVER TIME
    # 6. FIGURE 2B: WEALTH QUINTILES IN 1989, 2009, AND 2019
    # 7. FIGURE 3: RACE DIFFERENCE BETWEEN AVERAGE GINI COEFFICIENT FOR HOUSEHOLDS AND CLANS

# Need this for lonely PSUs when applying survey weights (there is only one 'lonely' PSU)
options(survey.lonely.psu = "adjust")

# Functions
get_wtd_median <- function(design, var) {
  as.numeric(svyquantile(as.formula(paste0("~", var)), design, 0.5, na.rm = TRUE, ci = FALSE)[1])
}
get_last_labels <- function(df, series, label_name) {
  df |> slice_tail(n = 1) |>
    transmute(x = year + 1, y = .data[[series]], Label = sprintf("%.2f", .data[[series]]), Series = label_name)
}
gg_gini_time <- function(df, y_hh, y_cl, title, caption) {
  labels <- bind_rows(
    get_last_labels(df, y_hh, "Household"),
    get_last_labels(df, y_cl, "Clan")
  )
  ggplot(df, aes(x = year)) +
    geom_line(aes(y = .data[[y_hh]], color = "Household"), size = 0.9) +
    geom_line(aes(y = .data[[y_cl]], color = "Clan"), size = 0.9) +
    geom_text(data = labels, aes(x = x, y = y, label = Label, color = Series),
              inherit.aes = FALSE, hjust = 0, family = "serif", size = 3.2) +
    scale_color_manual(values = c("Household" = "black", "Clan" = "#B6ACD1")) +
    scale_y_continuous(limits = c(0.25, 1)) +
    scale_x_continuous(breaks = seq(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE), by = 5),
                       expand = expansion(mult = c(0, 0.2))) +
    labs(title = title, x = "Year", y = "Gini Coefficient", color = "Unit", caption = caption) +
    theme_minimal(base_size = 12, base_family = "serif") +
    theme(legend.position = "right",
          plot.caption = element_text(hjust = 0.5, face = "italic", size = 10),
          plot.caption.position = "plot",
          plot.margin = margin(t = 10, r = 10, b = 50, l = 10))
}
fmt_table <- function(dat) {
  flextable(dat) |>
    autofit() |>
    theme_vanilla() |>
    fontsize(size = 12, part = "all") |>
    fontsize(size = 10, part = "body") |>
    align(align = "center", part = "all")
}
doc_write <- function(ft, caption, note, outpath) {
  note_style <- fp_text(italic = TRUE, font.size = 10)
  ft <- set_caption(ft, caption)
  read_docx() |>
    body_add_par("", style = "Normal") |>
    body_add_flextable(ft) |>
    body_add_fpar(fpar(ftext(note, prop = note_style), fp_p = fp_par(text.align = "center"))) |>
    print(target = outpath)
  invisible(NULL)
}

# TABLE 1: AVERAGE GINI COEFFICIENTS OVER TIME
inc_by_year    <- read_csv(here("5_calculate_gini", "output", "inc_by_year.csv"), show_col_types = FALSE)
wealth_by_year <- read_csv(here("5_calculate_gini", "output", "wealth_by_year.csv"), show_col_types = FALSE)
inc_all    <- inc_by_year    |> filter(year == "ALL")
wealth_all <- wealth_by_year |> filter(year == "ALL")

gini_table <- tibble(
  Unit   = c("Households", "Clans"),
  Income = c(inc_all$r_hh_w_inc, inc_all$r_cl_w_inc) |> round(3),
  Wealth = c(wealth_all$r_hh_w_wealth, wealth_all$r_cl_w_wealth) |> round(3)
)

ft1 <- fmt_table(gini_table)

doc_write(
  ft1,
  "Table 1: Average Gini Coefficients Over Time",
  "Note: Gini coefficients reported are averages across all years. 
      Income data were collected annually until 1997, and biennially thereafter. 
      Wealth data were collected every five years from 1984 to 1999, and every other year thereafter. 
      Both income and wealth are adjusted for inflation. Wealth is measured excluding home equity.
      Standard errors are shown in parentheses.",
  here("8_presentations", "ipr", "output", "table1.docx")
)

# TABLE 2: DESCRIPTIVE STATISTICS
r_hh    <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
wealth_years <- c(1984, 1989, 1994, seq(1999, 2021, by = 2))
r_hh_w    <- r_hh    |> filter(year %in% wealth_years)
r_clans_w <- r_clans |> filter(year %in% wealth_years)

hh_d   <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~fam_weight,  data = r_hh,   nest = TRUE)
cl_d   <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~clan_weight, data = r_clans, nest = TRUE)
hhw_d  <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~fam_weight,  data = r_hh_w,   nest = TRUE)
clnw_d <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~clan_weight, data = r_clans_w, nest = TRUE)

t2 <- bind_rows(
  tibble(
    Unit = "Households",
    N = nrow(r_hh),
    `Unique clans` = n_distinct(r_hh$id1968),
    `Black %` = 100 * as.numeric(svymean(~black_head, hh_d, na.rm = TRUE)),
    `Avg. No. of Individuals` = as.numeric(svymean(~numfu, hh_d, na.rm = TRUE)),
    `Avg. No. of Households` = NA_real_,
    `Median Income` = get_wtd_median(hh_d, "inc_all"),
    `Median Wealth` = get_wtd_median(hhw_d, "wealth_nohouse")
  ),
  tibble(
    Unit = "Clans",
    N = nrow(r_clans),
    `Unique clans` = n_distinct(r_clans$id1968),
    `Black %` = 100 * as.numeric(svymean(~black_clan, cl_d, na.rm = TRUE)),
    `Avg. No. of Individuals` = as.numeric(svymean(~num_clan_people, cl_d, na.rm = TRUE)),
    `Avg. No. of Households`  = as.numeric(svymean(~numclan, cl_d, na.rm = TRUE)),
    `Median Income` = get_wtd_median(cl_d, "inc_all"),
    `Median Wealth` = get_wtd_median(clnw_d, "wealth_nohouse")
  )
) |>
  mutate(
    N = format(round(N), big.mark = ","),
    `Unique clans` = format(round(`Unique clans`), big.mark = ","),
    `Black %` = sprintf("%.1f", `Black %`),
    `Avg. No. of Individuals` = sprintf("%.1f", `Avg. No. of Individuals`),
    `Avg. No. of Households`  = ifelse(is.na(`Avg. No. of Households`), "", sprintf("%.1f", `Avg. No. of Households`)),
    `Median Income` = format(round(`Median Income`), big.mark = ","),
    `Median Wealth` = format(round(`Median Wealth`), big.mark = ",")
  )

ft2 <- fmt_table(t2)
doc_write(
  ft2,
  "Table 2: Summary Statistics",
  "Note: Statistics pool all years. Percent Black and central tendencies are weighted values. Wealth excludes home equity.",
  here("8_presentations", "ipr", "output", "table2.docx")
)

# FIGURE 1A: INCOME GINI COEFFICIENTS OVER TIME
inc_yearly <- inc_by_year |> filter(year != "ALL") |> mutate(year = as.numeric(year))
p_inc <- gg_gini_time(
  inc_yearly, "r_hh_w_inc", "r_cl_w_inc",
  "Figure 1A: Income Inequality from 1969 - 2021",
  "Note: Gini coefficents are weighted. Income data were collected annually until 1997, and biennially thereafter."
)
ggsave(here("8_presentations", "ipr", "output", "figure1a.pdf"),
       plot = p_inc, width = 7, height = 5, bg = "white")

# FIGURE 2A: WEALTH GINI COEFFICIENTS OVER TIME
wealth_yearly <- wealth_by_year |> filter(year != "ALL") |> mutate(year = as.numeric(year))
p_w <- gg_gini_time(
  wealth_yearly, "r_hh_w_wealth", "r_cl_w_wealth",
  "Figure 2A: Wealth Inequality from 1984 - 2021",
  "Note: Gini coefficients are weighted. Wealth data were collected every five years 
  from 1984 to 1999, and every other year thereafter. 
  Wealth is measured excluding home equity."
)
ggsave(here("8_presentations", "ipr", "output", "figure2a.pdf"),
       plot = p_w, width = 7, height = 5, bg = "white")

# FIGURE 1B: INCOME QUINTILES IN 1979, 1999, AND 2019
years_quintile <- c(1979, 1999, 2019)
hh_q <- r_hh |> filter(year %in% years_quintile)
cl_q <- r_clans |> filter(year %in% years_quintile)
get_quintile_means <- function(design, var) {
  qtiles <- svyquantile(as.formula(paste0("~", var)), design, quantiles = seq(0, 1, 0.2), na.rm = TRUE, ci = FALSE) |> unlist()
  design$variables$quintile <- cut(design$variables[[var]],
                                   breaks = c(-Inf, qtiles[2:5], Inf),
                                   labels = c("Lowest 20%", "2nd 20%", "3rd 20%", "4th 20%", "Highest 20%"),
                                   include.lowest = TRUE)
  m <- svyby(as.formula(paste0("~", var)), ~quintile, design, svymean, na.rm = TRUE)
  setNames(as.numeric(m[[var]]), as.character(m$quintile))
}

res_inc <- lapply(years_quintile, function(yr) {
  hh_y <- hh_q |> dplyr::filter(year == yr)
  cl_y <- cl_q |> dplyr::filter(year == yr)

  if (nrow(hh_y) == 0 || nrow(cl_y) == 0) return(NULL)

  hh_d <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~fam_weight,
                    data = hh_y, nest = TRUE)
  cl_d <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~clan_weight,
                    data = cl_y, nest = TRUE)

  qm_hh <- get_quintile_means(hh_d, "inc_all")
  qm_cl <- get_quintile_means(cl_d, "inc_all")

  dplyr::bind_rows(
    tibble::tibble(
      Year   = yr, Unit = "Household",
      Median = get_wtd_median(hh_d, "inc_all"),
      Mean   = as.numeric(svymean(~inc_all, hh_d, na.rm = TRUE)),
      people_or_clan = as.numeric(svymean(~numfu, hh_d, na.rm = TRUE)),
      `Lowest 20%`   = qm_hh[["Lowest 20%"]],
      `2nd 20%`      = qm_hh[["2nd 20%"]],
      `3rd 20%`      = qm_hh[["3rd 20%"]],
      `4th 20%`      = qm_hh[["4th 20%"]],
      `Highest 20%`  = qm_hh[["Highest 20%"]]
    ),
    tibble::tibble(
      Year   = yr, Unit = "Clan",
      Median = get_wtd_median(cl_d, "inc_all"),
      Mean   = as.numeric(svymean(~inc_all, cl_d, na.rm = TRUE)),
      people_or_clan = as.numeric(svymean(~num_clan_people, cl_d, na.rm = TRUE)),
      hh_per_clan    = as.numeric(svymean(~numclan, cl_d, na.rm = TRUE)),
      `Lowest 20%`   = qm_cl[["Lowest 20%"]],
      `2nd 20%`      = qm_cl[["2nd 20%"]],
      `3rd 20%`      = qm_cl[["3rd 20%"]],
      `4th 20%`      = qm_cl[["4th 20%"]],
      `Highest 20%`  = qm_cl[["Highest 20%"]]
    )
  )
}) |> dplyr::bind_rows()


t1b <- res_inc |>
  rename(`Avg. No. of Individuals` = people_or_clan, `Avg. No. of HH` = hh_per_clan) |>
  select(Year, Unit, `Avg. No. of Individuals`, `Avg. No. of HH`,
         Median, Mean, `Lowest 20%`, `2nd 20%`, `3rd 20%`, `4th 20%`, `Highest 20%`) |>
  arrange(Year, factor(Unit, levels = c("Household", "Clan")))

gini_lookup_inc <- inc_by_year |>
  filter(year %in% years_quintile) |>
  transmute(Year = as.numeric(year), Household = r_hh_w_inc, Clan = r_cl_w_inc) |>
  pivot_longer(c(Household, Clan), names_to = "Unit", values_to = "Gini")

t1b <- t1b |> left_join(gini_lookup_inc, by = c("Year", "Unit"))

t1b_fmt <- t1b |>
  select(Year, Unit, `Avg. No. of Individuals`, `Avg. No. of HH`,
         Gini, Median, Mean, `Lowest 20%`, `2nd 20%`, `3rd 20%`, `4th 20%`, `Highest 20%`) |>
  mutate(across(c(`Avg. No. of Individuals`, `Avg. No. of HH`, Gini),
                ~formatC(.x, format = "f", digits = 2, big.mark = ","))) |>
  mutate(across(c(Median, Mean, `Lowest 20%`, `2nd 20%`, `3rd 20%`, `4th 20%`, `Highest 20%`),
                ~formatC(.x, format = "f", digits = 0, big.mark = ",")))

ft1b <- fmt_table(t1b_fmt)
doc_write(
  ft1b,
  "Figure 1B. Income Quintiles for Households and Clans: 1979, 1999, 2019",
  "Note: Values are weighted. Quintile means are computed within each year’s distribution.",
  here("8_presentations", "ipr", "output", "figure1b.docx")
)

# FIGURE 2B: WEALTH QUINTILES IN 1989, 2009, AND 2019
years_quintile_w <- c(1989, 2009, 2019)
r_hh_w_all <- r_hh |> filter(year %in% wealth_years)
r_cl_w_all <- r_clans |> filter(year %in% wealth_years)

res_w <- lapply(years_quintile_w, function(yr) {
  hh_y <- r_hh_w_all |> dplyr::filter(year == yr)
  cl_y <- r_cl_w_all |> dplyr::filter(year == yr)
  if (nrow(hh_y) == 0 || nrow(cl_y) == 0) return(NULL)

  hh_dy <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~fam_weight,
                     data = hh_y, nest = TRUE)
  cl_dy <- svydesign(ids = ~cluster, strata = ~stratum, weights = ~clan_weight,
                     data = cl_y, nest = TRUE)

  qm_hh <- get_quintile_means(hh_dy, "wealth_nohouse")
  qm_cl <- get_quintile_means(cl_dy, "wealth_nohouse")

  dplyr::bind_rows(
    tibble::tibble(
      Year = yr, Unit = "Household",
      Median = get_wtd_median(hh_dy, "wealth_nohouse"),
      Mean   = as.numeric(svymean(~wealth_nohouse, hh_dy, na.rm = TRUE)),
      people_or_clan = as.numeric(svymean(~numfu, hh_dy, na.rm = TRUE)),
      `Lowest 20%`   = qm_hh[["Lowest 20%"]],
      `2nd 20%`      = qm_hh[["2nd 20%"]],
      `3rd 20%`      = qm_hh[["3rd 20%"]],
      `4th 20%`      = qm_hh[["4th 20%"]],
      `Highest 20%`  = qm_hh[["Highest 20%"]]
    ),
    tibble::tibble(
      Year = yr, Unit = "Clan",
      Median = get_wtd_median(cl_dy, "wealth_nohouse"),
      Mean   = as.numeric(svymean(~wealth_nohouse, cl_dy, na.rm = TRUE)),
      people_or_clan = as.numeric(svymean(~num_clan_people, cl_dy, na.rm = TRUE)),
      hh_per_clan    = as.numeric(svymean(~numclan, cl_dy, na.rm = TRUE)),
      `Lowest 20%`   = qm_cl[["Lowest 20%"]],
      `2nd 20%`      = qm_cl[["2nd 20%"]],
      `3rd 20%`      = qm_cl[["3rd 20%"]],
      `4th 20%`      = qm_cl[["4th 20%"]],
      `Highest 20%`  = qm_cl[["Highest 20%"]]
    )
  )
}) |> dplyr::bind_rows()


t2b <- res_w |>
  rename(`Avg. No. of Individuals` = people_or_clan, `Avg. No. of HH` = hh_per_clan) |>
  select(Year, Unit, `Avg. No. of Individuals`, `Avg. No. of HH`,
         Median, Mean, `Lowest 20%`, `2nd 20%`, `3rd 20%`, `4th 20%`, `Highest 20%`) |>
  arrange(Year, factor(Unit, levels = c("Household", "Clan")))

gini_lookup_w <- wealth_by_year |>
  filter(year %in% years_quintile_w) |>
  transmute(Year = as.numeric(year), Household = r_hh_w_wealth, Clan = r_cl_w_wealth) |>
  pivot_longer(c(Household, Clan), names_to = "Unit", values_to = "Gini")

t2b <- t2b |> left_join(gini_lookup_w, by = c("Year", "Unit"))

t2b_fmt <- t2b |>
  select(Year, Unit, `Avg. No. of Individuals`, `Avg. No. of HH`,
         Gini, Median, Mean, `Lowest 20%`, `2nd 20%`, `3rd 20%`, `4th 20%`, `Highest 20%`) |>
  mutate(across(c(`Avg. No. of Individuals`, `Avg. No. of HH`, Gini),
                ~formatC(.x, format = "f", digits = 2, big.mark = ","))) |>
  mutate(across(c(Median, Mean, `Lowest 20%`, `2nd 20%`, `3rd 20%`, `4th 20%`, `Highest 20%`),
                ~formatC(.x, format = "f", digits = 0, big.mark = ",")))

ft2b <- fmt_table(t2b_fmt)
doc_write(
  ft2b,
  "Figure 2B. Wealth Quintiles for Households and Clans: 1989, 2009, 2019",
  "Note: Values are weighted. Quintile means are computed within each year’s distribution.",
  here("8_presentations", "ipr", "output", "figure2b.docx")
)

# FIGURE 3: RACE DIFFERENCE BETWEEN AVERAGE GINI COEFFICIENT FOR HOUSEHOLDS AND CLANS
inc_by_year_race    <- read_csv(here("5_calculate_gini", "output", "inc_by_year_race.csv"), show_col_types = FALSE)
wealth_by_year_race <- read_csv(here("5_calculate_gini", "output", "wealth_by_year_race.csv"), show_col_types = FALSE)
inc_all_race    <- inc_by_year_race    |> filter(year == "ALL")
wealth_all_race <- wealth_by_year_race |> filter(year == "ALL")

diff_table <- tibble(
  Group  = c("Black", "Non-Black"),
  Income = c(
    as.numeric(inc_all_race$r_hh_w_inc_black  - inc_all_race$r_cl_w_inc_black),
    as.numeric(inc_all_race$r_hh_w_inc_nonblack - inc_all_race$r_cl_w_inc_nonblack)
  ),
  Wealth = c(
    as.numeric(wealth_all_race$r_hh_w_wealth_black - wealth_all_race$r_cl_w_wealth_black),
    as.numeric(wealth_all_race$r_hh_w_wealth_nonblack - wealth_all_race$r_cl_w_wealth_nonblack)
  )
) |>
  mutate(across(c(Income, Wealth), ~sprintf("%.3f", .x)))

ft3 <- fmt_table(diff_table)
doc_write(
  ft3,
  "Table 3: Difference in Gini Coefficients for Households versus Clans by Race",
  "Note: Positive values mean household Gini exceeds clan Gini. Values are survey-weighted; Ginis averaged across years within groups. Wealth excludes home equity.",
  here("8_presentations", "ipr", "output", "table3.docx")
)
