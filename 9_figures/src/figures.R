library(here)
library(dplyr)
library(tidyr)     
library(tibble)
library(purrr)     
library(readr)
library(ggplot2)
library(flextable)
library(officer)
library(grid)
library(gridExtra)

# Load data
dir.create(here("9_figures", "output"), recursive = TRUE, showWarnings = FALSE)
r_hh    <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

wealth_years <- c(1984, 1989, 1994, seq(1999, 2023, by = 2))
r_hh_wealth    <- r_hh    %>% filter(year %in% wealth_years)
r_clans_wealth <- r_clans %>% filter(year %in% wealth_years)

inc_by_year    <- read_csv(here("6_calculate_gini", "output", "income.csv"), show_col_types = FALSE)
wealth_by_year <- read_csv(here("6_calculate_gini", "output", "wealth_nohouse.csv"), show_col_types = FALSE)
summary        <- read_csv(here("5_summary", "output", "summary_statistics.csv"), show_col_types = FALSE)


# Global styles
base_family     <- "serif"
base_size       <- 18
title_size      <- 22
sub_size        <- 16
note_size       <- 12
table_width_npc <- 0.62

theme_set(theme_minimal(base_size = base_size, base_family = base_family))
note_style <- fp_text(italic = TRUE, font.size = note_size)

# Function to format estimate and standard error for table display
fmt_se <- function(x, se) sprintf("%.3f\n(SE = %.3f)", x, se)

# Function to compute Lorenz curve points for values x with weights w
lorenz_tbl <- function(x, w) {
  stopifnot(length(x) == length(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  total_w <- sum(w)
  total_xw <- sum(x * w)
  cum_w  <- cumsum(w) / total_w
  cum_xw <- cumsum(x * w) / total_xw
  tibble(p = c(0, cum_w), L = c(0, cum_xw))
}

# Function to compute weighted Lorenz curves by year for a given unit/weight
get_lorenz_weighted <- function(df, value_var, weight_var, years, unit_label) {
  base <- df %>%
    filter(year %in% years, is.finite(.data[[value_var]])) %>%
    transmute(year, value = .data[[value_var]], w = .data[[weight_var]])

  base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      lorenz_tbl(d$value, d$w) %>%
        mutate(
          year = unique(d$year),
          Unit = factor(unit_label, levels = c("Household", "Clan"))
        )
    })
}

# Function to plot Gini over time with clean "pretty" x-axis ticks + always show final year; no end labels
make_gini_plot <- function(by_year_df, hh_col, cl_col, ylab, n_breaks = 6) {

  yearly_plot <- by_year_df %>%
    filter(year != "ALL") %>%
    mutate(year = as.numeric(year))

  min_year <- min(yearly_plot$year, na.rm = TRUE)
  max_year <- max(yearly_plot$year, na.rm = TRUE)

  x_breaks <- {
    br <- scales::breaks_pretty(n = n_breaks)(c(min_year, max_year))
    br <- br[br >= min_year & br <= max_year]
    br <- sort(unique(c(br, max_year)))

    if (length(br) >= 2 && (max_year - br[length(br) - 1]) <= 2) {
      br <- br[-(length(br) - 1)]
      br[length(br)] <- max_year
    }
    br
  }

  ggplot(yearly_plot, aes(x = year)) +
    geom_smooth(aes(y = {{ hh_col }}, linetype = "Household"),
                color = "#E66101", se = FALSE, size = 0.9) +
    geom_smooth(aes(y = {{ cl_col }}, linetype = "Clan"),
                color = "#FDB863", se = FALSE, size = 0.9) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(
      breaks = x_breaks,
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    labs(x = "Year", y = ylab, linetype = "Unit") +
    theme(legend.position = "bottom", plot.title = element_blank())
}

# Function to make Lorenz plot (household vs clan) for selected years
make_lorenz_plot <- function(df_hh, df_cl, value_var, years, ylab, colors) {
  hh <- get_lorenz_weighted(df_hh, value_var, "fam_weight", years, "Household")
  cl <- get_lorenz_weighted(df_cl, value_var, "clan_weight", years, "Clan")
  dat <- bind_rows(hh, cl)

  ggplot(
    dat,
    aes(x = p, y = L, color = factor(year), linetype = Unit,
        group = interaction(year, Unit))
  ) +
    geom_line(size = 0.75) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    labs(
      x = "Cumulative Proportion of Units",
      y = ylab,
      color = "Year", linetype = "Unit"
    ) +
    theme(legend.position = "bottom", plot.title = element_blank())
}

# Function to  pull household-years, clan-years, and unique clans from summary stats
get_year_counts <- function(sum_df) {
  tibble(
    hh_years = sum_df %>% filter(Unit == "Household") %>% pull(N) %>% first(),
    cl_years = sum_df %>% filter(Unit == "Clan")      %>% pull(N) %>% first(),
    uniq_cl  = sum_df %>% filter(Unit == "Clan")      %>% pull(unique_clans) %>% first()
  )
}

# Function to  get a specific Gini from a "by year" file (ignores the "ALL" row)
get_gini_at <- function(df, year_value, col) {
  df %>%
    filter(year != "ALL") %>%
    mutate(year = as.numeric(year)) %>%
    filter(year == year_value) %>%
    pull({{ col }}) %>%
    first()
}

# Function to  percent change helper
pct_change <- function(start, end) 100 * (end - start) / start

# Function to  wrap long note text for PDFs
wrap_note <- function(x, width = 110) paste(strwrap(x, width = width), collapse = "\n")

# Function to  weighted mean + weighted median income for Figure 3 note
wtd_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  sum(x * w) / sum(w)
}

wtd_median <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

fmt_money0 <- function(x) format(round(x, 0), big.mark = ",")

# Function to  format integers with commas
fmt_int <- function(x) format(round(as.numeric(x), 0), big.mark = ",")

# Figure notes
# Precompute counts used in Figure 1 note
inc_counts    <- get_year_counts(summary %>% filter(Table == "Income", Unit %in% c("Household","Clan")))
wealth_counts <- get_year_counts(summary %>% filter(Table == "Wealth", Unit %in% c("Household","Clan")))

# Precompute endpoint ginis + percent changes used in Figure 2 note
inc_hh_1969 <- get_gini_at(inc_by_year,    1969, r_hh_w_inc)
inc_hh_2023 <- get_gini_at(inc_by_year,    2023, r_hh_w_inc)
inc_cl_1969 <- get_gini_at(inc_by_year,    1969, r_cl_w_inc)
inc_cl_2023 <- get_gini_at(inc_by_year,    2023, r_cl_w_inc)

w_hh_1984   <- get_gini_at(wealth_by_year, 1984, r_hh_w_wealth)
w_hh_2023   <- get_gini_at(wealth_by_year, 2023, r_hh_w_wealth)
w_cl_1984   <- get_gini_at(wealth_by_year, 1984, r_cl_w_wealth)
w_cl_2023   <- get_gini_at(wealth_by_year, 2023, r_cl_w_wealth)

inc_hh_pct <- pct_change(inc_hh_1969, inc_hh_2023)
inc_cl_pct <- pct_change(inc_cl_1969, inc_cl_2023)
w_hh_pct   <- pct_change(w_hh_1984,   w_hh_2023)
w_cl_pct   <- pct_change(w_cl_1984,   w_cl_2023)

# Figure 1
fig1_tbl <- tribble(
  ~Unit, ~Households, ~Clans, ~Difference,
  "Income",
  fmt_se(inc_by_year$r_hh_w_inc[1], inc_by_year$r_hh_w_inc_se[1]),
  fmt_se(inc_by_year$r_cl_w_inc[1], inc_by_year$r_cl_w_inc_se[1]),
  fmt_se(
    inc_by_year$r_hh_w_inc[1] - inc_by_year$r_cl_w_inc[1],
    sqrt(inc_by_year$r_hh_w_inc_se[1]^2 + inc_by_year$r_cl_w_inc_se[1]^2)
  ),
  "Wealth",
  fmt_se(wealth_by_year$r_hh_w_wealth[1], wealth_by_year$r_hh_w_wealth_se[1]),
  fmt_se(wealth_by_year$r_cl_w_wealth[1], wealth_by_year$r_cl_w_wealth_se[1]),
  fmt_se(
    wealth_by_year$r_hh_w_wealth[1] - wealth_by_year$r_cl_w_wealth[1],
    sqrt(wealth_by_year$r_hh_w_wealth_se[1]^2 + wealth_by_year$r_cl_w_wealth_se[1]^2)
  )
)

ft1 <- flextable(fig1_tbl) |>
  set_caption("Table 1. Average Gini Coefficients for Households and Clans, 1969–2023") |>
  theme_vanilla() |>
  bold(part = "header") |>
  align(align = "center", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  autofit()

doc1 <- read_docx() |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft1) |>
  body_add_fpar(
    fpar(
      ftext(
        sprintf(
          paste0(
            "Note: Gini coefficients reported are averages across all years. ",
            "Income data were collected annually until 1997, and biennially thereafter. ",
            "Wealth data were collected every five years from 1984 to 1999, and every other year thereafter. ",
            "Both income and wealth are adjusted for inflation. Wealth is measured excluding home equity. ",
            "Standard errors are shown in parentheses. ",
            "Income estimates use %s household-years and %s clan-years, with %s unique clans. ",
            "Wealth estimates use %s household-years and %s clan-years, with %s unique clans."
          ),
          format(round(inc_counts$hh_years, 0), big.mark = ","),
          format(round(inc_counts$cl_years, 0), big.mark = ","),
          format(round(inc_counts$uniq_cl,  0), big.mark = ","),
          format(round(wealth_counts$hh_years, 0), big.mark = ","),
          format(round(wealth_counts$cl_years, 0), big.mark = ","),
          format(round(wealth_counts$uniq_cl,  0), big.mark = ",")
        ),
        prop = note_style
      ),
      fp_p = fp_par(text.align = "center")
    )
  )

print(doc1, target = here("9_figures", "output", "table1.docx"))
message("Saved: ", here("9_figures", "output", "table1.docx"))

# Figure 2
out2 <- here("9_figures", "output", "figure2.pdf")

income_sum <- summary %>% filter(Table == "Income", Unit %in% c("Household", "Clan"))
wealth_sum <- summary %>% filter(Table == "Wealth", Unit %in% c("Household", "Clan"))

inc_mean_hh_w <- income_sum %>% filter(Unit == "Household") %>% pull(mean_val_w) %>% first()
inc_mean_cl_w <- income_sum %>% filter(Unit == "Clan")      %>% pull(mean_val_w) %>% first()
w_mean_hh_w   <- wealth_sum %>% filter(Unit == "Household") %>% pull(mean_val_w) %>% first()
w_mean_cl_w   <- wealth_sum %>% filter(Unit == "Clan")      %>% pull(mean_val_w) %>% first()

note2_text <- sprintf(
  paste0(
    "Note: Gini coefficients are estimated from weighted data using PSID family and clan weights. ",
    "Weighted mean income is %s for households and %s for clans. ",
    "Weighted mean wealth is %s for households and %s for clans (wealth excludes home equity). ",
    "The Gini coefficient for income for households rose by %.1f%% (%.2f in 1969 and %.2f in 2023). ",
    "The Gini coefficient for income for clans rose by %.1f%% (%.2f in 1969 and %.2f in 2023). ",
    "The Gini coefficient for wealth for households rose by %.1f%% (%.2f in 1984 and %.2f in 2023). ",
    "The Gini coefficient for wealth for clans rose by %.1f%% (%.2f in 1984 and %.2f in 2023)."
  ),
  format(round(inc_mean_hh_w, 0), big.mark = ","),
  format(round(inc_mean_cl_w, 0), big.mark = ","),
  format(round(w_mean_hh_w,   0), big.mark = ","),
  format(round(w_mean_cl_w,   0), big.mark = ","),
  inc_hh_pct, inc_hh_1969, inc_hh_2023,
  inc_cl_pct, inc_cl_1969, inc_cl_2023,
  w_hh_pct,   w_hh_1984,   w_hh_2023,
  w_cl_pct,   w_cl_1984,   w_cl_2023
)

p2_gini_inc <- make_gini_plot(inc_by_year, r_hh_w_inc, r_cl_w_inc, "Gini Coefficient", n_breaks = 5)
p2_gini_w   <- make_gini_plot(wealth_by_year, r_hh_w_wealth, r_cl_w_wealth, "Gini Coefficient")

title2 <- textGrob(
  "Figure 2. Income and Wealth Inequality Overtime",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
)
sub2L <- textGrob("Panel A: Income inequality from 1969 to 2023",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
sub2R <- textGrob("Panel B: Wealth inequality from 1984 to 2023",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))

note2 <- textGrob(
  wrap_note(note2_text, width = 180),
  gp = gpar(fontfamily = base_family, fontface = "italic", fontsize = note_size),
  just = "center"
)

left2  <- arrangeGrob(sub2L, p2_gini_inc, ncol = 1, heights = c(0.08, 0.92))
right2 <- arrangeGrob(sub2R, p2_gini_w,   ncol = 1, heights = c(0.08, 0.92))
plots2 <- arrangeGrob(left2, right2, ncol = 2)

fig2 <- arrangeGrob(title2, plots2, note2, ncol = 1, heights = c(0.10, 0.78, 0.12))
ggsave(out2, fig2, width = 14, height = 9)
message("Saved: ", out2)

# Figure 3
out3 <- here("9_figures", "output", "figure3.pdf")

p3_lor_inc <- make_lorenz_plot(
  r_hh, r_clans, "inc_all", years = c(1984, 2023),
  ylab = "Cumulative Proportion of Income",
  colors = c("1984" = "#4a71c7", "2023" = "#E66101")
)
p3_lor_w <- make_lorenz_plot(
  r_hh_wealth, r_clans_wealth, "wealth_nohouse", years = c(1984, 2023),
  ylab = "Cumulative Proportion of Wealth",
  colors = c("1984" = "#4a71c7", "2023" = "#E66101")
)

inc_stats_hh <- r_hh %>%
  filter(year %in% c(1984, 2023), is.finite(inc_all), is.finite(fam_weight), fam_weight > 0) %>%
  group_by(year) %>%
  summarise(
    mean = wtd_mean(inc_all, fam_weight),
    med  = wtd_median(inc_all, fam_weight),
    .groups = "drop"
  )

inc_stats_cl <- r_clans %>%
  filter(year %in% c(1984, 2023), is.finite(inc_all), is.finite(clan_weight), clan_weight > 0) %>%
  group_by(year) %>%
  summarise(
    mean = wtd_mean(inc_all, clan_weight),
    med  = wtd_median(inc_all, clan_weight),
    .groups = "drop"
  )

hh_1984_mean <- fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 1984])
hh_1984_med  <- fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 1984])
cl_1984_mean <- fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 1984])
cl_1984_med  <- fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 1984])

hh_2023_mean <- fmt_money0(inc_stats_hh$mean[inc_stats_hh$year == 2023])
hh_2023_med  <- fmt_money0(inc_stats_hh$med [inc_stats_hh$year == 2023])
cl_2023_mean <- fmt_money0(inc_stats_cl$mean[inc_stats_cl$year == 2023])
cl_2023_med  <- fmt_money0(inc_stats_cl$med [inc_stats_cl$year == 2023])

title3 <- textGrob(
  "Figure 3. Lorenz Curves at the Household and Clan Levels",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
)

sub3L <- textGrob("Panel A: Distribution of income in 1984 and 2023",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
sub3R <- textGrob("Panel B: Distribution of wealth in 1984 and 2023",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))

note3_text <- sprintf(
  paste0(
    "Note: Lorenz curves are estimated from weighted data using PSID family and clan weights. ",
    "Wealth is measured excluding home equity. ",
    "For income in 1984, the weighted mean (median) is %s (%s) for households and %s (%s) for clans. ",
    "For income in 2023, the weighted mean (median) is %s (%s) for households and %s (%s) for clans."
  ),
  hh_1984_mean, hh_1984_med, cl_1984_mean, cl_1984_med,
  hh_2023_mean, hh_2023_med, cl_2023_mean, cl_2023_med
)

note3 <- textGrob(
  wrap_note(note3_text, width = 110),
  gp = gpar(fontfamily = base_family, fontface = "italic", fontsize = note_size),
  just = "center"
)

left3  <- arrangeGrob(sub3L, p3_lor_inc, ncol = 1, heights = c(0.08, 0.92))
right3 <- arrangeGrob(sub3R, p3_lor_w,   ncol = 1, heights = c(0.08, 0.92))
plots3 <- arrangeGrob(left3, right3, ncol = 2)

fig3 <- arrangeGrob(title3, plots3, note3, ncol = 1, heights = c(0.10, 0.78, 0.12))
ggsave(out3, fig3, width = 14, height = 9)
message("Saved: ", out3)


# Figure 4
inc_race <- read_csv(here("7_gini_by_race", "output", "income_race.csv"), show_col_types = FALSE) %>%
  filter(year == "ALL")
w_race <- read_csv(here("7_gini_by_race", "output", "wealth_nohouse_race.csv"), show_col_types = FALSE) %>%
  filter(year == "ALL")

# --- Race-specific counts from MICRODATA (race gini outputs do not include N's)
# Race flags:
#   r_hh$black_head   (0/1)
#   r_clans$black_clan (0/1)
# Clan id:
#   r_clans$id1968
#
# Table-1 style: counts reflect the estimation sample for each measure:
#   Income:  require finite inc_all + positive weights
#   Wealth:  require finite wealth_nohouse + positive weights (on wealth-year subsamples)

# Income sample counts (HH-years)
inc_hh_counts <- r_hh %>%
  filter(is.finite(inc_all), is.finite(fam_weight), fam_weight > 0, !is.na(black_head)) %>%
  summarise(
    inc_black_hh_years    = sum(black_head == 1, na.rm = TRUE),
    inc_nonblack_hh_years = sum(black_head == 0, na.rm = TRUE)
  )

# Income sample counts (clan-years + unique clans)
inc_cl_counts <- r_clans %>%
  filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0, !is.na(black_clan)) %>%
  summarise(
    inc_black_cl_years      = sum(black_clan == 1, na.rm = TRUE),
    inc_nonblack_cl_years   = sum(black_clan == 0, na.rm = TRUE),
    inc_uniq_black_clans    = n_distinct(id1968[black_clan == 1]),
    inc_uniq_nonblack_clans = n_distinct(id1968[black_clan == 0])
  )

# Wealth sample counts (HH-years) -- use the wealth-year subsample (r_hh_wealth)
w_hh_counts <- r_hh_wealth %>%
  filter(is.finite(wealth_nohouse), is.finite(fam_weight), fam_weight > 0, !is.na(black_head)) %>%
  summarise(
    w_black_hh_years    = sum(black_head == 1, na.rm = TRUE),
    w_nonblack_hh_years = sum(black_head == 0, na.rm = TRUE)
  )

# Wealth sample counts (clan-years + unique clans) -- wealth-year subsample (r_clans_wealth)
w_cl_counts <- r_clans_wealth %>%
  filter(is.finite(wealth_nohouse), is.finite(clan_weight), clan_weight > 0, !is.na(black_clan)) %>%
  summarise(
    w_black_cl_years      = sum(black_clan == 1, na.rm = TRUE),
    w_nonblack_cl_years   = sum(black_clan == 0, na.rm = TRUE),
    w_uniq_black_clans    = n_distinct(id1968[black_clan == 1]),
    w_uniq_nonblack_clans = n_distinct(id1968[black_clan == 0])
  )

# Unique clan totals (match Table 1 sample definition) + overlap across race

# INCOME sample: total unique clans (Table 1 style)
inc_total_uniq_clans <- r_clans %>%
  filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0) %>%
  summarise(u = n_distinct(id1968)) %>%
  pull(u)

# INCOME sample: unique clans by race exposure + overlap (time-varying black_clan allowed)
inc_black_ids <- r_clans %>%
  filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0, black_clan == 1) %>%
  distinct(id1968)

inc_nonblack_ids <- r_clans %>%
  filter(is.finite(inc_all), is.finite(clan_weight), clan_weight > 0, black_clan == 0) %>%
  distinct(id1968)

inc_black_uniq_clans    <- nrow(inc_black_ids)
inc_nonblack_uniq_clans <- nrow(inc_nonblack_ids)
inc_overlap_uniq_clans  <- nrow(inner_join(inc_black_ids, inc_nonblack_ids, by = "id1968"))


# WEALTH sample: total unique clans (Table 1 style; use wealth subsample)
w_total_uniq_clans <- r_clans_wealth %>%
  filter(is.finite(wealth_nohouse), is.finite(clan_weight), clan_weight > 0) %>%
  summarise(u = n_distinct(id1968)) %>%
  pull(u)

# WEALTH sample: unique clans by race exposure + overlap
w_black_ids <- r_clans_wealth %>%
  filter(is.finite(wealth_nohouse), is.finite(clan_weight), clan_weight > 0, black_clan == 1) %>%
  distinct(id1968)

w_nonblack_ids <- r_clans_wealth %>%
  filter(is.finite(wealth_nohouse), is.finite(clan_weight), clan_weight > 0, black_clan == 0) %>%
  distinct(id1968)

w_black_uniq_clans    <- nrow(w_black_ids)
w_nonblack_uniq_clans <- nrow(w_nonblack_ids)
w_overlap_uniq_clans  <- nrow(inner_join(w_black_ids, w_nonblack_ids, by = "id1968"))

# Pull scalars
inc_black_hh_years      <- inc_hh_counts$inc_black_hh_years
inc_nonblack_hh_years   <- inc_hh_counts$inc_nonblack_hh_years
inc_black_cl_years      <- inc_cl_counts$inc_black_cl_years
inc_nonblack_cl_years   <- inc_cl_counts$inc_nonblack_cl_years
inc_uniq_black_clans    <- inc_cl_counts$inc_uniq_black_clans
inc_uniq_nonblack_clans <- inc_cl_counts$inc_uniq_nonblack_clans

w_black_hh_years        <- w_hh_counts$w_black_hh_years
w_nonblack_hh_years     <- w_hh_counts$w_nonblack_hh_years
w_black_cl_years        <- w_cl_counts$w_black_cl_years
w_nonblack_cl_years     <- w_cl_counts$w_nonblack_cl_years
w_uniq_black_clans      <- w_cl_counts$w_uniq_black_clans
w_uniq_nonblack_clans   <- w_cl_counts$w_uniq_nonblack_clans


fig4_tbl <- tribble(
  ~Measure,
  ~`Black HH`, ~`Black Clans`, ~`Diff. (Black)`,
  ~`Non-Black HH`, ~`Non-Black Clans`, ~`Diff. (Non-Black)`,

  "Income",
  fmt_se(inc_race$r_hh_w_inc_black, inc_race$r_hh_w_inc_black_se),
  fmt_se(inc_race$r_cl_w_inc_black, inc_race$r_cl_w_inc_black_se),
  fmt_se(
    inc_race$r_hh_w_inc_black - inc_race$r_cl_w_inc_black,
    sqrt(inc_race$r_hh_w_inc_black_se^2 + inc_race$r_cl_w_inc_black_se^2)
  ),
  fmt_se(inc_race$r_hh_w_inc_nonblack, inc_race$r_hh_w_inc_nonblack_se),
  fmt_se(inc_race$r_cl_w_inc_nonblack, inc_race$r_cl_w_inc_nonblack_se),
  fmt_se(
    inc_race$r_hh_w_inc_nonblack - inc_race$r_cl_w_inc_nonblack,
    sqrt(inc_race$r_hh_w_inc_nonblack_se^2 + inc_race$r_cl_w_inc_nonblack_se^2)
  ),

  "Wealth",
  fmt_se(w_race$r_hh_w_wealth_black, w_race$r_hh_w_wealth_black_se),
  fmt_se(w_race$r_cl_w_wealth_black, w_race$r_cl_w_wealth_black_se),
  fmt_se(
    w_race$r_hh_w_wealth_black - w_race$r_cl_w_wealth_black,
    sqrt(w_race$r_hh_w_wealth_black_se^2 + w_race$r_cl_w_wealth_black_se^2)
  ),
  fmt_se(w_race$r_hh_w_wealth_nonblack, w_race$r_hh_w_wealth_nonblack_se),
  fmt_se(w_race$r_cl_w_wealth_nonblack, w_race$r_cl_w_wealth_nonblack_se),
  fmt_se(
    w_race$r_hh_w_wealth_nonblack - w_race$r_cl_w_wealth_nonblack,
    sqrt(w_race$r_hh_w_wealth_nonblack_se^2 + w_race$r_cl_w_wealth_nonblack_se^2)
  )
)

ft4 <- flextable(fig4_tbl) |>
  set_caption("Table 4. Differences in Inequality by Race") |>
  theme_vanilla() |>
  bold(part = "header") |>
  align(align = "center", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  autofit()

doc4 <- read_docx() |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft4) |>
  body_add_fpar(
    fpar(
      ftext(
  sprintf(
    paste0(
  "Note: Gini coefficients reported are averages across all years. ",
  "Income data were collected annually until 1997, and biennially thereafter. ",
  "Wealth data were collected every five years from 1984 to 1999, and every other year thereafter. ",
  "Both income and wealth are adjusted for inflation. Wealth is measured excluding home equity. ",
  "Standard errors are shown in parentheses. ",
  "Income estimates use %s Black household-years and %s Black clan-years, and %s Non-Black household-years and %s Non-Black clan-years, ",
  "with %s unique clans in total; %s clans ever classified as Black and %s clans ever classified as Non-Black, ",
  "including %s clans appearing in both groups across years. ",
  "Wealth estimates use %s Black household-years and %s Black clan-years, and %s Non-Black household-years and %s Non-Black clan-years, ",
  "with %s unique clans in total; %s clans ever classified as Black and %s clans ever classified as Non-Black, ",
  "including %s clans appearing in both groups across years."
),
    fmt_int(inc_black_hh_years),    fmt_int(inc_black_cl_years),
    fmt_int(inc_nonblack_hh_years), fmt_int(inc_nonblack_cl_years),
    fmt_int(inc_total_uniq_clans),  fmt_int(inc_black_uniq_clans),
    fmt_int(inc_nonblack_uniq_clans), fmt_int(inc_overlap_uniq_clans),
    fmt_int(w_black_hh_years),      fmt_int(w_black_cl_years),
    fmt_int(w_nonblack_hh_years),   fmt_int(w_nonblack_cl_years),
    fmt_int(w_total_uniq_clans),    fmt_int(w_black_uniq_clans),
    fmt_int(w_nonblack_uniq_clans), fmt_int(w_overlap_uniq_clans)
  ),
  prop = note_style
),

      fp_p = fp_par(text.align = "center")
    )
  )

out4 <- here("9_figures", "output", "table4.docx")
print(doc4, target = out4)
message("Saved: ", out4)

