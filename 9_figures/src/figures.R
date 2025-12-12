# Load packages
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

# Styles for all tables / plots
base_family     <- "serif"
base_size       <- 12
title_size      <- 18   # <-- bigger titles (Figures 2 & 3)
sub_size        <- 12
note_size       <- 10
table_width_npc <- 0.62

theme_set(theme_minimal(base_size = base_size, base_family = base_family))
note_style <- fp_text(italic = TRUE, font.size = note_size)

fmt_se <- function(x, se) sprintf("%.3f\n(SE = %.3f)", x, se)

# Function to get Lorenz curve
lorenz_tbl <- function(x, w) {
  stopifnot(length(x) == length(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  total_w <- sum(w); total_xw <- sum(x * w)
  cum_w  <- cumsum(w) / total_w
  cum_xw <- cumsum(x * w) / total_xw
  tibble(p = c(0, cum_w), L = c(0, cum_xw))
}

# Function to weight Lorenz curve
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

# Add table to top of combined figures and position note
make_top_table_grob <- function(sum_df, table_width_npc, base_family, base_size) {
  # note pieces
  unw_vals <- sum_df %>%
    summarise(
      unw_hh = first(mean_val[Unit == "Household"]),
      unw_cl = first(mean_val[Unit == "Clan"]),
      uniq   = first(unique_clans[Unit == "Clan"])
    )

  note_text <- sprintf(
    "Note: Reported mean %s is weighted. Unweighted mean %s is %s for households and %s for clans. There are %s unique clans%s.",
    tolower(first(sum_df$Table)),
    tolower(first(sum_df$Table)),
    format(round(unw_vals$unw_hh, 0), big.mark = ","),
    format(round(unw_vals$unw_cl, 0), big.mark = ","),
    format(round(unw_vals$uniq,   0), big.mark = ","),
    ifelse(first(sum_df$Table) == "Wealth", " (wealth excludes home equity)", "")
  )

  table_df <- sum_df %>%
    select(Unit, N, mean_val_w) %>%
    rename(`Mean (Wtd.)` = mean_val_w) %>%
    mutate(
      N = format(round(N, 0), big.mark = ","),
      `Mean (Wtd.)` = format(round(`Mean (Wtd.)`, 0), big.mark = ",")
    )

  ft <- flextable(table_df)
  ft <- theme_vanilla(ft)
  ft <- fontsize(ft, size = base_size, part = "all")
  ft <- font(ft, fontname = base_family, part = "all")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "all")
  ft <- width(ft, j = 1, width = 1.4)
  ft <- width(ft, j = 2, width = 1.4)
  ft <- width(ft, j = 3, width = 1.8)
  ft <- autofit(ft)

  table_grob_raw <- flextable::gen_grob(ft)
  table_grob <- gTree(
    children = gList(table_grob_raw),
    vp = viewport(width = unit(table_width_npc, "npc"), just = "center")
  )

  list(table_grob = table_grob, note_text = note_text)
}

# Plot Gini over time
make_gini_plot <- function(by_year_df, hh_col, cl_col, ylab, label_bump) {
  yearly_plot <- by_year_df %>%
    filter(year != "ALL") %>%
    mutate(year = as.numeric(year))

  all_vals <- by_year_df %>%
    filter(year == "ALL") %>%
    select({{hh_col}}, {{cl_col}}) %>%
    pivot_longer(everything(), names_to = "Unit", values_to = "Gini") %>%
    mutate(
      Label = sprintf("%.2f", Gini),
      x = max(yearly_plot$year, na.rm = TRUE) + 1,
      y = Gini + label_bump
    )

  ggplot(yearly_plot, aes(x = year)) +
    geom_smooth(aes(y = {{hh_col}}, linetype = "Household"),
                color = "#E66101", se = FALSE, size = 0.9) +
    geom_smooth(aes(y = {{cl_col}}, linetype = "Clan"),
                color = "#FDB863", se = FALSE, size = 0.9) +
    geom_text(
      data = all_vals,
      aes(x = x, y = y, label = Label),
      inherit.aes = FALSE,
      hjust = 0, family = base_family, size = 3.5
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(
      breaks = seq(min(yearly_plot$year, na.rm = TRUE),
                   max(yearly_plot$year, na.rm = TRUE),
                   by = 5),
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
    labs(x = "Year", y = ylab, linetype = "Unit") +
    theme(legend.position = "bottom", plot.title = element_blank())
}

# Plot Lorenz
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

# Load data
r_hh    <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

wealth_years <- c(1984, 1989, 1994, seq(1999, 2021, by = 2))
r_hh_wealth    <- r_hh    %>% filter(year %in% wealth_years)
r_clans_wealth <- r_clans %>% filter(year %in% wealth_years)

# Ginis
inc_by_year    <- read_csv(here("6_calculate_gini", "output", "income.csv"), show_col_types = FALSE)
wealth_by_year <- read_csv(here("6_calculate_gini", "output", "wealth_nohouse.csv"), show_col_types = FALSE)

# Summary stats
summary <- read_csv(here("5_summary", "output", "summary_statistics.csv"), show_col_types = FALSE)


# FIGURE 1 (docx table)
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
  set_caption("Figure 1. Average Gini Coefficients for Households and Clans, 1969–2021") |>
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
        "Note: Gini coefficients reported are averages across all years. Income data were collected annually until 1997, and biennially thereafter. Wealth data were collected every five years from 1984 to 1999, and every other year thereafter. Both income and wealth are adjusted for inflation. Wealth is measured excluding home equity. Standard errors are shown in parentheses.",
        prop = note_style
      ),
      fp_p = fp_par(text.align = "center")
    )
  )

dir.create(here("9_figures", "output"), recursive = TRUE, showWarnings = FALSE)
print(doc1, target = here("9_figures", "output", "Figure1.docx"))


# FIGURE 2 (income combined PDF)
out2 <- here("9_figures", "output", "figure2.pdf")

income_sum <- summary %>% filter(Table == "Income", Unit %in% c("Household", "Clan"))
top2 <- make_top_table_grob(income_sum, table_width_npc, base_family, base_size)

p2_gini <- make_gini_plot(inc_by_year, r_hh_w_inc, r_cl_w_inc, "Gini Coefficient", label_bump = 0.10)
p2_lor  <- make_lorenz_plot(
  r_hh, r_clans, "inc_all", years = c(1979, 2019),
  ylab = "Cumulative Proportion of Income",
  colors = c("1979" = "#4a71c7", "2019" = "#E66101")
)

title2 <- textGrob(
  "Figure 2. Income Inequality at the Household and Clan Levels",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
)
sub2A <- textGrob("Figure 2A: Income summary statistics",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
sub2B <- textGrob("Figure 2B: Income inequality from 1969 to 2021",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
sub2C <- textGrob("Figure 2C: Distribution of income in 1979 and 2019",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
note2 <- textGrob(top2$note_text,
                  gp = gpar(fontfamily = base_family, fontface = "italic", fontsize = note_size),
                  just = "center")

left2  <- arrangeGrob(sub2B, p2_gini, ncol = 1, heights = c(0.08, 0.92))
right2 <- arrangeGrob(sub2C, p2_lor,  ncol = 1, heights = c(0.08, 0.92))
plots2 <- arrangeGrob(left2, right2, ncol = 2)

fig2 <- arrangeGrob(
  title2, sub2A, top2$table_grob, plots2, note2,
  ncol = 1, heights = c(0.07, 0.05, 0.18, 0.60, 0.10)
)

ggsave(out2, fig2, width = 14, height = 9)
message("Saved: ", out2)


# FIGURE 3 (wealth combined PDF)
out3 <- here("9_figures", "output", "figure3.pdf")

wealth_sum <- summary %>% filter(Table == "Wealth", Unit %in% c("Household", "Clan"))
top3 <- make_top_table_grob(wealth_sum, table_width_npc, base_family, base_size)

p3_gini <- make_gini_plot(wealth_by_year, r_hh_w_wealth, r_cl_w_wealth, "Gini Coefficient", label_bump = 0.01)
p3_lor  <- make_lorenz_plot(
  r_hh_wealth, r_clans_wealth, "wealth_nohouse", years = c(1989, 2019),
  ylab = "Cumulative Proportion of Wealth",
  colors = c("1989" = "#4a71c7", "2019" = "#E66101")
)

title3 <- textGrob(
  "Figure 3. Wealth Inequality at the Household and Clan Levels",
  x = unit(0, "npc"), just = "left",
  gp = gpar(fontfamily = base_family, fontface = "bold", fontsize = title_size)
)
sub3A <- textGrob("Figure 3A: Wealth summary statistics",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
sub3B <- textGrob("Figure 3B: Wealth inequality from 1984 to 2021",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
sub3C <- textGrob("Figure 3C: Distribution of wealth in 1989 and 2019",
                  x = unit(0, "npc"), just = "left",
                  gp = gpar(fontfamily = base_family, fontsize = sub_size))
note3 <- textGrob(top3$note_text,
                  gp = gpar(fontfamily = base_family, fontface = "italic", fontsize = note_size),
                  just = "center")

left3  <- arrangeGrob(sub3B, p3_gini, ncol = 1, heights = c(0.08, 0.92))
right3 <- arrangeGrob(sub3C, p3_lor,  ncol = 1, heights = c(0.08, 0.92))
plots3 <- arrangeGrob(left3, right3, ncol = 2)

fig3 <- arrangeGrob(
  title3, sub3A, top3$table_grob, plots3, note3,
  ncol = 1, heights = c(0.07, 0.05, 0.18, 0.60, 0.10)
)

ggsave(out3, fig3, width = 14, height = 9)
message("Saved: ", out3)


# FIGURE 4 (race table docx)
inc_race <- read_csv(here("7_gini_by_race", "output", "income_race.csv"), show_col_types = FALSE) %>%
  filter(year == "ALL")
w_race <- read_csv(here("7_gini_by_race", "output", "wealth_nohouse_race.csv"), show_col_types = FALSE) %>%
  filter(year == "ALL")

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
  set_caption("Figure 4. Differences in Inequality by Race") |>
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
        "Note: Gini coefficients and distributions based on weighted data using PSID family and clan weights. Wealth is measured excluding home equity. Income data are from 1969 to 2021. Wealth data are from 1984 to 2021. Standard errors are shown in parentheses.",
        prop = note_style
      ),
      fp_p = fp_par(text.align = "center")
    )
  )

out4 <- here("9_figures", "output", "figure4.docx")
print(doc4, target = out4)
message("Saved: ", out4)

