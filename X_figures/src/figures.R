# INPUTS: 5_calculate_gini/output and 6_summary_output
# OUTPUTS: 7_figures/output

# SET UP ----------------------------------------------------------------------------------------------
# Data
r_hh     <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans  <- readRDS(here("4_clans", "output", "robust_clans.rds"))

# Restrict to wealth supplement years
wealth_years <- c(1984, 1989, 1994, seq(1999, 2021, by = 2))
r_hh_wealth    <- r_hh %>% filter(year %in% wealth_years)
r_clans_wealth <- r_clans %>% filter(year %in% wealth_years)

# Fonts for figures
theme_set(theme_minimal(base_size = 12, base_family = "serif"))
note_style <- fp_text(italic = TRUE, font.size = 10)

# Define functions for plotting Lorenz curves
# Manually plotting the Lorenz as a CDF because the weights introduced errors with the lorenz function
lorenz_tbl <- function(x, w) {
  stopifnot(length(x) == length(w))
  
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  
  # order by x
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  # totals
  total_w  <- sum(w)
  total_xw <- sum(x * w)
  
  # cumulative shares
  cum_w  <- cumsum(w) / total_w
  cum_xw <- cumsum(x * w) / total_xw
  
  tibble(p = c(0, cum_w), L = c(0, cum_xw))
}

get_lorenz_weighted <- function(df, value_var, weight_var, years, unit_label) {
  base <- df %>%
    filter(year %in% years, is.finite(.data[[value_var]])) %>%
    transmute(year, value = .data[[value_var]], w = .data[[weight_var]])
  
  base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())
      lorenz_tbl(d$value, d$w) %>%
        mutate(year = unique(d$year),
               Unit = factor(unit_label, levels = c("Household", "Clan")))
    })
}


# FIGURE 1 - COMPARISON OF INCOME AND WEALTH INEQUALITY FOR HOUSEHOLDS AND CLANS ------------------------------------------

# Data for Figure 1 
inc_by_year    <- read_csv(here("6_calculate_gini", "output", "all_ginis", "income.csv"))
wealth_by_year <- read_csv(here("6_calculate_gini", "output", "all_ginis", "wealth_nohouse.csv"))

# Build summary table
summary_tbl <- tribble(
  ~Unit,   ~Households,                                               ~Clans,
  "Income",   sprintf("%.3f\n(SE = %.3f)", inc_by_year$r_hh_w_inc[1], inc_by_year$r_hh_w_inc_se[1]),
              sprintf("%.3f\n(SE = %.3f)", inc_by_year$r_cl_w_inc[1], inc_by_year$r_cl_w_inc_se[1]),
  "Wealth",   sprintf("%.3f\n(SE = %.3f)", wealth_by_year$r_hh_w_wealth[1], wealth_by_year$r_hh_w_wealth_se[1]),
              sprintf("%.3f\n(SE = %.3f)", wealth_by_year$r_cl_w_wealth[1], wealth_by_year$r_cl_w_wealth_se[1])
)

ft <- flextable(summary_tbl) |>
  set_caption("Figure 1. Average Gini Coefficients for Households and Clans, 1969–2021") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |> 
  align(align = "center", part = "all")

note_style <- fp_text(italic = TRUE, font.size = 10)

doc <- read_docx() |>
  body_add_par("", style = "Normal") |>  
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(
      ftext("Note: Gini coefficients reported are averages across all years. 
      Income data were collected annually until 1997, and biennially thereafter. 
      Wealth data were collected every five years from 1984 to 1999, and every other year thereafter. 
      Both income and wealth are adjusted for inflation to constant YEAR dollars. Wealth is measured excluding home equity.
      Standard errors are shown in parentheses.", prop = note_style),
      fp_p = fp_par(text.align = "center")  
    )
  )

print(doc, target = here("X_figures", "output", "figure1", "Figure1.docx"))



# FIGURE 2 --------------------------------------------------------------------------------------------------------------

# ---- 2A. Income Table ----
summary <- read_csv(here("5_summary/output/summary_statistics.csv"))

table <- summary %>%
  filter(Table == "Income", Unit %in% c("Household", "Clan")) %>%
  select(Unit, N, unique_clans, mean_val_w, mean_val) %>%
  rename(
    `Unique Clans` = unique_clans,
    `Mean (Wtd.)`  = mean_val_w,
    `Mean (Unwtd.)`= mean_val
  ) %>%
  mutate(
    N              = format(round(N, 0), big.mark = ","),
    `Unique Clans` = format(round(`Unique Clans`, 0), big.mark = ","),
    `Mean (Wtd.)`  = format(round(`Mean (Wtd.)`, 0), big.mark = ","),
    `Mean (Unwtd.)`= format(round(`Mean (Unwtd.)`, 0), big.mark = ",")
  )

ft <- flextable(table) |>
  set_caption("Figure 2A. Income Summary Statistics") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(ftext("Note: Gini coefficients and distributions based on weighted data using PSID family and clan weights.", prop = note_style),
         fp_p = fp_par(text.align = "center"))
  )

print(doc, target = here("X_figures", "output", "figure2", "Figure2A.docx"))


# ---- 2B. Line Plot  ----
inc_data <- inc_by_year

# Data
yearly_plot <- inc_data %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year))

# Grab ALL values for labeling averages
all_vals <- inc_data %>%
  filter(year == "ALL") %>%
  select(r_hh_w_inc, r_cl_w_inc) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "Unit",
                      values_to = "Gini") %>%
  mutate(
    Label = sprintf("%.2f", Gini),   
    x = max(yearly_plot$year) + 1,
    y = Gini
  )


# Position the average labels
all_vals <- all_vals %>%
  mutate(y = case_when(
    Unit == "r_hh_w_inc" ~ Gini + 0.10,  # Household label a bit higher
    Unit == "r_cl_w_inc" ~ Gini + 0.10,  # Clan label even higher
    TRUE ~ Gini
  ))

plot <- ggplot(yearly_plot, aes(x = year)) +
  geom_smooth(aes(y = r_hh_w_inc, linetype = "Household"),
              color = "#E66101", se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_cl_w_inc, linetype = "Clan"),
              color = "#FDB863", se = FALSE, size = 0.9) +
  geom_text(data = all_vals,
            aes(x = x, y = y, label = Label),
            inherit.aes = FALSE,
            hjust = 0, family = "serif", size = 3.5) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(
    breaks = seq(min(yearly_plot$year, na.rm = TRUE),
               max(yearly_plot$year, na.rm = TRUE),
               by = 5),
    expand = expansion(mult = c(0, 0.15))  # add 15% space to the right
    ) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = "Figure 2B. Income Inequality 1969 - 2021",
    x = "Year", y = "Gini Coefficient", linetype = "Unit"
    ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

# plot <- plot_grid(
#   plot,
#   ggdraw() + draw_label("Note: Estimates weighted using PSID family and clan weights.",
#                         fontfamily = "serif", fontface = "italic", size = 10,
#                         x = 0.5, hjust = 0.5),
#   ncol = 1, rel_heights = c(1, 0.05)
# )


# ---- 2C. Lorenz Curve (orange, combined panel) ----
years_income <- c(1979, 2019)
inc_hh   <- get_lorenz_weighted(r_hh,    "inc_all",     "fam_weight",  years_income, "Household")
inc_clan <- get_lorenz_weighted(r_clans, "inc_all", "clan_weight", years_income, "Clan")
inc_all  <- bind_rows(inc_hh, inc_clan)

lorenz <- ggplot(
  inc_all,
  aes(x = p, y = L,
      color = factor(year),      # color only by year
      linetype = Unit,           # solid vs dashed by unit
      group = interaction(year, Unit))
) +
  geom_line(size = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("1979" = "#4a71c7", "2019" = "#E66101")) +  # orange shades
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = paste0("Figure 2C. Distribution of Income in ", paste(years_income, collapse = ", ")),
    x = "Cumulative Proportion of Units",
    y = "Cumulative Proportion of Income",
    color = "Year", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

# Combine plots side by side
combined <- plot_grid(
  plot, lorenz,
  ncol = 2,               # side by side
  rel_widths = c(1, 1)    # adjust widths if needed
)

# Save combined PDF
ggsave(
  here("X_figures", "output", "figure2", "Figure2B_C_combined.pdf"),
  plot = combined, width = 14, height = 7
)




# FIGURE 3 - WEALTH -------------------------------------------------------------------------------------------------------------
# ---- 3A. Wealth Table ----
summary <- read_csv(here("5_summary/output/summary_statistics.csv"))

table <- summary %>%
  filter(Table == "Wealth", Unit %in% c("Household", "Clan")) %>%
  select(Unit, N, unique_clans, mean_val_w, mean_val) %>%
  rename(
    `Unique Clans` = unique_clans,
    `Mean (Wtd.)`  = mean_val_w,
    `Mean (Unwtd.)`= mean_val
  ) %>%
  mutate(
     N              = format(round(N, 0), big.mark = ","),
    `Unique Clans` = format(round(`Unique Clans`, 0), big.mark = ","),
    `Mean (Wtd.)`  = format(round(`Mean (Wtd.)`, 0), big.mark = ","),
    `Mean (Unwtd.)`= format(round(`Mean (Unwtd.)`, 0), big.mark = ",")
  )

ft <- flextable(table) |>
  set_caption("Figure 3A. Wealth Summary Statistics") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(ftext("Note: Gini coefficients and distributions based on weighted data using PSID family and clan weights. Wealth is measured exclusive of home equity. ", prop = note_style),
         fp_p = fp_par(text.align = "center"))
  )

print(doc, target = here("X_figures", "output", "figure3", "Figure3A.docx"))


# ---- 3B. Line Plot  ----
wealth_data <- wealth_by_year

# Data
yearly_plot <- wealth_data %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year))

# Grab ALL values for labeling averages (numeric only)
all_vals <- wealth_data %>%
  filter(year == "ALL") %>%
  select(r_hh_w_wealth, r_cl_w_wealth) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "Unit",
                      values_to = "Gini") %>%
  mutate(
    Label = sprintf("%.2f", Gini),   # just number
    x = max(yearly_plot$year) + 1,
    y = Gini
  )

# Position labels slightly above the line
all_vals <- all_vals %>%
  mutate(y = case_when(
    Unit == "r_hh_w_wealth" ~ Gini + 0.01,
    Unit == "r_cl_w_wealth" ~ Gini + 0.01,
    TRUE ~ Gini
  ))

plot <- ggplot(yearly_plot, aes(x = year)) +
  geom_smooth(aes(y = r_hh_w_wealth, linetype = "Household"),
              color = "#E66101", se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_cl_w_wealth, linetype = "Clan"),
              color = "#FDB863", se = FALSE, size = 0.9) +
  geom_text(data = all_vals,
            aes(x = x, y = y, label = Label),
            inherit.aes = FALSE,
            hjust = 0, family = "serif", size = 3.5) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(
    breaks = seq(min(yearly_plot$year, na.rm = TRUE),
                 max(yearly_plot$year, na.rm = TRUE),
                 by = 5),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = "Figure 3B. Wealth Inequality 1984 - 2021",
    x = "Year", y = "Gini Coefficient", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")


# ---- 3C. Lorenz Curve ----
years_wealth <- c(1989, 2019)
w_hh   <- get_lorenz_weighted(r_hh_wealth,    "wealth_nohouse", "fam_weight",  years_wealth, "Household")
w_clan <- get_lorenz_weighted(r_clans_wealth, "wealth_nohouse", "clan_weight", years_wealth, "Clan")
w_all  <- bind_rows(w_hh, w_clan)

lorenz <- ggplot(
  w_all,
  aes(x = p, y = L,
      color = factor(year),
      linetype = Unit,
      group = interaction(year, Unit))
) +
  geom_line(size = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("1989" = "#4a71c7", "2019" = "#E66101")) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = paste0("Figure 3C. Distribution of Wealth in ", paste(years_wealth, collapse = ", ")),
    x = "Cumulative Proportion of Units",
    y = "Cumulative Proportion of Wealth",
    color = "Year", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")


# ---- Combine line + Lorenz plots side by side ----
combined <- plot_grid(
  plot, lorenz,
  ncol = 2,
  rel_widths = c(1, 1)
)

ggsave(
  here("X_figures", "output", "figure3", "Figure3B_C_combined.pdf"),
  plot = combined, width = 14, height = 7
)




# FIGURE 4 - BY RACE ----------------------------------------------------------------------------------------------------
# ---- 4A. Race Table (Income) ----
race_gini <- read_csv(here("6_calculate_gini/output/race_ginis/income_race.csv"))
race_all <- race_gini %>% filter(year == "ALL")

table_income <- tibble(
  Unit = c("Households", "Clans"),
  N = c(
    summary %>% filter(Table == "Income", Unit == "Household") %>% pull(N),
    summary %>% filter(Table == "Income", Unit == "Clan") %>% pull(N)
  ),
  `Black % Wtd.` = c(
    summary %>% filter(Table == "Income", Unit == "Household") %>% pull(black_pct_w),
    summary %>% filter(Table == "Income", Unit == "Clan") %>% pull(black_pct_w)
  ),
  `Gini Black` = c(
    race_all$r_hh_w_inc_black,
    race_all$r_cl_w_inc_black
  ),
  `Gini Non-Black` = c(
    race_all$r_hh_w_inc_nonblack,
    race_all$r_cl_w_inc_nonblack
  )
) %>%
  mutate(
    N = format(round(N, 0), big.mark = ","),
    `Black % Wtd.` = sprintf("%.1f%%", `Black % Wtd.`),
    `Gini Black` = sprintf("%.3f", `Gini Black`),
    `Gini Non-Black` = sprintf("%.3f", `Gini Non-Black`)
  )

ft_income <- flextable(table_income) |>
  set_caption("Figure 4A. Income Summary Statisics by Race") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

# ---- 4B. Race Line Plot (Income) ----
race_data <- read_csv(here("5_calculate_gini", "output", "inc_by_year_race.csv"))
yearly_plot <- race_data %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year))

all_vals <- race_data %>%
  filter(year == "ALL") %>%
  select(r_hh_w_inc_black, r_hh_w_inc_nonblack, 
         r_cl_w_inc_black, r_cl_w_inc_nonblack) %>%
  pivot_longer(everything(), names_to = "Series", values_to = "Gini") %>%
  mutate(
    Label = sprintf("%.2f", Gini),  # numeric only
    Race = ifelse(grepl("nonblack", Series), "Non-Black", "Black"),
    Unit = ifelse(grepl("^r_hh", Series), "Household", "Clan"),
    x = max(yearly_plot$year) + 1,
    y = Gini + 0.05
  )

plot_income <- ggplot(yearly_plot, aes(x = year)) +
  geom_smooth(aes(y = r_hh_w_inc_black, color = "Black", linetype = "Household"),
              se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_hh_w_inc_nonblack, color = "Non-Black", linetype = "Household"),
              se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_cl_w_inc_black, color = "Black", linetype = "Clan"),
              se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_cl_w_inc_nonblack, color = "Non-Black", linetype = "Clan"),
              se = FALSE, size = 0.9) +
  geom_text(data = all_vals,
            aes(x = x, y = y, label = Label, color = Race),
            inherit.aes = FALSE,
            hjust = 0, family = "serif", size = 3.5) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(
    breaks = seq(min(yearly_plot$year, na.rm = TRUE),
                 max(yearly_plot$year, na.rm = TRUE),
                 by = 5),
    expand = expansion(mult = c(0, 0.3))
  ) +
  scale_color_manual(values = c("Black" = "#1b9e77", "Non-Black" = "#E66101")) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = "Figure 4B. Income Inequality by Race 1968 - 2021",
    x = "Year", y = "Gini Coefficient",
    color = "Race", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

# ---- 4C. Wealth Table ----
wealth_gini <- read_csv(here("5_calculate_gini", "output", "wealth_by_year_race.csv"))
wealth_all <- wealth_gini %>% filter(year == "ALL")

table_wealth <- tibble(
  Unit = c("Households", "Clans"),
  N = c(
    summary %>% filter(Table == "Wealth", Unit == "Household") %>% pull(N),
    summary %>% filter(Table == "Wealth", Unit == "Clan") %>% pull(N)
  ),
  `Black % Wtd.` = c(
    summary %>% filter(Table == "Wealth", Unit == "Household") %>% pull(black_pct_w),
    summary %>% filter(Table == "Wealth", Unit == "Clan") %>% pull(black_pct_w)
  ),
  `Gini Black` = c(
    wealth_all$r_hh_w_wealth_black,
    wealth_all$r_cl_w_wealth_black
  ),
  `Gini Non-Black` = c(
    wealth_all$r_hh_w_wealth_nonblack,
    wealth_all$r_cl_w_wealth_nonblack
  )
) %>%
  mutate(
    N = format(round(N, 0), big.mark = ","),
    `Black % Wtd.` = sprintf("%.1f%%", `Black % Wtd.`),
    `Gini Black` = sprintf("%.3f", `Gini Black`),
    `Gini Non-Black` = sprintf("%.3f", `Gini Non-Black`)
  )

ft_wealth <- flextable(table_wealth) |>
  set_caption("Figure 4C. Wealth Summary Statistics by Race") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

# ---- 4D. Wealth Line Plot ----
wealth_data <- read_csv(here("5_calculate_gini", "output", "wealth_by_year_race.csv"))
yearly_plot <- wealth_data %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year))

all_vals <- wealth_data %>%
  filter(year == "ALL") %>%
  select(r_hh_w_wealth_black, r_hh_w_wealth_nonblack, 
         r_cl_w_wealth_black, r_cl_w_wealth_nonblack) %>%
  pivot_longer(everything(), names_to = "Series", values_to = "Gini") %>%
  mutate(
    Label = sprintf("%.2f", Gini),  # numeric only
    Race = ifelse(grepl("nonblack", Series), "Non-Black", "Black"),
    Unit = ifelse(grepl("^r_hh", Series), "Household", "Clan"),
    x = max(yearly_plot$year) + 1,
    y = Gini + 0.02
  )

plot_wealth <- ggplot(yearly_plot, aes(x = year)) +
  geom_smooth(aes(y = r_hh_w_wealth_black, color = "Black", linetype = "Household"),
              se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_hh_w_wealth_nonblack, color = "Non-Black", linetype = "Household"),
              se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_cl_w_wealth_black, color = "Black", linetype = "Clan"),
              se = FALSE, size = 0.9) +
  geom_smooth(aes(y = r_cl_w_wealth_nonblack, color = "Non-Black", linetype = "Clan"),
              se = FALSE, size = 0.9) +
  geom_text(data = all_vals,
            aes(x = x, y = y, label = Label, color = Race),
            inherit.aes = FALSE,
            hjust = 0, family = "serif", size = 3.5) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(
    breaks = seq(min(yearly_plot$year, na.rm = TRUE),
                 max(yearly_plot$year, na.rm = TRUE),
                 by = 5),
    expand = expansion(mult = c(0, 0.3))
  ) +
  scale_color_manual(values = c("Black" = "#1b9e77", "Non-Black" = "#E66101")) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = "Figure 4D. Wealth Inequality by Race 1984 - 2021",
    x = "Year", y = "Gini Coefficient",
    color = "Race", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

# ---- Export Tables (Word, side by side) ----
doc <- read_docx() |>
  body_add_flextable(ft_income) |>
  body_add_flextable(ft_wealth, align = "right")

print(doc, target = here("X_figures", "output", "figure4", "Figure4_Tables.docx"))

# ---- Export Plots (PDF, side by side) ----
combined_plots <- plot_grid(plot_income, plot_wealth, ncol = 2, rel_widths = c(1, 1))

ggsave(
  here("X_figures", "output", "figure4", "Figure4_Plots.pdf"),
  plot = combined_plots, width = 14, height = 7
)
