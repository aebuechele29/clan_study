# INPUTS: 5_calculate_gini/output and 6_summary_output
# OUTPUTS: 7_figures/output

# SET UP ----------------------------------------------------------------------------------------------
# Data
# DATA ---------------------------------------------------------------------
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
# Manually plotting the Lorenz as a CDF because the weights introduced errors
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
inc_by_year    <- read_csv(here("5_calculate_gini", "output", "inc_by_year.csv"))
wealth_by_year <- read_csv(here("5_calculate_gini", "output", "wealth_by_year.csv"))

# Build summary table
summary_tbl <- tribble(
  ~Unit,   ~Households,                                               ~Clans,
  "Income",   sprintf("%.3f\n(SE = %.3f)", inc_by_year$r_hh_w_inc[1], inc_by_year$r_hh_w_inc_se[1]),
              sprintf("%.3f\n(SE = %.3f)", inc_by_year$r_cl_w_inc[1], inc_by_year$r_cl_w_inc_se[1]),
  "Wealth",   sprintf("%.3f\n(SE = %.3f)", wealth_by_year$r_hh_w_wealth[1], wealth_by_year$r_hh_w_wealth_se[1]),
              sprintf("%.3f\n(SE = %.3f)", wealth_by_year$r_cl_w_wealth[1], wealth_by_year$r_cl_w_wealth_se[1])
)

# Prep for export
ft <- flextable(summary_tbl) |>
  set_caption("Figure 1. Comparison of Income and Wealth Inequality for Households and Clans") |>
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
      ftext("Note: Standard errors in parentheses.", prop = note_style),
      fp_p = fp_par(text.align = "center")  
    )
  )

print(doc, target = here("7_figures", "output", "figure1", "Figure1.docx"))



# FIGURE 2 --------------------------------------------------------------------------------------------------------------

# ---- 2A. Income Table ----
summary <- read_csv(here("6_summary", "output", "summary_statistics.csv"))

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
  set_caption("Figure 2A. Income Table") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(ftext("Note: Estimates weighted using PSID family and clan weights.", prop = note_style),
         fp_p = fp_par(text.align = "center"))
  )

print(doc, target = here("7_figures", "output", "figure2", "Figure2A.docx"))


# ---- 2B. Line Plot  ----
inc_data <- read_csv(here("5_calculate_gini", "output", "inc_by_year.csv"))

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
    Label = case_when(
      Unit == "r_hh_w_inc" ~ sprintf("HH Avg. = %.2f", Gini),
      Unit == "r_cl_w_inc" ~ sprintf("Clan Avg. = %.2f", Gini)
    ),
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
    title = "Figure 2B. Gini Coefficient by Year",
    x = "Year", y = "Gini Coefficient", linetype = "Unit"
    ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

plot <- plot_grid(
  plot,
  ggdraw() + draw_label("Note: Estimates weighted using PSID family and clan weights.",
                        fontfamily = "serif", fontface = "italic", size = 10,
                        x = 0.5, hjust = 0.5),
  ncol = 1, rel_heights = c(1, 0.05)
)

ggsave(here("7_figures", "output", "figure2", "Figure2B.pdf"),
       plot = plot, width = 9, height = 7)


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
    title = paste0("Figure 2C. Lorenz Curves — Income (", paste(years_income, collapse = ", "), ")"),
    x = "Cumulative Proportion of Units",
    y = "Cumulative Proportion of Income",
    color = "Year", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

lorenz <- plot_grid(
  lorenz,
  ggdraw() + draw_label("Note: Estimates weighted using PSID family and clan weights.",
                        fontfamily = "serif", fontface = "italic", size = 10,
                        x = 0.5, hjust = 0.5),
  ncol = 1, rel_heights = c(1, 0.05)
)

ggsave(here("7_figures", "output", "figure2", "Figure2C.pdf"),
       plot = lorenz, width = 8, height = 6)



# FIGURE 3 - WEALTH -------------------------------------------------------------------------------------------------------------
# ---- 3A. Wealth Table ----
summary <- read_csv(here("6_summary", "output", "summary_statistics.csv"))

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
  set_caption("Figure 3A. Wealth Table") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(ftext("Note: Estimates weighted using PSID family and clan weights.", prop = note_style),
         fp_p = fp_par(text.align = "center"))
  )

print(doc, target = here("7_figures", "output", "figure3", "Figure3A.docx"))


# ---- 3B. Line Plot  ----
inc_data <- read_csv(here("5_calculate_gini", "output", "wealth_by_year.csv"))

# Data
yearly_plot <- inc_data %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year))

# Grab ALL values for labeling averages
all_vals <- inc_data %>%
  filter(year == "ALL") %>%
  select(r_hh_w_wealth, r_cl_w_wealth) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "Unit",
                      values_to = "Gini") %>%
  mutate(
    Label = case_when(
      Unit == "r_hh_w_wealth" ~ sprintf("HH Avg. = %.2f", Gini),
      Unit == "r_cl_w_wealth" ~ sprintf("Clan Avg. = %.2f", Gini)
    ),
    x = max(yearly_plot$year) + 1,
    y = Gini
  )

# Position the average labels
all_vals <- all_vals %>%
  mutate(y = case_when(
    Unit == "r_hh_w_wealth" ~ Gini + 0.8,  # Household label a bit higher
    Unit == "r_cl_w_wealth" ~ Gini + 0.8,  # Clan label even higher
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
    expand = expansion(mult = c(0, 0.15))  # add 15% space to the right
    ) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = "Figure 3B. Gini Coefficient by Year",
    x = "Year", y = "Gini Coefficient", linetype = "Unit"
    ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

plot <- plot_grid(
  plot,
  ggdraw() + draw_label("Note: Estimates weighted using PSID family and clan weights.",
                        fontfamily = "serif", fontface = "italic", size = 10,
                        x = 0.5, hjust = 0.5),
  ncol = 1, rel_heights = c(1, 0.05)
)

ggsave(here("7_figures", "output", "figure3", "Figure3B.pdf"),
       plot = plot, width = 9, height = 7)


# ---- 3C. Lorenz Curve (orange, combined panel) ----
r_hh_wealth <- r_hh %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

r_clans_wealth <- r_clans %>%
  filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2))) 

years_wealth <- c(1989, 2019)
w_hh   <- get_lorenz_weighted(r_hh_wealth,    "wealth_nohouse", "fam_weight",  years_wealth, "Household")
w_clan <- get_lorenz_weighted(r_clans_wealth, "wealth_nohouse", "clan_weight", years_wealth, "Clan")
w_all  <- bind_rows(w_hh, w_clan)

lorenz <- ggplot(
  w_all,
  aes(x = p, y = L,
      color = factor(year),      # color only by year
      linetype = Unit,           # solid vs dashed by unit
      group = interaction(year, Unit))
) +
  geom_line(size = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("1989" = "#4a71c7", "2019" = "#E66101")) +  # orange shades
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = paste0("Figure 3C. Lorenz Curves — Wealth (", paste(years_income, collapse = ", "), ")"),
    x = "Cumulative Proportion of Units",
    y = "Cumulative Proportion of Wealth",
    color = "Year", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

lorenz <- plot_grid(
  lorenz,
  ggdraw() + draw_label("Note: Estimates weighted using PSID family and clan weights.",
                        fontfamily = "serif", fontface = "italic", size = 10,
                        x = 0.5, hjust = 0.5),
  ncol = 1, rel_heights = c(1, 0.05)
)

ggsave(here("7_figures", "output", "figure3", "Figure3C.pdf"),
       plot = lorenz, width = 8, height = 6)



# FIGURE 4 - BY RACE ----------------------------------------------------------------------------------------------------
   # ---- 4A. Race Table ----
summary <- read_csv(here("6_summary", "output", "summary_statistics.csv"))
race_gini <- read_csv(here("5_calculate_gini", "output", "inc_by_year_race.csv"))

# Get ALL row from race_gini for average Ginis
race_all <- race_gini %>% filter(year == "ALL")

table <- tibble(
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
    `Black % Wtd.` = sprintf("%.1f%%", `Black % Wtd.` * 1),  # ensure percent
    `Gini Black` = sprintf("%.3f", `Gini Black`),
    `Gini Non-Black` = sprintf("%.3f", `Gini Non-Black`)
  )

ft <- flextable(table) |>
  set_caption("Figure 4A. Income Inequality by Race") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(
      ftext("Note: Estimates weighted using PSID family and clan weights.", 
            prop = fp_text(italic = TRUE, font.size = 10)),
      fp_p = fp_par(text.align = "center")
    )
  )

print(doc, target = here("7_figures", "output", "figure4", "Figure4A.docx"))

# ---- 4B. Race Line Plot ----
race_data <- read_csv(here("5_calculate_gini", "output", "inc_by_year_race.csv"))

yearly_plot <- race_data %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year))

# Adjust ALL labels with spacing
all_vals <- race_data %>%
  filter(year == "ALL") %>%
  select(r_hh_w_inc_black, r_hh_w_inc_nonblack, 
         r_cl_w_inc_black, r_cl_w_inc_nonblack) %>%
  pivot_longer(everything(), names_to = "Series", values_to = "Gini") %>%
  mutate(
    Label = case_when(
      Series == "r_hh_w_inc_black"     ~ sprintf("HH Black Avg. = %.2f", Gini),
      Series == "r_hh_w_inc_nonblack" ~ sprintf("HH Non-Black Avg. = %.2f", Gini),
      Series == "r_cl_w_inc_black"    ~ sprintf("Clan Black Avg. = %.2f", Gini),
      Series == "r_cl_w_inc_nonblack" ~ sprintf("Clan Non-Black Avg. = %.2f", Gini)
    ),
    Unit = ifelse(grepl("^r_hh", Series), "Household", "Clan"),
    Race = ifelse(grepl("nonblack", Series), "Non-Black", "Black"),
    Linetype = ifelse(Unit == "Household", "solid", "dotted"),
    Color = ifelse(Race == "Black", "#1b9e77", "#E66101"),
    x = max(yearly_plot$year) + 1,   # just past last year
    y = case_when(
      # Households: a bit above their line
      grepl("hh", Series) ~ Gini + 0.07,
      # Clans: right on line or slightly above
      grepl("cl", Series) ~ Gini + 0.06,
      TRUE ~ Gini
    )
  )



# Build plot
plot <- ggplot(yearly_plot, aes(x = year)) +
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
    expand = expansion(mult = c(0, 0.3))   # more room on right side
  ) +
  scale_color_manual(values = c("Black" = "#1b9e77", "Non-Black" = "#E66101")) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = "Figure 4B. Gini Coefficient by Race and Unit",
    x = "Year", y = "Gini Coefficient",
    color = "Race", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")


plot <- plot_grid(
  plot,
  ggdraw() + draw_label("Note: Estimates weighted using PSID family and clan weights.",
                        fontfamily = "serif", fontface = "italic", size = 10,
                        x = 0.5, hjust = 0.5),
  ncol = 1, rel_heights = c(1, 0.05)
)

ggsave(here("7_figures", "output", "figure4", "Figure4B.pdf"),
       plot = plot, width = 9, height = 7)


# ---- 4C. Wealth Table ----
summary <- read_csv(here("6_summary", "output", "summary_statistics.csv"))
wealth_gini <- read_csv(here("5_calculate_gini", "output", "wealth_by_year_race.csv"))

# Get ALL row from wealth_gini for average Ginis
wealth_all <- wealth_gini %>% filter(year == "ALL")

table <- tibble(
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

ft <- flextable(table) |>
  set_caption("Figure 4C. Wealth Inequality by Race") |>
  autofit() |>
  theme_vanilla() |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "body") |>
  align(align = "center", part = "all")

doc <- read_docx() |>
  body_add_flextable(ft) |>
  body_add_fpar(
    fpar(
      ftext("Note: Estimates weighted using PSID family and clan weights. Wealth excludes home equity.", 
            prop = fp_text(italic = TRUE, font.size = 10)),
      fp_p = fp_par(text.align = "center")
    )
  )

print(doc, target = here("7_figures", "output", "figure4", "Figure4C.docx"))


# ---- 4D. Wealth Line Plot ----
wealth_data <- read_csv(here("5_calculate_gini", "output", "wealth_by_year_race.csv"))

yearly_plot <- wealth_data %>%
  filter(year != "ALL") %>%
  mutate(year = as.numeric(year))

# Grab ALL row for labels
all_vals <- wealth_data %>%
  filter(year == "ALL") %>%
  select(r_hh_w_wealth_black, r_hh_w_wealth_nonblack, 
         r_cl_w_wealth_black, r_cl_w_wealth_nonblack) %>%
  pivot_longer(everything(), names_to = "Series", values_to = "Gini") %>%
  mutate(
    Label = case_when(
      Series == "r_hh_w_wealth_black"     ~ sprintf("HH Black Avg. = %.2f", Gini),
      Series == "r_hh_w_wealth_nonblack" ~ sprintf("HH Non-Black Avg. = %.2f", Gini),
      Series == "r_cl_w_wealth_black"    ~ sprintf("Clan Black Avg. = %.2f", Gini),
      Series == "r_cl_w_wealth_nonblack" ~ sprintf("Clan Non-Black Avg. = %.2f", Gini)
    ),
    Unit = ifelse(grepl("^r_hh", Series), "Household", "Clan"),
    Race = ifelse(grepl("nonblack", Series), "Non-Black", "Black"),
    Linetype = ifelse(Unit == "Household", "solid", "dotted"),
    Color = ifelse(Race == "Black", "#1b9e77", "#E66101"),
    x = max(yearly_plot$year) + 1,
    y = case_when(
      grepl("hh", Series) ~ Gini + 0.02,  # HH labels slightly above
      grepl("cl", Series) ~ Gini + 0.01,  # Clan labels almost aligned
      TRUE ~ Gini
    )
  )

# Build plot
plot <- ggplot(yearly_plot, aes(x = year)) +
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
    expand = expansion(mult = c(0, 0.3))   # extra room for labels
  ) +
  scale_color_manual(values = c("Black" = "#1b9e77", "Non-Black" = "#E66101")) +
  scale_linetype_manual(values = c("Household" = "solid", "Clan" = "dotted")) +
  labs(
    title = "Figure 4D. Gini Coefficient of Wealth by Race and Unit",
    x = "Year", y = "Gini Coefficient",
    color = "Race", linetype = "Unit"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "bottom")

plot <- plot_grid(
  plot,
  ggdraw() + draw_label("Note: Estimates weighted using PSID family and clan weights. Wealth excludes home equity.",
                        fontfamily = "serif", fontface = "italic", size = 10,
                        x = 0.5, hjust = 0.5),
  ncol = 1, rel_heights = c(1, 0.05)
)

ggsave(here("7_figures", "output", "figure4", "Figure4D.pdf"),
       plot = plot, width = 9, height = 7)
