# ===============================================================
# Figure: Income and Wealth Inequality Over Time
# ===============================================================

# Packages
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(tidyverse, cowplot, here)

# Data
inc_data    <- read_csv(here("5_calculate_gini", "output", "inc_by_year.csv"))
wealth_data <- read_csv(here("5_calculate_gini", "output", "wealth_by_year.csv"))

# Remove "ALL" and ensure year is numeric
inc_yearly <- inc_data %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))
wealth_yearly <- wealth_data %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))

# Function to label last year values
get_last_labels <- function(df, series, label_name) {
  df %>%
    slice_tail(n = 1) %>%
    transmute(
      x = year + 1,
      y = .data[[series]],
      Label = sprintf("%.2f", .data[[series]]),
      Series = label_name
    )
}

# Colors
col_house <- "black"
col_clan  <- "#B6ACD1"

# ----------------- Income plot -----------------
labels_inc <- bind_rows(
  get_last_labels(inc_yearly, "r_hh_w_inc", "Household"),
  get_last_labels(inc_yearly, "r_cl_w_inc", "Clan")
)

p_inc <- ggplot(inc_yearly, aes(x = year)) +
  geom_line(aes(y = r_hh_w_inc, color = "Household", linetype = "Weighted"), size = 0.9) +
  geom_line(aes(y = r_hh_u_inc, color = "Household", linetype = "Unweighted"), size = 0.9) +
  geom_line(aes(y = r_cl_w_inc, color = "Clan", linetype = "Weighted"), size = 0.9) +
  geom_line(aes(y = r_cl_u_inc, color = "Clan", linetype = "Unweighted"), size = 0.9) +
  geom_text(data = labels_inc,
            aes(x = x, y = y, label = Label, color = Series),
            inherit.aes = FALSE, hjust = 0, family = "serif", size = 3.2) +
  scale_color_manual(values = c("Household" = "black", "Clan" = "#B6ACD1")) +
  scale_linetype_manual(values = c("Weighted" = "solid", "Unweighted" = "dotted")) +
  scale_y_continuous(limits = c(0.25, 1)) +
  scale_x_continuous(
    breaks = seq(min(inc_yearly$year), max(inc_yearly$year), by = 5),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Income",
    x = "Year", y = "Gini Coefficient", color = "Unit", linetype = ""
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "none")  # suppress legend here

# ----------------- Wealth plot -----------------
labels_wealth <- bind_rows(
  get_last_labels(wealth_yearly, "r_hh_w_wealth", "Household"),
  get_last_labels(wealth_yearly, "r_cl_w_wealth", "Clan")
)

p_wealth <- ggplot(wealth_yearly, aes(x = year)) +
  geom_line(aes(y = r_hh_w_wealth, color = "Household", linetype = "Weighted"), size = 0.9) +
  geom_line(aes(y = r_hh_u_wealth, color = "Household", linetype = "Unweighted"), size = 0.9) +
  geom_line(aes(y = r_cl_w_wealth, color = "Clan", linetype = "Weighted"), size = 0.9) +
  geom_line(aes(y = r_cl_u_wealth, color = "Clan", linetype = "Unweighted"), size = 0.9) +
  geom_text(data = labels_wealth,
            aes(x = x, y = y, label = Label, color = Series),
            inherit.aes = FALSE, hjust = 0, family = "serif", size = 3.2) +
  scale_color_manual(values = c("Household" = "black", "Clan" = "#B6ACD1")) +
  scale_linetype_manual(values = c("Weighted" = "solid", "Unweighted" = "dotted")) +
  scale_y_continuous(limits = c(0.25, 1)) +
  scale_x_continuous(
    breaks = seq(min(wealth_yearly$year), max(wealth_yearly$year), by = 5),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Wealth",
    x = "Year", y = "Gini Coefficient", color = "Unit", linetype = ""
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "right")  # keep single legend here

# ----------------- Extract legend -----------------
legend <- get_legend(p_wealth)

p_wealth_clean <- p_wealth + theme(legend.position = "none")

# ----------------- Combine -----------------
legend_tight <- ggdraw(legend) + theme(plot.margin = margin(0, 0, 0, 0))

combined <- plot_grid(
  p_inc, p_wealth_clean, legend_tight,
  ncol = 3,
  rel_widths = c(1, 1, 0.3)  # very narrow legend
)

ggsave(
  here("10_poster", "figure2.pdf"),
  plot = combined, width = 14, height = 7
)


# ===============================================================
# Figure: Income and Wealth Inequality by Race
# ===============================================================

# Packages
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(tidyverse, cowplot, here)

# Data
inc_data    <- read_csv(here("5_calculate_gini", "output", "inc_by_year_race.csv"))
wealth_data <- read_csv(here("5_calculate_gini", "output", "wealth_by_year_race.csv"))

# Remove "ALL" and ensure year is numeric
inc_yearly    <- inc_data %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))
wealth_yearly <- wealth_data %>% filter(year != "ALL") %>% mutate(year = as.numeric(year))

# Function to label last year values
get_last_labels <- function(df, series, label_name) {
  df %>%
    slice_tail(n = 1) %>%
    transmute(
      x = year + 1,
      y = .data[[series]],
      Label = sprintf("%.2f", .data[[series]]),
      Series = label_name
    )
}

# Colors
col_house <- "black"
col_clan  <- "#B6ACD1"

# ----------------- Income plot -----------------
labels_inc <- bind_rows(
  get_last_labels(inc_yearly, "r_hh_w_inc_black",    "Household Black"),
  get_last_labels(inc_yearly, "r_hh_w_inc_nonblack", "Household Non-Black"),
  get_last_labels(inc_yearly, "r_cl_w_inc_black",    "Clan Black"),
  get_last_labels(inc_yearly, "r_cl_w_inc_nonblack", "Clan Non-Black")
)

p_inc <- ggplot(inc_yearly, aes(x = year)) +
  geom_line(aes(y = r_hh_w_inc_black,    color = "Household", linetype = "Black"), size = 0.9) +
  geom_line(aes(y = r_hh_w_inc_nonblack, color = "Household", linetype = "Non-Black"), size = 0.9) +
  geom_line(aes(y = r_cl_w_inc_black,    color = "Clan",      linetype = "Black"), size = 0.9) +
  geom_line(aes(y = r_cl_w_inc_nonblack, color = "Clan",      linetype = "Non-Black"), size = 0.9) +
  geom_text(data = labels_inc,
            aes(x = x, y = y, label = Label, color = ifelse(grepl("Household", Series), "Household", "Clan")),
            inherit.aes = FALSE, hjust = 0, family = "serif", size = 3.2) +
  scale_color_manual(values = c("Household" = col_house, "Clan" = col_clan)) +
  scale_linetype_manual(values = c("Black" = "solid", "Non-Black" = "dotted")) +
  scale_y_continuous(limits = c(.25, 1.5)) +
  scale_x_continuous(
    breaks = seq(min(inc_yearly$year), max(inc_yearly$year), by = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Income",
    x = "Year", y = "Gini Coefficient",
    color = "Unit", linetype = "Population"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "none")

# ----------------- Wealth plot -----------------
labels_wealth <- bind_rows(
  get_last_labels(wealth_yearly, "r_hh_w_wealth_black",    "Household Black"),
  get_last_labels(wealth_yearly, "r_hh_w_wealth_nonblack", "Household Non-Black"),
  get_last_labels(wealth_yearly, "r_cl_w_wealth_black",    "Clan Black"),
  get_last_labels(wealth_yearly, "r_cl_w_wealth_nonblack", "Clan Non-Black")
)

p_wealth <- ggplot(wealth_yearly, aes(x = year)) +
  geom_line(aes(y = r_hh_w_wealth_black,    color = "Household", linetype = "Black"), size = 0.9) +
  geom_line(aes(y = r_hh_w_wealth_nonblack, color = "Household", linetype = "Non-Black"), size = 0.9) +
  geom_line(aes(y = r_cl_w_wealth_black,    color = "Clan",      linetype = "Black"), size = 0.9) +
  geom_line(aes(y = r_cl_w_wealth_nonblack, color = "Clan",      linetype = "Non-Black"), size = 0.9) +
  geom_text(data = labels_wealth,
            aes(x = x, y = y, label = Label, color = ifelse(grepl("Household", Series), "Household", "Clan")),
            inherit.aes = FALSE, hjust = 0, family = "serif", size = 3.2) +
  scale_color_manual(values = c("Household" = col_house, "Clan" = col_clan)) +
  scale_linetype_manual(values = c("Black" = "solid", "Non-Black" = "dotted")) +
  scale_y_continuous(limits = c(.25, 1.5)) +
  scale_x_continuous(
    breaks = seq(min(wealth_yearly$year), max(wealth_yearly$year), by = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Wealth",
    x = "Year", y = "Gini Coefficient",
    color = "Unit", linetype = "Population"
  ) +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(legend.position = "right")

# ----------------- Extract legend -----------------
legend <- get_legend(p_wealth)
p_wealth_clean <- p_wealth + theme(legend.position = "none")

# ----------------- Combine -----------------
legend_tight <- ggdraw(legend) + theme(plot.margin = margin(0, 0, 0, 0))

combined <- plot_grid(
  p_inc, p_wealth_clean, legend_tight,
  ncol = 3,
  rel_widths = c(1, 1, 0.3)  # very narrow legend
)

ggsave(
  here("10_poster", "figure3.pdf"),
  plot = combined, width = 14, height = 7
)
