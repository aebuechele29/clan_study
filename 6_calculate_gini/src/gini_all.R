# LOAD DATA ------------------------------------------------------------------
hh <- readRDS(here("3_households", "output", "households.rds"))
clans <- readRDS(here("4_clans", "output", "clans.rds"))
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))

hh_wealth <- readRDS(here("3_households", "output", "households_wealth.rds"))
clans_wealth <- readRDS(here("4_clans", "output", "clans_wealth.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))


# Adjust weighted designs for lonely PSUs
options(survey.lonely.psu = "adjust")


# CALCULATE GINIS ---------------------------------------------------------------
  # For each variable, choose whether to use the:
    # robust version of the data (r_*)
    # weighted or unweighted version w_ or u_
  # and calculate for HH and Clans

# Income
inc_dfs <- list(
  # Households
    # run_gini(hh,      "inc_all",        NULL,         FALSE, FALSE, "hh_u_inc"),
    run_gini(hh,      "inc_all",        "fam_weight", FALSE, TRUE,  "hh_w_inc"),
    # run_gini(r_hh,    "inc_all",        NULL,         FALSE, FALSE, "r_hh_u_inc"),
    run_gini(r_hh,    "inc_all",        "fam_weight", FALSE, TRUE,  "r_hh_w_inc"),
  
  # Clans - means
    # run_gini(clans,   "inc_all_mean",   NULL,         TRUE,  FALSE, "cl_u_inc_mean"),
    # run_gini(clans, "inc_all_mean",   "clan_weight",         FALSE,  TRUE, "cl_w_inc_mean"),
    # run_gini(r_clans, "inc_all_mean",   NULL,         TRUE,  FALSE, "r_cl_u_inc_mean"),
    # run_gini(r_clans, "inc_all_mean",   "clan_weight",         FALSE,  TRUE, "r_cl_w_inc_mean"),

  # Clans - totals
    # run_gini(clans,   "inc_all",   NULL,         TRUE,  FALSE, "cl_u_inc"),
    run_gini(clans, "inc_all",   "clan_weight",         FALSE,  TRUE, "cl_w_inc"),
    # run_gini(r_clans, "inc_all",   NULL,         TRUE,  FALSE, "r_cl_u_inc"),
    run_gini(r_clans, "inc_all",   "clan_weight",         FALSE,  TRUE, "r_cl_w_inc")
)

inc_by_year <- reduce(inc_dfs, full_join, by = "year") %>% arrange(year)
inc_by_year <- append_mean_row(inc_by_year)
write.csv(inc_by_year, here("6_calculate_gini", "output", "all_ginis", "income.csv"), row.names = FALSE)



# Wealth (excluding home equity)
wealth_nohouse_dfs <- list(
  # Households
    # run_gini(hh_wealth,   "wealth_nohouse",        NULL,         FALSE, FALSE, "hh_u_wealth"),
    run_gini(hh_wealth,   "wealth_nohouse",        "fam_weight", FALSE, TRUE,  "hh_w_wealth"),
    # run_gini(r_hh_wealth, "wealth_nohouse",        NULL,         FALSE, FALSE, "r_hh_u_wealth"),
    run_gini(r_hh_wealth, "wealth_nohouse",        "fam_weight", FALSE, TRUE,  "r_hh_w_wealth"),
  
  # Clans - means
    # run_gini(clans_wealth,   "wealth_nohouse_mean",   NULL,         TRUE,  FALSE, "cl_u_wealth_mean"),
    # run_gini(clans_wealth, "wealth_nohouse_mean",   "clan_weight",         FALSE,  TRUE, "cl_w_wealth_mean"),
    # run_gini(r_clans_wealth, "wealth_nohouse_mean",   NULL,         TRUE,  FALSE, "r_cl_u_wealth_mean"),
    # run_gini(r_clans_wealth, "wealth_nohouse_mean",   "clan_weight",         FALSE,  TRUE, "r_cl_w_wealth_mean"),

  # Clans - totals
    # run_gini(clans_wealth,   "wealth_nohouse",   NULL,         TRUE,  FALSE, "cl_u_wealth"),
    run_gini(clans_wealth, "wealth_nohouse",   "clan_weight",         FALSE,  TRUE, "cl_w_wealth"),
    # run_gini(r_clans_wealth, "wealth_nohouse",   NULL,         TRUE,  FALSE, "r_cl_u_wealth"),
    run_gini(r_clans_wealth, "wealth_nohouse",   "clan_weight",         FALSE,  TRUE, "r_cl_w_wealth")
)

wealth_by_year <- reduce(wealth_nohouse_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year_nohouse <- append_mean_row(wealth_by_year)
write.csv(wealth_by_year_nohouse, here("6_calculate_gini", "output", "all_ginis", "wealth_nohouse.csv"), row.names = FALSE)


# Wealth (including home equity)
wealth_dfs <- list(
  # Households
    # run_gini(hh_wealth,   "wealth",        NULL,         FALSE, FALSE, "hh_u_wealth"),
    run_gini(hh_wealth,   "wealth",        "fam_weight", FALSE, TRUE,  "hh_w_wealth"),
    # run_gini(r_hh_wealth, "wealth",        NULL,         FALSE, FALSE, "r_hh_u_wealth"),
    run_gini(r_hh_wealth, "wealth",        "fam_weight", FALSE, TRUE,  "r_hh_w_wealth"),
  
  # Clans - means
    # run_gini(clans_wealth,   "wealth_mean",   NULL,         TRUE,  FALSE, "cl_u_wealth_mean"),
    # run_gini(clans_wealth, "wealth_mean",   "clan_weight",         FALSE,  TRUE, "cl_w_wealth_mean"),
    # run_gini(r_clans_wealth, "wealth_mean",   NULL,         TRUE,  FALSE, "r_cl_u_wealth_mean"),
    # run_gini(r_clans_wealth, "wealth_mean",   "clan_weight",         FALSE,  TRUE, "r_cl_w_wealth_mean"),

  # Clans - totals
    # run_gini(clans_wealth,   "wealth",   NULL,         TRUE,  FALSE, "cl_u_wealth"),
    run_gini(clans_wealth, "wealth",   "clan_weight",         FALSE,  TRUE, "cl_w_wealth"),
    # run_gini(r_clans_wealth, "wealth",   NULL,         TRUE,  FALSE, "r_cl_u_wealth"),
    run_gini(r_clans_wealth, "wealth",   "clan_weight",         FALSE,  TRUE, "r_cl_w_wealth")
)

wealth_by_year <- reduce(wealth_dfs, full_join, by = "year") %>% arrange(year)
wealth_by_year <- append_mean_row(wealth_by_year)
write.csv(wealth_by_year, here("6_calculate_gini", "output", "all_ginis", "wealth_withhome.csv"), row.names = FALSE)




# PLOT AND COMPARE GINIS FOR DIFFERENT WEALTH VARIABLES
# Wealth (excluding home equity)
cols_nohouse <- pick_cols(
  wealth_by_year_nohouse,
  hh_vars = c("r_hh_w_wealth", "hh_w_wealth"),
  cl_vars = c("r_cl_w_wealth", "cl_w_wealth")
)
df_nohouse <- prep_plot_df(
  wealth_by_year_nohouse,
  hh_col   = cols_nohouse$hh,
  cl_col   = cols_nohouse$cl,
  title_lab= "Excl. Home Equity"
)

# Wealth (including home equity)
cols_withhome <- pick_cols(
  wealth_by_year,
  hh_vars = c("r_hh_w_wealth", "hh_w_wealth"),
  cl_vars = c("r_cl_w_wealth", "cl_w_wealth")
)
df_withhome <- prep_plot_df(
  wealth_by_year,
  hh_col   = cols_withhome$hh,
  cl_col   = cols_withhome$cl,
  title_lab= "Incl. Home Equity"
)

# Combine to plot
plot_df <- bind_rows(df_nohouse, df_withhome)

ylims <- range(plot_df$Gini, na.rm = TRUE)
pad   <- diff(ylims) * 0.03; if (!is.finite(pad)) pad <- 0
ylims <- c(ylims[1] - pad, ylims[2] + pad)

p <- ggplot(
  plot_df,
  aes(x = year, y = Gini,
      color = Unit,              
      linetype = PanelTitle,     
      group = interaction(Unit, PanelTitle))
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(limits = ylims) +
  labs(
    title = "Wealth Ginis Over Time — HH vs Clan (Excl. vs Incl. Home Equity)",
    x = "Year", y = "Gini", color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

out_dir <- here("6_calculate_gini", "output", "all_ginis", "compare_wealth")
ggsave(file.path(out_dir, "wealth_ginis_combined_single_plot.pdf"),
       plot = p, device = "pdf", width = 8.5, height = 6, dpi = 300)

p

# Create table comparing wealth variables
compare_wealth <- read.csv(here("6_calculate_gini", "output", "all_ginis", "wealth_nohouse.csv")) %>%
  select(year,
         r_hh_w_wealth_nohouse = r_hh_w_wealth,
         r_cl_w_wealth_nohouse = r_cl_w_wealth) %>%
  full_join(
    read.csv(here("6_calculate_gini", "output", "all_ginis", "wealth_withhome.csv")) %>%
      select(year,
             r_hh_w_wealth_withhome = r_hh_w_wealth,
             r_cl_w_wealth_withhome = r_cl_w_wealth),
    by = "year"
  ) %>%
  # reorder to show hh_w_wealth_nohouse then hh_w_wealth_withhome, etc.
  select(year,
         r_hh_w_wealth_nohouse,
         r_cl_w_wealth_nohouse,
         r_hh_w_wealth_withhome,
         r_cl_w_wealth_withhome) %>%
    # add a col after each pair showing the difference 
  mutate(
    diff_nohouse = r_hh_w_wealth_nohouse - r_cl_w_wealth_nohouse,
    diff_withhome = r_hh_w_wealth_withhome - r_cl_w_wealth_withhome,
    diff_wealth = diff_nohouse - diff_withhome
  ) %>%
  write.csv(here("6_calculate_gini", "output", "all_ginis", "compare_wealth", "wealth_ginis_comparison.csv"),
            row.names = FALSE)





# COMPARE SAMPLES
income <- read.csv(here("6_calculate_gini", "output", "all_ginis", "income.csv")) %>%
  select(year,
         hh_w_inc,
         r_hh_w_inc,
         cl_w_inc,
         r_cl_w_inc)  %>%
  filter(year != "ALL") %>%
  arrange(as.integer(year)) %>%
  mutate(
    all_hh_diff = hh_w_inc - cl_w_inc,
    robust_hh_diff = r_hh_w_inc - r_cl_w_inc,
    diff_samples = all_hh_diff - robust_hh_diff
  ) %>%
  write.csv(here("6_calculate_gini", "output", "all_ginis", "compare_samples", "income.csv"),
            row.names = FALSE)


wealth_nohouse <- read.csv(here("6_calculate_gini", "output", "all_ginis", "wealth_nohouse.csv")) %>%
  select(year,
         hh_w_wealth,
         r_hh_w_wealth,
         cl_w_wealth,
         r_cl_w_wealth) %>%
  filter(year != "ALL") %>%
  arrange(as.integer(year)) %>%
  mutate(
    all_hh_diff = hh_w_wealth - cl_w_wealth,
    robust_hh_diff = r_hh_w_wealth - r_cl_w_wealth,
    diff_samples = all_hh_diff - robust_hh_diff
  ) %>%
  write.csv(here("6_calculate_gini", "output", "all_ginis", "compare_samples", "wealth_nohouse.csv"),
            row.names = FALSE)


wealth_nohouse <- read.csv(here("6_calculate_gini", "output", "all_ginis", "wealth_withhome.csv")) %>%
  select(year,
         hh_w_wealth,
         r_hh_w_wealth,
         cl_w_wealth,
         r_cl_w_wealth) %>%
  filter(year != "ALL") %>%
  arrange(as.integer(year)) %>%
  mutate(
    all_hh_diff = hh_w_wealth - cl_w_wealth,
    robust_hh_diff = r_hh_w_wealth - r_cl_w_wealth,
    diff_samples = all_hh_diff - robust_hh_diff
  ) %>%
  write.csv(here("6_calculate_gini", "output", "all_ginis", "compare_samples", "wealth_withhome.csv"),
            row.names = FALSE)

