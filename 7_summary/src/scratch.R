
# CREATE PLOTS FOR INCOME AND WEALTH OVER TIME -----------------------------------
# --- FUNCTION FOR WEIGHTED QUINTILES + TOP 5% ----------------------------------------
get_quints <- function(design, var, year) {
  qs <- svyquantile(
    as.formula(paste0("~", var)),
    design,
    quantiles = c(0.2, 0.4, 0.6, 0.8, 1.0, 0.95),  # quintiles + top 5%
    na.rm = TRUE,
    ci = FALSE
  )
  tibble(
    group = c("Q1", "Q2", "Q3", "Q4", "Q5", "Top 5%"),
    value = as.numeric(qs[[1]]),
    year  = year
  )
}

# --- CALCULATE FOR HOUSEHOLDS AND CLANS -------------------------------------
hh_quints <- r_hh %>%
  group_split(year) %>%
  setNames(unique(r_hh$year)) %>%
  map(~ svydesign(
    ids     = ~cluster,
    strata  = ~stratum,
    weights = ~fam_weight,
    data    = .x,
    nest    = TRUE
  )) %>%
  map2_dfr(names(.), ~ get_quints(.x, "inc_all", as.integer(.y))) %>%
  mutate(unit = "Household")

clan_quints <- r_clans %>%
  group_split(year) %>%
  setNames(unique(r_clans$year)) %>%
  map(~ svydesign(
    ids     = ~cluster,
    strata  = ~stratum,
    weights = ~clan_weight,
    data    = .x,
    nest    = TRUE
  )) %>%
  map2_dfr(names(.), ~ get_quints(.x, "inc_all", as.integer(.y))) %>%
  mutate(unit = "Clan")

quints_all <- bind_rows(hh_quints, clan_quints)

# Label endpoints
labels_2021 <- quints_all %>%
  filter(year == 2021) %>%
  mutate(label = scales::dollar(value, accuracy = 1))

# Plot
p_quints <- ggplot(quints_all, aes(
  x = year, y = value,
  color = group,
  group = interaction(group, unit)
)) +
  geom_smooth(
    data = filter(quints_all, unit == "Household"),
    se = FALSE, size = 0.8
  ) +
  geom_line(
    data = filter(quints_all, unit == "Clan"),
    size = 0.8
  ) +
  geom_text(
    data = labels_2021,
    aes(label = label),
    hjust = -0.1, vjust = 0.5, size = 3,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    limits = c(0, 1000000)   # up to $1M
  ) +
  labs(
    title = "Weighted Income Quintiles + Top 5%, 1969–2021",
    x = "Year",
    y = "Income (weighted $)",
    color = "Group"
  ) +
  facet_wrap(~ unit, nrow = 1) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  ) +
  xlim(min(quints_all$year), max(quints_all$year) + 2)  


ggsave(
  filename = here("7_summary", "output", "income_quintiles_top5.pdf"),
  plot = p_quints,
  width = 12,
  height = 5
)
