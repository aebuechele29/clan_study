# LOAD DATA ------------------------------------------------------------------
r_clans <- readRDS(here("4_clans", "output", "robust_clans.rds"))
r_clans_wealth <- readRDS(here("4_clans", "output", "robust_clans_wealth.rds"))

options(scipen = 999)   # no scientific notation


# Income vs Clan Size
p1 <- ggplot(r_clans, aes(x = inc_all, y = numclan)) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE,
    linewidth = 1.1
  ) +
  labs(
    title = "Income vs. Clan Size",
    x = "Income",
    y = "Clan Size (# Households)"
  ) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_minimal(base_size = 14)


# Log Income vs Clan Size
p2 <- ggplot(r_clans, aes(x = inc_all, y = numclan)) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE,
    linewidth = 1.1
  ) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Income vs. Clan Size (Log Income)",
    x = "Income (log scale)",
    y = "Clan Size (# Households)"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal(14)


# Wealth vs Clan Size
p3 <- ggplot(r_clans_wealth, aes(x = numclan, y = wealth_nohouse)) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = FALSE,
    linewidth = 1.1
  ) +
  labs(
    title = "Wealth (No Home Equity) vs. Clan Size",
    x = "Clan Size (# Households)",
    y = "Wealth"
  ) +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  theme_minimal(base_size = 14)

# Plot frequency of clan sizes
clan_unique <- r_clans %>%
  distinct(id1968, numclan)

p4 <- ggplot(clan_unique, aes(x = factor(numclan))) +
  geom_bar(fill = "darkorange", color = "white") +
  labs(
    title = "Frequency of Clan Sizes (Unique Clans Only)",
    x = "Clan Size (# Households)",
    y = "Number of Clans"
  ) +
  theme_minimal(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export
pdf(
  file = here("8_clan_size", "output", "clan_size_plots.pdf"),
  width = 8,
  height = 6
)

print(p1)
print(p2)
print(p3)
print(p4)

dev.off()



