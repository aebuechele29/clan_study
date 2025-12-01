# LOAD DATA ------------------------------------------------------------------
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))


# Calculate Gini after cutting different parts of the distribution
inc_all_ginis <- gini_by_dist(r_hh, inc_all, cutoff = 0.10) # Excludes top 10% and bottom 10%
wealth_nohouse_ginis <- gini_by_dist(r_hh_wealth, wealth_nohouse, cutoff = 0.10) # Excludes top 10% and bottom 10%
wealth_withhome_ginis <- gini_by_dist(r_hh_wealth, wealth, cutoff = 0.10) # Excludes top 10% and bottom 10%


# Function to plot Ginis by distribution cutoff
plot_gini_by_dist <- function(gini_df, title, filename,
                              out_dir = here("6_calculate_gini", "output", "by_dist_gini", "10pct")) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  gini_cols <- intersect(names(gini_df),
                         c("gini_all", "gini_ex_top_10", "gini_ex_bottom_10"))

  df_long <- gini_df %>%
    filter(year != "ALL") %>%
    mutate(year = suppressWarnings(as.integer(year))) %>%
    select(year, all_of(gini_cols)) %>%
    tidyr::pivot_longer(-year, names_to = "series", values_to = "gini") %>%
    mutate(
      series = dplyr::recode(
        series,
        "gini_all" = "All",
        "gini_ex_top_10" = "Exclude Top 10%",
        "gini_ex_bottom_10" = "Exclude Bottom 10%"
      ),
      series = factor(series, levels = c("All", "Exclude Top 10%", "Exclude Bottom 10%"))
    )

  p <- ggplot2::ggplot(df_long, aes(x = year, y = gini, color = series)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(title = title, x = "Year", y = "Gini", color = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  filepath <- file.path(out_dir, filename)

  # Debug from ChatGPT: Try Cairo PDF; if it fails (missing X11), fall back to native PDF
  ok <- TRUE
  tryCatch({
    ggplot2::ggsave(filename = filepath, plot = p,
                    device = cairo_pdf, width = 8, height = 5, dpi = 300)
  }, error = function(e) {
    ok <<- FALSE
  }, warning = function(w) {
    ok <<- FALSE
  })

  if (!ok) {
    ggplot2::ggsave(filename = filepath, plot = p,
                    device = "pdf", width = 8, height = 5, dpi = 300)
  }

  p
}


# Income
inc_all_ginis <- gini_by_dist(r_hh, inc_all, cutoff = 0.10)
plot_gini_by_dist(
  gini_df = inc_all_ginis,
  title   = "Income Gini by Distribution Cutoff (HH)",
  filename= "income.pdf"
)

# Wealth (excluding home equity)
wealth_nohouse_ginis <- gini_by_dist(r_hh_wealth, wealth_nohouse, cutoff = 0.10)
plot_gini_by_dist(
  gini_df = wealth_nohouse_ginis,
  title   = "Wealth (Excl. Home) Gini by Distribution Cutoff (HH)",
  filename= "wealth_nohouse.pdf"
)

# Wealth (including home equity)
wealth_withhome_ginis <- gini_by_dist(r_hh_wealth, wealth, cutoff = 0.10)
plot_gini_by_dist(
  gini_df = wealth_withhome_ginis,
  title   = "Wealth (Incl. Home) Gini by Distribution Cutoff (HH)",
  filename= "wealth_withhome.pdf"
)






# Calculate Gini after cutting different parts of the distribution
inc_all_ginis <- gini_by_dist(r_hh, inc_all, cutoff = 0.50) 
wealth_nohouse_ginis <- gini_by_dist(r_hh_wealth, wealth_nohouse, cutoff = 0.50) 
wealth_withhome_ginis <- gini_by_dist(r_hh_wealth, wealth, cutoff = 0.50) 


# Function to plot Ginis by distribution cutoff
plot_gini_by_dist <- function(gini_df, title, filename,
                              out_dir = here("6_calculate_gini", "output", "by_dist_gini", "50pct")) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  gini_cols <- intersect(names(gini_df),
                         c("gini_all", "gini_ex_top_50", "gini_ex_bottom_50"))

  df_long <- gini_df %>%
    filter(year != "ALL") %>%
    mutate(year = suppressWarnings(as.integer(year))) %>%
    select(year, all_of(gini_cols)) %>%
    tidyr::pivot_longer(-year, names_to = "series", values_to = "gini") %>%
    mutate(
      series = dplyr::recode(
        series,
        "gini_all" = "All",
        "gini_ex_top_50" = "Exclude Top 50%",
        "gini_ex_bottom_50" = "Exclude Bottom 50%"
      ),
      series = factor(series, levels = c("All", "Exclude Top 50%", "Exclude Bottom 50%"))
    )

  p <- ggplot2::ggplot(df_long, aes(x = year, y = gini, color = series)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(title = title, x = "Year", y = "Gini", color = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  filepath <- file.path(out_dir, filename)

  # Debug from ChatGPT: Try Cairo PDF; if it fails (missing X11), fall back to native PDF
  ok <- TRUE
  tryCatch({
    ggplot2::ggsave(filename = filepath, plot = p,
                    device = cairo_pdf, width = 8, height = 5, dpi = 300)
  }, error = function(e) {
    ok <<- FALSE
  }, warning = function(w) {
    ok <<- FALSE
  })

  if (!ok) {
    ggplot2::ggsave(filename = filepath, plot = p,
                    device = "pdf", width = 8, height = 5, dpi = 300)
  }

  p
}


# Income
inc_all_ginis <- gini_by_dist(r_hh, inc_all, cutoff = 0.50)
plot_gini_by_dist(
  gini_df = inc_all_ginis,
  title   = "Income Gini by Distribution Cutoff (HH)",
  filename= "income.pdf"
)

# Wealth (excluding home equity)
wealth_nohouse_ginis <- gini_by_dist(r_hh_wealth, wealth_nohouse, cutoff = 0.50)
plot_gini_by_dist(
  gini_df = wealth_nohouse_ginis,
  title   = "Wealth (Excl. Home) Gini by Distribution Cutoff (HH)",
  filename= "wealth_nohouse.pdf"
)

# Wealth (including home equity)
wealth_withhome_ginis <- gini_by_dist(r_hh_wealth, wealth, cutoff = 0.50)
plot_gini_by_dist(
  gini_df = wealth_withhome_ginis,
  title   = "Wealth (Incl. Home) Gini by Distribution Cutoff (HH)",
  filename= "wealth_withhome.pdf"
)

