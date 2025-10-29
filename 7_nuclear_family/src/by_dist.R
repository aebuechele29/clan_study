# LOAD DATA ------------------------------------------------------------------
r_hh <- readRDS(here("3_households", "output", "robust_households.rds"))
r_hh_wealth <- readRDS(here("3_households", "output", "robust_households_wealth.rds"))


# Plot three panels (All, Exclude Top 10%, Exclude Bottom 10%) on one PDF page for C1, C2, and C3
plot_C123_by_dist <- function(c123_df, title, filename,
                              out_dir = here("7_nuclear_family", "output", "by_dist", "10pct")) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  cols_all    <- intersect(names(c123_df), c("C1_all","C2_all","C3_all"))
  cols_top    <- intersect(names(c123_df), c("C1_ex_top_10","C2_ex_top_10","C3_ex_top_10"))
  cols_bottom <- intersect(names(c123_df), c("C1_ex_bottom_10","C2_ex_bottom_10","C3_ex_bottom_10"))

  df <- c123_df %>%
    filter(year != "ALL") %>%
    mutate(year = suppressWarnings(as.integer(year)))

  ycols <- c(cols_all, cols_top, cols_bottom)
  yvals <- df %>% select(any_of(ycols)) %>% as.matrix() %>% as.numeric()
  ylims <- range(yvals, na.rm = TRUE)
  pad   <- diff(ylims) * 0.03
  ylims <- c(ylims[1] - pad, ylims[2] + pad)

  mk_plot <- function(dat, keep_cols, subtitle) {
    if (length(keep_cols) == 0) {
      return(
        ggplot() + theme_void() +
          ggtitle(paste0(subtitle, " (no data)"))
      )
    }
    dat %>%
      select(year, all_of(keep_cols)) %>%
      pivot_longer(-year, names_to = "Index", values_to = "Value") %>%
      mutate(
        Index = recode(Index,
                       "C1_all" = "C1 (Anchored to Bottom)", "C2_all" = "C2 (Gini)", "C3_all" = "C3 (Anchored to Top)",
                       "C1_ex_top_10" = "C1 (Anchored to Bottom)", "C2_ex_top_10" = "C2 (Gini)", "C3_ex_top_10" = "C3 (Anchored to Top)",
                       "C1_ex_bottom_10" = "C1 (Anchored to Bottom)", "C2_ex_bottom_10" = "C2 (Gini)", "C3_ex_bottom_10" = "C3 (Anchored to Top)"),
        Index = factor(Index, levels = c("C1 (Anchored to Bottom)","C2 (Gini)","C3 (Anchored to Top)"))
      ) %>%
      ggplot(aes(x = year, y = Value, color = Index, group = Index)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.6) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(limits = ylims) +
      labs(title = subtitle, x = "Year", y = NULL, color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  }

  p_all    <- mk_plot(df, cols_all,    "All")
  p_top    <- mk_plot(df, cols_top,    "Exclude Top 10%")
  p_bottom <- mk_plot(df, cols_bottom, "Exclude Bottom 10%")

  filepath <- file.path(out_dir, filename)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_combined <- p_all / p_top / p_bottom + patchwork::plot_annotation(title = title)
    ggsave(filepath, plot = p_combined, device = "pdf", width = 8.5, height = 11, dpi = 300)
    return(p_combined)
  } else {
    # Fallback: 3-page PDF
    pdf(filepath, width = 8.5, height = 11)
    grid::grid.newpage(); grid::grid.text(title, y = 0.98, gp = grid::gpar(fontsize = 14, fontface = "bold"))
    print(p_all); print(p_top); print(p_bottom)
    dev.off()
    invisible(NULL)
  }
}


# Income 
inc <- C123_by_dist(r_hh, inc_all, weight = TRUE, weight_var = "fam_weight", cutoff = 0.10)
plot_C123_by_dist(
  inc,
  title    = "Income: C1/C2/C3 by Distribution Cut (HH)",
  filename = "income_C123_by_dist.pdf"
)


# Wealth (excluding home equity)
wealth_nohouse <- C123_by_dist(r_hh_wealth, wealth_nohouse, weight = TRUE, weight_var = "fam_weight", cutoff = 0.10)
plot_C123_by_dist(
  wealth_nohouse,
  title    = "Wealth (exl. home equity): C1/C2/C3 by Distribution Cut (HH)",
  filename = "wealth_nohouse_C123_by_dist.pdf"
)

# Wealth (including home equity)
wealth <- C123_by_dist(r_hh_wealth, wealth, weight = TRUE, weight_var = "fam_weight", cutoff = 0.10)
plot_C123_by_dist(
  wealth,
  title    = "Wealth (incl. home equity): C1/C2/C3 by Distribution Cut (HH)",
  filename = "wealth_C123_by_dist.pdf"
)





# Plot three panels (All, Exclude Top 50%, Exclude Bottom 50%) on one PDF page for C1, C2, and C3
plot_C123_by_dist <- function(c123_df, title, filename,
                              out_dir = here("7_nuclear_family", "output", "by_dist", "50pct")) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  cols_all    <- intersect(names(c123_df), c("C1_all","C2_all","C3_all"))
  cols_top    <- intersect(names(c123_df), c("C1_ex_top_50","C2_ex_top_50","C3_ex_top_50"))
  cols_bottom <- intersect(names(c123_df), c("C1_ex_bottom_50","C2_ex_bottom_50","C3_ex_bottom_50"))

  df <- c123_df %>%
    filter(year != "ALL") %>%
    mutate(year = suppressWarnings(as.integer(year)))

  ycols <- c(cols_all, cols_top, cols_bottom)
  yvals <- df %>% select(any_of(ycols)) %>% as.matrix() %>% as.numeric()
  ylims <- range(yvals, na.rm = TRUE)
  pad   <- diff(ylims) * 0.03
  ylims <- c(ylims[1] - pad, ylims[2] + pad)

  mk_plot <- function(dat, keep_cols, subtitle) {
    if (length(keep_cols) == 0) {
      return(
        ggplot() + theme_void() +
          ggtitle(paste0(subtitle, " (no data)"))
      )
    }
    dat %>%
      select(year, all_of(keep_cols)) %>%
      pivot_longer(-year, names_to = "Index", values_to = "Value") %>%
      mutate(
        Index = recode(Index,
                       "C1_all" = "C1 (Anchored to Bottom)", "C2_all" = "C2 (Gini)", "C3_all" = "C3 (Anchored to Top)",
                       "C1_ex_top_50" = "C1 (Anchored to Bottom)", "C2_ex_top_50" = "C2 (Gini)", "C3_ex_top_50" = "C3 (Anchored to Top)",
                       "C1_ex_bottom_50" = "C1 (Anchored to Bottom)", "C2_ex_bottom_50" = "C2 (Gini)", "C3_ex_bottom_50" = "C3 (Anchored to Top)"),
        Index = factor(Index, levels = c("C1 (Anchored to Bottom)","C2 (Gini)","C3 (Anchored to Top)"))
      ) %>%
      ggplot(aes(x = year, y = Value, color = Index, group = Index)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.6) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(limits = ylims) +
      labs(title = subtitle, x = "Year", y = NULL, color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  }

  p_all    <- mk_plot(df, cols_all,    "All")
  p_top    <- mk_plot(df, cols_top,    "Bottom 50%")
  p_bottom <- mk_plot(df, cols_bottom, "Top 50%")

  filepath <- file.path(out_dir, filename)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_combined <- p_all / p_top / p_bottom + patchwork::plot_annotation(title = title)
    ggsave(filepath, plot = p_combined, device = "pdf", width = 8.5, height = 11, dpi = 300)
    return(p_combined)
  } else {
    # Fallback: 3-page PDF
    pdf(filepath, width = 8.5, height = 11)
    grid::grid.newpage(); grid::grid.text(title, y = 0.98, gp = grid::gpar(fontsize = 14, fontface = "bold"))
    print(p_all); print(p_top); print(p_bottom)
    dev.off()
    invisible(NULL)
  }
}


# Income 
inc <- C123_by_dist(r_hh, inc_all, weight = TRUE, weight_var = "fam_weight", cutoff = 0.50)
plot_C123_by_dist(
  inc,
  title    = "Income: C1/C2/C3 by Distribution Cut (HH)",
  filename = "income_C123_by_dist.pdf"
)


# Wealth (excluding home equity)
wealth_nohouse <- C123_by_dist(r_hh_wealth, wealth_nohouse, weight = TRUE, weight_var = "fam_weight", cutoff = 0.50)
plot_C123_by_dist(
  wealth_nohouse,
  title    = "Wealth (exl. home equity): C1/C2/C3 by Distribution Cut (HH)",
  filename = "wealth_nohouse_C123_by_dist.pdf"
)

# Wealth (including home equity)
wealth <- C123_by_dist(r_hh_wealth, wealth, weight = TRUE, weight_var = "fam_weight", cutoff = 0.50)
plot_C123_by_dist(
  wealth,
  title    = "Wealth (incl. home equity): C1/C2/C3 by Distribution Cut (HH)",
  filename = "wealth_C123_by_dist.pdf"
)






