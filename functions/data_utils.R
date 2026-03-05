# data_utils.R
# Pipeline and assembly helpers shared across multiple scripts.



# size_standardize
# Apply household-size and clan-size normalisation to all eight data frames.

size_standardize <- function(hh, r_hh, hh_wealth, r_hh_wealth,
                              clans, r_clans, clans_wealth, r_clans_wealth) {
  hh          <- hh          %>% dplyr::mutate(inc_all = inc_all / numfu)
  r_hh        <- r_hh        %>% dplyr::mutate(inc_all = inc_all / numfu)
  hh_wealth   <- hh_wealth   %>% dplyr::mutate(
    wealth_nohouse = wealth_nohouse / numfu,
    wealth         = wealth         / numfu)
  r_hh_wealth <- r_hh_wealth %>% dplyr::mutate(
    wealth_nohouse = wealth_nohouse / numfu,
    wealth         = wealth         / numfu)

  clans          <- clans          %>% dplyr::mutate(inc_all = inc_all / numclan)
  r_clans        <- r_clans        %>% dplyr::mutate(inc_all = inc_all / numclan)
  clans_wealth   <- clans_wealth   %>% dplyr::mutate(
    wealth_nohouse = wealth_nohouse / numclan,
    wealth         = wealth         / numclan)
  r_clans_wealth <- r_clans_wealth %>% dplyr::mutate(
    wealth_nohouse = wealth_nohouse / numclan,
    wealth         = wealth         / numclan)

  list(hh = hh, r_hh = r_hh, hh_wealth = hh_wealth, r_hh_wealth = r_hh_wealth,
       clans = clans, r_clans = r_clans,
       clans_wealth = clans_wealth, r_clans_wealth = r_clans_wealth)
}


# join_hh_clan
# Merge HH and clan C123 result tibbles, suffix each with _hh / _clan,
join_hh_clan <- function(hh_df, clan_df, var_label, out_dir) {
  out <- dplyr::full_join(
    hh_df   %>% dplyr::rename_with(~ paste0(.x, "_hh"),   -year),
    clan_df %>% dplyr::rename_with(~ paste0(.x, "_clan"), -year),
    by = "year"
  ) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(year = as.character(year))

  all_row <- out %>%
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric),
                    ~ round(mean(.x, na.rm = TRUE), 3))
    ) %>%
    dplyr::mutate(year = "ALL", .before = 1)

  result <- dplyr::bind_rows(out, all_row) %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ round(as.numeric(.x), 3))
    )

  readr::write_csv(result, file.path(out_dir, paste0(var_label, "_C123.csv")))
  invisible(result)
}


# Pull the Gini for a single year from a by-year data frame
get_gini_at <- function(df, year_value, col) {
  df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(year == year_value) %>%
    dplyr::pull({{ col }}) %>%
    dplyr::first()
}

# Average HH-Clan gap across all non-ALL years
avg_gap <- function(df, hh_col, cl_col) {
  d <- df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(
      diff = as.numeric(.data[[hh_col]]) - as.numeric(.data[[cl_col]])
    ) %>%
    dplyr::filter(is.finite(diff))
  round(mean(d$diff), 3)
}


# Italic note textGrob, left-aligned, word-wrapped
note_grob <- function(txt, width = 110,
                       size = note_size, family = base_family) {
  wrapped <- paste(strwrap(txt, width = width), collapse = "\n")
  grid::textGrob(
    wrapped,
    x    = grid::unit(0.01, "npc"),
    just = "left",
    gp   = grid::gpar(fontfamily = family, fontface = "italic",
                      fontsize = size),
    default.units = "npc"
  )
}


# Remove RDS/CSV/PDF files in a directory, optionally keeping some by name
clear_output <- function(dir, keep = NULL) {
  files <- list.files(dir, pattern = "\\.(rds|csv|pdf)$", full.names = TRUE)
  if (!is.null(keep)) files <- files[!basename(files) %in% keep]
  file.remove(files)
  invisible(NULL)
}

# Read a Gini CSV and coerce year to integer
read_gini <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(year = suppressWarnings(as.integer(as.character(year))))
}
