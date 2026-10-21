# Functions defined in this file:
#   get_gini_at
#   avg_gap
#   clear_output
#   read_gini



# Pull the Gini for a single year from a by-year data frame
# Used in: figure_main
get_gini_at <- function(df, year_value, col) {
  df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(year == year_value) %>%
    dplyr::pull({{ col }}) %>%
    dplyr::first()
}

# Average HH-Clan gap across all non-ALL years
# Used in: figure_appendix
avg_gap <- function(df, hh_col, cl_col) {
  d <- df %>%
    dplyr::filter(year != "ALL") %>%
    dplyr::mutate(
      diff = as.numeric(.data[[hh_col]]) - as.numeric(.data[[cl_col]])
    ) %>%
    dplyr::filter(is.finite(diff))
  round(mean(d$diff), 3)
}

# Remove RDS/CSV/PDF files in a directory, optionally keeping some by name
# Used in: clans
clear_output <- function(dir, keep = NULL) {
  files <- list.files(dir, pattern = "\\.(rds|csv|pdf)$", full.names = TRUE)
  if (!is.null(keep)) files <- files[!basename(files) %in% keep]
  file.remove(files)
  invisible(NULL)
}

# Read a Gini CSV and coerce year to integer
# Used in: supplemental
read_gini <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(year = suppressWarnings(as.integer(as.character(year))))
}