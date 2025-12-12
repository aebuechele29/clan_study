# Function to calculate minimum
efficient_min <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(min(x, na.rm = na.rm))

}

# Function to calculate maximum
efficient_max <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(max(x, na.rm = na.rm))

}

# Function to calculate mean
efficient_mean <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(mean(x, na.rm = na.rm))

}

# Function to calculate sum
efficient_sum <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(sum(x, na.rm = na.rm))

} 

# Function to clear files in a folder
clear_output <- function(dir, keep = character()) {
  files <- list.files(dir, pattern = "\\.rds$", full.names = FALSE)

  files_to_remove <- if (length(keep) == 0) {
    files
  } else {
    setdiff(files, keep)
  }

  if (length(files_to_remove) > 0) {
    file.remove(file.path(dir, files_to_remove))
  }
}


# Function to create difference tables (for appendix)
`%||%` <- function(x, y) if (!is.null(x)) x else y

make_two_panel_table <- function(
  df,
  title,
  outfile,
  year_col = year,
  left_hh, left_clans,
  right_hh, right_clans,
  left_label,
  right_label,
  overall_label = "Difference Overall") {

  year_col   <- ensym(year_col)
  left_hh    <- ensym(left_hh)
  left_clans <- ensym(left_clans)
  right_hh   <- ensym(right_hh)
  right_clans<- ensym(right_clans)

  tbl <- df %>%
    mutate(
      year_chr = as.character(!!year_col),
      year_num = suppressWarnings(as.integer(year_chr)),
      ord      = ifelse(year_chr == "ALL", -Inf, year_num)
    ) %>%
    arrange(ord) %>%
    transmute(
      Year = year_chr,

      HH_left    = !!left_hh,
      Clans_left = !!left_clans,
      Diff_left  = (!!left_hh) - (!!left_clans),

      HH_right    = !!right_hh,
      Clans_right = !!right_clans,
      Diff_right  = (!!right_hh) - (!!right_clans),

      Difference_Overall = Diff_right - Diff_left
    ) %>%
    mutate(across(-Year, ~ round(.x, 3)))

  ft <- flextable(tbl)

  ft <- add_header_row(
    ft,
    values    = c("", left_label, right_label, overall_label),
    colwidths = c(1, 3, 3, 1)
  )

  ft <- set_header_labels(
    ft,
    Year = "Year",
    HH_left = "HH", Clans_left = "Clans", Diff_left = "Diff",
    HH_right = "HH", Clans_right = "Clans", Diff_right = "Diff",
    Difference_Overall = "Diff"
  )

  ft <- theme_booktabs(ft)
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "all")
  ft <- autofit(ft)

  doc <- read_docx()
  doc <- body_add_par(doc, title, style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, ft)

  print(doc, target = outfile)
}
