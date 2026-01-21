# THERE ARE THREE SETS OF FUNCTIONS
# - GENERAL FUNCTIONS
# - GINI FUNCTIONS (TO CALCULATE GINI COEFFICIENTS)
# - NUCLEAR FAMILY FUNCTIONS (TO CALCULATE C1, C2, AND C3)


# GENERAL FUNCTIONS
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


# GINI FUNCTIONS
# Function to calculate Gini by year with or without survey weights for a given variable/df
gini_by_year_svy <- function(df, value_var, weight_var = NULL, simple_design = FALSE, with_se = FALSE) {
  v <- rlang::as_name(rlang::enquo(value_var))

  
  df <- df %>%
    filter(is.finite(!!sym(v))) %>%
    arrange(year, !!sym(v))

  # Weighted or unweighted
  if (simple_design) {
    des <- svydesign(ids = ~1, weights = ~1, data = df) |> convey_prep()
    vt <- if (with_se) c("se") else NULL
  } else {
    if (is.null(weight_var)) {
      df <- df %>% mutate(.one = 1)
      w_formula <- ~.one
    } else {
      w_formula <- as.formula(paste0("~", weight_var))
    }
    des <- svydesign(ids = ~cluster, strata = ~stratum, weights = w_formula, 
                     data = df, nest = TRUE) |> convey_prep()
    vt <- if (with_se) c("se") else NULL
  }

  out <- svyby(
    as.formula(paste0("~", v)), # Variable to call Gini on
    ~year, # Group Gini by year
    design = des, # Survey design object (for weights)
    FUN = svygini, # Calls Gini calculation
    na.rm = TRUE,
    vartype = vt,
    keep.names = FALSE
  ) %>%
    rename(gini = !!v) %>%
    mutate(
      gini = round(as.numeric(gini), 3),
      se   = if ("se" %in% names(.)) round(as.numeric(se), 3) else NULL
    ) %>%
    arrange(year)

  if (!with_se) {
    out <- select(out, year, gini)
  } else {
    out <- select(out, year, gini, se)
  }

  out
}

# Function to append mean Gini (across all years) row at top of dataframe
append_mean_row <- function(df) {
  mean_row <- df %>%
    summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 3)))
  
  if ("year" %in% names(df)) {
    df <- df %>% mutate(year = as.character(year))
    mean_row <- mean_row %>%
      mutate(year = "ALL") %>%
      relocate(year, .before = 1)
  }
  
  df <- bind_rows(mean_row, df)
  
  df
}

# Function to help run Gini calculations
run_gini <- function(df, var, weight = NULL, simple = FALSE, se = FALSE, name) {
  out <- gini_by_year_svy(df, !!sym(var), weight_var = weight, 
                          simple_design = simple, with_se = se)
  if (se) {
    out <- rename(out, !!name := gini, !!paste0(name, "_se") := se)
  } else {
    out <- rename(out, !!name := gini)
  }
  out
}

# Function to run Gini calculations by race
run_gini_race <- function(df_black, df_nonblack, var, weight = NULL, simple = FALSE, se = FALSE, name) {
  black_df     <- run_gini(df_black,    var, weight, simple, se, paste0(name, "_black"))
  nonblack_df  <- run_gini(df_nonblack, var, weight, simple, se, paste0(name, "_nonblack"))
  full_join(black_df, nonblack_df, by = "year")
}


# Function to create datasets excluding the TOP X% of a variable within each year
exclude_top <- function(df, var, percentile) {
  v <- ensym(var)
  df %>%
    group_by(year) %>%
    mutate(cutoff = quantile(!!v, percentile, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!!v <= cutoff) %>%
    select(-cutoff)
}

# Function to create datasets excluding the BOTTOM X% of a variable within each year
exclude_bottom <- function(df, var, percentile) {
  v <- ensym(var)
  df %>%
    group_by(year) %>%
    mutate(cutoff = quantile(!!v, percentile, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!!v >= cutoff) %>%
    select(-cutoff)
}


# Function to compare Gini calculations by distribution: all, excluding top X%, excluding bottom X%
gini_by_dist <- function(df, var, cutoff = 0.10) {
  v <- as_string(ensym(var))
  pct_label <- gsub("\\.", "_", as.character(cutoff * 100)) 

  g_all <- run_gini(
    df     = df,
    var    = v,
    weight = "fam_weight",
    simple = FALSE,
    se     = FALSE,
    name   = "gini_all"
  )

  top <- run_gini(
    df     = exclude_top(df, !!sym(v), 1 - cutoff),  # keep bottom (1 - cutoff)
    var    = v,
    weight = "fam_weight",
    simple = FALSE,
    se     = FALSE,
    name   = paste0("gini_ex_top_", pct_label)
  )

  bottom <- run_gini(
    df     = exclude_bottom(df, !!sym(v), cutoff),   # keep top (1 - cutoff)
    var    = v,
    weight = "fam_weight",
    simple = FALSE,
    se     = FALSE,
    name   = paste0("gini_ex_bottom_", pct_label)
  )

  out <- list(g_all, top, bottom) %>%
    reduce(full_join, by = "year") %>%
    arrange(year) %>%
    select(year, gini_all,
           !!sym(paste0("gini_ex_top_", pct_label)),
           !!sym(paste0("gini_ex_bottom_", pct_label)))

  append_mean_row(out)
}

# Function to pick HH and Clan Gini columns from a dataframe
pick_cols <- function(df, hh_vars, cl_vars) {
  hh_col <- intersect(hh_vars, names(df))[1]
  cl_col <- intersect(cl_vars, names(df))[1]
  if (is.na(hh_col) || is.na(cl_col)) {
    stop("Could not find expected HH/Clan Gini columns in the dataframe.")
  }
  list(hh = hh_col, cl = cl_col)
}

# Prep plotting data (drop ALL column from by_year files)
prep_plot_df <- function(df, hh_col, cl_col, title_lab) {
  df %>%
    filter(year != "ALL") %>%
    mutate(year = suppressWarnings(as.integer(year))) %>%
    select(year, HH = all_of(hh_col), Clan = all_of(cl_col)) %>%
    pivot_longer(-year, names_to = "Unit", values_to = "Gini") %>%
    mutate(PanelTitle = title_lab)
}

# NUCLEAR FAMILY FUNCTIONS
# Functions go from raw data > Lorenz curve > Scaled conditional mean curve > C1, C2, C3

# Function to calculate the Lorenz curve
# L(u) = 1/u * ∫_0^u F^(-1) * (t) dt
lorenz <- function(x, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x)) else stopifnot(length(x) == length(w))
  keep <- is.finite(x) & is.finite(w) & (w > 0)
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(tibble(p = c(0, 1), L = c(0, 1)))

  ord <- order(x); x <- x[ord]; w <- w[ord]
  sw  <- sum(w); sxw <- sum(x * w)
  if (sw <= 0 || sxw == 0) return(tibble(p = c(0, 1), L = c(0, 1)))

  p <- c(0, cumsum(w) / sw)
  L <- c(0, cumsum(x * w) / sxw)
  tibble(p = p, L = L)
}


# Function to calculate the scaled conditional mean curve
# M(u) = L(u) / u
scm_points <- function(p, L) {
  stopifnot(length(p) == length(L))

  ord <- order(p); p <- p[ord]; L <- L[ord] # Order data
  p_safe <- pmax(p, .Machine$double.eps) # Set integration limit
  M <- L / p_safe

  M[p == 1] <- 1  # Boundary condition M(1)=1
  tibble(u = p, M = M)
}


# Function to calculate first three  moments on the scaled conditional mean curve (Aaberge Eq. 8, pp. 311)
# Ck(F) = k * ∫_0^1 u^(k-1) * (1 - M(u)) du
scm_moments <- function(u, M, k) {
  stopifnot(length(u) == length(M), k >= 1)

  keep <- is.finite(u) & is.finite(M) # Keep finite values
  u <- u[keep]; M <- M[keep] 
  ord <- order(u); u <- u[ord]; M <- M[ord] # Order values in population

  u <- pmin(pmax(u, 0), 1) # Set integration limites
  u_safe <- pmax(u, .Machine$double.eps)

  integrand <- (u_safe^(k - 1)) * (1 - M)
  k * trapz(u, integrand)
}


# Helper function to calculate C123 (raw data > Lorenz > SCM > C1,C2,C3)
c123 <- function(x, w = NULL) {
  # Raw to Lorenz
  lz <- lorenz(x, w) 

  # Lorenz to SCM
  scm <- scm_points(lz$p, lz$L) 

  # SCM to C123
  tibble(
    C1 = scm_moments(scm$u, scm$M, 1),
    C2 = scm_moments(scm$u, scm$M, 2),
    C3 = scm_moments(scm$u, scm$M, 3)
  )
}


# Helper function to calculate moments by year
C123_by_year <- function(df, value_var, weight = TRUE, weight_var = NULL) {
  v <- as_string(ensym(value_var))

  if (weight && is.null(weight_var)) {
    stop("When weight = TRUE, provide weight_var.")
  }

  base <- df %>%
    filter(is.finite(.data[[v]])) %>%
    transmute(
      year  = year,
      value = .data[[v]],
      w     = if (weight) .data[[weight_var]] else NA_real_
    )

  base %>%
    group_split(year) %>%
    map_dfr(function(d) {
      if (nrow(d) == 0) return(tibble())

      out <- c123(d$value, if (weight) d$w else NULL)

      out %>%
        mutate(year = unique(d$year), .before = 1)
    }) %>%
    arrange(year) %>%
    mutate(
      across(where(is.numeric), ~ round(as.numeric(.x), 3))   # <- NEW: round to 3 decimals
    )
}


# Trapezoidal integration helper
trapz <- function(x, y) sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)