# Function to calculate Gini by year with or without survey weights for a given variable/df
gini_by_year_svy <- function(df, value_var, weight_var = NULL, simple_design = FALSE, with_se = FALSE) {
  v <- as_name(enquo(value_var))
  
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

# Helper function to run Gini calculations
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


# PLOTTING FUNCTIONS ---------------------------------------------------------
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

