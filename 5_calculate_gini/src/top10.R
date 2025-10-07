# Load data
hh     <- readRDS(here("3_households", "output", "households.rds"))
clans  <- readRDS(here("4_clans", "output", "clans.rds"))
r_hh   <- readRDS(here("3_households", "output", "robust_households.rds"))
r_clans<- readRDS(here("4_clans", "output", "robust_clans.rds"))

hh_wealth <- hh %>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))
r_hh_wealth <- r_hh %>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))
clans_wealth <- clans %>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))
r_clans_wealth <- r_clans %>% filter(year %in% c(1984, 1989, 1994, seq(1999, 2021, by = 2)))

options(survey.lonely.psu = "adjust")

# Functions (from calculate_gini.R)
gini_by_year_svy <- function(df, value_var, weight_var = NULL, simple_design = FALSE, with_se = FALSE) {
  v <- as_name(enquo(value_var))
  df <- df %>%
    filter(is.finite(!!sym(v))) %>%
    arrange(year, !!sym(v))

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
    as.formula(paste0("~", v)),
    ~year,
    design = des,
    FUN = svygini,
    na.rm = TRUE,
    vartype = vt,
    keep.names = FALSE
  ) %>%
    rename(gini = !!v) %>%
    mutate(
      gini = as.numeric(gini),
      se   = if ("se" %in% names(.)) as.numeric(se) else NULL
    ) %>%
    arrange(year)

  if (!with_se) select(out, year, gini) else select(out, year, gini, se)
}

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

# Additional functions
top_p_share <- function(x, w, p = 0.10) {
  x <- as.numeric(x); w <- as.numeric(w)
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(NA_real_)
  o <- order(x, decreasing = TRUE)
  x <- x[o]; w <- w[o]

  W <- sum(w)
  if (W <= 0) return(NA_real_)
  target <- p * W

  cw_before <- c(0, cumsum(w))[seq_along(w)]
  take <- pmin(pmax(target - cw_before, 0), w)  # fractional last unit allowed
  total <- sum(x * w)
  if (total <= 0 || !is.finite(total)) return(NA_real_)

  sum(x * take) / total
}

bottom_p_share <- function(x, w, p = 0.10) {
  x <- as.numeric(x); w <- as.numeric(w)
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]; w <- w[keep]
  if (!length(x)) return(NA_real_)
  o <- order(x, decreasing = FALSE)  # poorest first
  x <- x[o]; w <- w[o]

  W <- sum(w)
  if (W <= 0) return(NA_real_)
  target <- p * W

  cw_before <- c(0, cumsum(w))[seq_along(w)]
  take <- pmin(pmax(target - cw_before, 0), w)  # fractional last unit allowed
  total <- sum(x * w)
  if (total <= 0 || !is.finite(total)) return(NA_real_)

  sum(x * take) / total
}


dedup_households <- function(df) {
  df %>%
    filter(!is.na(id1968), !is.na(fam_id)) %>%
    arrange(year, id1968, fam_id) %>%
    distinct(year, id1968, fam_id, .keep_all = TRUE)
}

#Income
inc_gini_hh <- run_gini(
  df   = r_hh,
  var  = "inc_all",
  weight = "fam_weight",
  simple = FALSE,
  se   = FALSE,
  name = "gini_households"
)  # cols: year, gini_households

inc_top10_hh <- r_hh %>%
  filter(is.finite(inc_all), is.finite(fam_weight)) %>%
  dedup_households() %>%
  group_by(year) %>%
  summarise(top10_share_households = top_p_share(inc_all, fam_weight, p = 0.10),
            .groups = "drop")

inc_top10_clan <- r_clans %>%
  filter(is.finite(inc_all), is.finite(clan_weight), !is.na(id1968)) %>%
  arrange(year, id1968) %>%
  distinct(year, id1968, .keep_all = TRUE) %>%
  group_by(year) %>%
  summarise(top10_share_clans = top_p_share(inc_all, clan_weight, p = 0.10),
            .groups = "drop")

inc_bottom10_hh <- r_hh %>%
  filter(is.finite(inc_all), is.finite(fam_weight)) %>%
  dedup_households() %>%
  group_by(year) %>%
  summarise(bottom10_share_households = bottom_p_share(inc_all, fam_weight, p = 0.10),
            .groups = "drop")

inc_bottom10_clan <- r_clans %>%
  filter(is.finite(inc_all), is.finite(clan_weight), !is.na(id1968)) %>%
  arrange(year, id1968) %>%
  distinct(year, id1968, .keep_all = TRUE) %>%
  group_by(year) %>%
  summarise(bottom10_share_clans = bottom_p_share(inc_all, clan_weight, p = 0.10),
            .groups = "drop")

inc_by_year <- inc_gini_hh %>%
  left_join(inc_top10_hh,     by = "year") %>%
  left_join(inc_bottom10_hh,  by = "year") %>%
  left_join(inc_top10_clan,   by = "year") %>%
  left_join(inc_bottom10_clan,by = "year") %>%
  arrange(year)


# Wealth
wealth_years <- c(1984, 1989, 1994, seq(1999, 2021, by = 2))

wealth_gini_hh <- run_gini(
  df   = r_hh %>% filter(year %in% wealth_years),
  var  = "wealth_nohouse",
  weight = "fam_weight",
  simple = FALSE,
  se   = FALSE,
  name = "gini_households"
)

wealth_top10_hh <- r_hh %>%
  filter(year %in% wealth_years,
         is.finite(wealth_nohouse), is.finite(fam_weight)) %>%
  dedup_households() %>%
  group_by(year) %>%
  summarise(top10_share_households = top_p_share(wealth_nohouse, fam_weight, p = 0.10),
            .groups = "drop")

wealth_top10_clan <- r_clans %>%
  filter(year %in% wealth_years,
         is.finite(wealth_nohouse), is.finite(clan_weight), !is.na(id1968)) %>%
  arrange(year, id1968) %>%
  distinct(year, id1968, .keep_all = TRUE) %>%
  group_by(year) %>%
  summarise(top10_share_clans = top_p_share(wealth_nohouse, clan_weight, p = 0.10),
            .groups = "drop")

wealth_bottom10_hh <- r_hh %>%
  filter(year %in% wealth_years,
         is.finite(wealth_nohouse), is.finite(fam_weight)) %>%
  dedup_households() %>%
  group_by(year) %>%
  summarise(bottom10_share_households = bottom_p_share(wealth_nohouse, fam_weight, p = 0.10),
            .groups = "drop")

wealth_bottom10_clan <- r_clans %>%
  filter(year %in% wealth_years,
         is.finite(wealth_nohouse), is.finite(clan_weight), !is.na(id1968)) %>%
  arrange(year, id1968) %>%
  distinct(year, id1968, .keep_all = TRUE) %>%
  group_by(year) %>%
  summarise(bottom10_share_clans = bottom_p_share(wealth_nohouse, clan_weight, p = 0.10),
            .groups = "drop")

wealth_by_year <- wealth_gini_hh %>%
  left_join(wealth_top10_hh,      by = "year") %>%
  left_join(wealth_bottom10_hh,   by = "year") %>%
  left_join(wealth_top10_clan,    by = "year") %>%
  left_join(wealth_bottom10_clan, by = "year") %>%
  arrange(year)

# Combine and export
combined_by_year <- inc_by_year %>%
  full_join(wealth_by_year, by = "year", suffix = c("_inc", "_wealth")) %>%
  arrange(year) %>%
  select(
    year,
    gini_households_inc,
    top10_share_households_inc,
    top10_share_clans_inc,
    bottom10_share_households_inc,
    bottom10_share_clans_inc,
    gini_households_wealth,
    top10_share_households_wealth,
    top10_share_clans_wealth,
    bottom10_share_households_wealth,
    bottom10_share_clans_wealth
  ) %>%
  mutate(across(-year, ~ round(.x, 2)))

write_csv(combined_by_year, here("5_calculate_gini", "output", "top_inc_wealth.csv"))
