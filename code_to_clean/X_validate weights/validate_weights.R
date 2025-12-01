# INPUTS: 3_create_households/output/households.rds
# OUTPUTS: 3_create_households/output/validate

# LOAD DATA ------------------------------------------------------------------
households <- readRDS(here("3_households", "output", "households.rds"))

# Filter out non-sample households
households <- households %>%
  filter(!(sample_head == 0 & sample_spouse == 0))

# CHECK WEIGHTS ---------------------------------------------------
# PSID makes summary data available for household weights, below is code to reproduce the summary statistics in the reports below:
  # 1989 - 2005: https://psidonline.isr.umich.edu/data/weights/Long-weights-doc.pdf see Table 10 p. 32
  # 2001 - 2021: https://psidonline.isr.umich.edu/data/weights/long_weight_21.pdf see Table 5 p. 12

hh <- households %>%
  filter(!(year %in% c(2001, 2003) & fam_weight <= 0))

validate_weights <- hh %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    mean_weight   = mean(fam_weight, na.rm = TRUE),
    median_weight = median(fam_weight, na.rm = TRUE),
    sd_weight     = sd(fam_weight, na.rm = TRUE),
    n             = dplyr::n(),
    .groups = "drop"
  )

write_csv(validate_weights,  here("3_households", "output", "validate_weights.csv"))

# CHECK IF WEIGHTED SUMMARY STATISTICS FOR RACE ARE ACCURATE -----------------------------------
# PSID makes summary data available for household weights, below is code to reproduce the summary statistics for race:
    # https://psidonline.isr.umich.edu/data/weights/long_weight_21.pdf see Table 9A p. 16

# Apply weights, strata, and clusters
des <- svydesign(
  id = ~cluster,
  strata = ~stratum,
  weights = ~fam_weight,
  data = households,
  nest = TRUE
)

# Create table with standard errors
validate_race_weights <- svyby(
    ~black_head,
    ~year,
    des,
    svymean,
    na.rm = TRUE,
    vartype = "se"
  ) %>%
  as_tibble() %>%
  transmute(
    year,
    pct_black = black_head * 100,
    se_black = se * 100,
    pct_nonblack = (1 - black_head) * 100,
    se_nonblack = se * 100
  )

write_csv(validate_race_weights,  here("3_households", "output", "validate_race_weights.csv"))
