library(here)
library(dplyr)
library(tidyr)
library(tibble)
library(openxlsx)

# LOAD DATA ------------------------------------------------------------------
# Each row represents a person-year, data is collected for the individual or the family
build <- readRDS(here("1_build_panel", "output", "build.rds"))

# Load Inflation Data -----------------------------------------------------
  # This file contains annual consumer price index (CPI) data for inflation adjustment from the Bureau of Labor Statistics
cpi <- openxlsx::read.xlsx(here("0_data", "cpi", "cpi.xlsx")) %>%
  as_tibble() %>%
  rename(inflation_value = Jan, year = Year) %>%
  select(year, inflation_value) %>%
  filter(year >= 1967)

# LIMIT TO THOSE PRESENT IN THE FU EACH YEAR -------------------------------------------------
build <- build %>%
  filter(
    sequence <= 20 & sequence > 0,  # Remove people who are not in FU at that year
    id1968 < 7001)                   # Exclude Latino sample only

# CLEAN IDENTIFIERS AND LIMIT SAMPLE -------------------------------------------------------------
  # 1968 1 head 2 wife/ spouse 3, child 4-7 other 8 spouse 9/ 0 NA
  # 1969-1982 8 other 9 spouse 0 NA
  # 1983-1999 10 head 20 22 88 90 spouse 30-38 83 child 40-75 95-98 other 0 NA
  # 2017 92 spouse
  # 1968-1982 4 = sibling of head; 5 = parent of head; 7 = other adult relative
  # 1983-  40, 47, 48 = sibling; 50, 57, 58 = parent; 
  # 60, 65 grand/ great-grandson/ daughter
  # 66, 67, 68, 69 = grand/ great-grandparent;
  # 70, 71 = nephew; 72, 73 = uncle/ aunt; 74, 75 = cousin; 95, 96, 97 = other 

# Recode relation to head, spouse, child, or other 
build <- build %>%
  mutate(
    relation =
      case_when(
        # Head
        relation == 1 | relation == 10 ~ 1, 
        # Spouse
        relation == 2 | relation == 20 | relation == 22 | 
          (relation == 8 & year == 1968) |
          (relation == 9 & (year > 1968 & year < 1983)) |
          (relation > 87 & relation < 93) ~ 2,
        # Child
        (relation == 3 | relation == 83 |
           (relation > 29 & relation < 39)) ~ 3,
        # Other
        (relation > 3 & relation < 8) |
          (relation > 39 & relation < 76) |
          (relation > 94 & relation < 99) |
          (relation == 8 & (year > 1968 & year < 1983)) ~ 4,
        relation == 0 | is.na(relation) |
          (relation == 9 & year == 1968) ~ 0,
        TRUE ~ relation
      ) 
    )

# CLEAN FAMILY DATA -------------------------------------------------------------------------------------------------

# CLEAN INCOME AND WEALTH ------------------------------------------------------------------
# Adjust NA income and wealth variables
build <- build %>%
  mutate(across(
    .cols = c("inc_all", "inc_tax_hs", "inc_tax_o", 
              "inc_trans_hs", "inc_trans_o1", "inc_trans_o2"),
    .fns = ~ case_when(
      year %in% c(1994, 1995) & . > 9999998 ~ NA_real_,
      TRUE ~ .
    )
  ))
  
# Add indicator for top-coded values, then adjust for inflation

topcode_rules <- tribble(
  ~var,                 ~year_start, ~year_end, ~topcode,
  "inc_all",         1969,        1979,      99999,
  "inc_all",         1980,        1980,      999999,
  "inc_all",         1981,        1983,      9999999,
  "inc_all",         1984,        1985,      999999,
  "inc_all",         1986,        1993,      9999999,
  "inc_all",         1994,        1997,      9999998,
  "inc_all",         1999,        2023,      9999999,

  "inc_tax_hs",      1969,        1978,      99999,
  "inc_tax_hs",      1979,        1980,      999999,
  "inc_tax_hs",      1982,        1996,      9999999,
  "inc_tax_hs",      1999,        2023,      9999999,

  "inc_tax_o",       1969,        1983,      99999,
  "inc_tax_o",       1984,        1993,      999999,
  "inc_tax_o",       1994,        2023,      9999998,

  "inc_trans_hs",    1970,        1992,      99999,
  "inc_trans_hs",    1993,        1993,      999999,
  "inc_trans_hs",    1994,        1996,      9999998,
  "inc_trans_hs",    1997,        1997,      999999,
  "inc_trans_hs",    1999,        2009,      9999998,
  "inc_trans_hs",    2011,        2023,      9999997,

  "inc_trans_o1",    1970,        1992,      99999,
  "inc_trans_o1",    1993,        1993,      999999,
  "inc_trans_o1",    1994,        1996,      9999998,
  "inc_trans_o1",    1997,        1997,      999999,
  "inc_trans_o1",    1999,        2009,      9999998,
  "inc_trans_o1",    2011,        2023,      9999997,

  "inc_trans_o2",    1970,        1992,      99999,
  "inc_trans_o2",    1993,        1993,      999999,
  "inc_trans_o2",    1994,        1996,      9999998,
  "inc_trans_o2",    1997,        1997,      999999,
  "inc_trans_o2",    1999,        2009,      9999998,
  "inc_trans_o2",    2011,        2023,      9999997,

  "wealth_nohouse",  1984,        2005,      999999998,
  "wealth_nohouse",  2007,        2009,      999999996,
  "wealth_nohouse",  2011,        2023,      999999997,

  "wealth",          1984,        2005,      999999998,
  "wealth",          2007,        2009,      999999996,
  "wealth",          2011,        2023,      999999997,

  "wealth_farmbus",  1984,        2005,      999999998,
  "wealth_farmbus",  2007,        2009,      999999996,
  "wealth_farmbus",  2011,        2023,      999999997,

  "wealth_checking", 1984,        2003,      999999998,
  "wealth_checking", 2005,        2005,      999999999,
  "wealth_checking", 2007,        2009,      999999996,
  "wealth_checking", 2011,        2017,      999999997,

  "wealth_re",       1984,        2005,      999999998,
  "wealth_re",       2007,        2009,      999999996,
  "wealth_re",       2011,        2023,      999999997,

  "wealth_stocks",   1984,        2005,      999999998,
  "wealth_stocks",   2007,        2009,      999999996,
  "wealth_stocks",   2011,        2023,      999999997,

  "wealth_vehicles", 1984,        2005,      999999998,
  "wealth_vehicles", 2007,        2009,      999999996,
  "wealth_vehicles", 2011,        2023,      999999997,

  "wealth_other",    1984,        2005,      999999998,
  "wealth_other",    2007,        2009,      999999996,
  "wealth_other",    2011,        2023,      999999997,

  "wealth_debt",     1984,        2005,      999999999,
  "wealth_debt",     2007,        2009,      999999997,

  "wealth_home",     1984,        2023,      999999997,

  "student_loans",   2011,        2023,      9999997
  
)

# Expand topcode rules to long format (unchanged)
topcodes <- topcode_rules %>%
  rowwise() %>%
  mutate(year = list(seq(year_start, year_end))) %>%
  unnest(year) %>%
  select(var, year, topcode) %>%
  ungroup()

topcodes_wide <- topcodes %>%
  pivot_wider(names_from = var, values_from = topcode, names_prefix = "topcode_")

build <- build %>%
  left_join(topcodes_wide, by = "year")

# Removed money vars that aren't being used on 20260121
money_vars <- c(
  "inc_all", "inc_tax_hs", "inc_tax_o",
  "inc_trans_hs", "inc_trans_o1", "inc_trans_o2",
  "wealth_nohouse", "wealth", "wealth_home"
)

for (var in money_vars) {
  topcode_col <- paste0("topcode_", var)
  indicator_col <- paste0("ind_top_", var)

  build[[indicator_col]] <- as.integer(!is.na(build[[topcode_col]]) & 
                                        build[[var]] > (build[[topcode_col]] - 1)) # conservative approach
}

build <- build %>%
  select(-starts_with("topcode_"))

rm(topcode_rules, topcodes, topcodes_wide)


# Join inflation, adjust, and rename
build <- build %>%
  left_join(cpi, by = "year", relationship = "many-to-one") %>%
  mutate(across(
    all_of(money_vars),
    ~ .x * inflation_value / 100,
    .names = "infl_{.col}"
  )) %>%
  select(-all_of(money_vars), -inflation_value) %>%
  rename_with(~ gsub("^infl_", "", .x), starts_with("infl_"))


# CLEAN FAMILY RACE -------------------------------------------------------------
  # Recode Race
  # 1 white 2 black 3 4 7 other 8 9 0 NA
build <- build %>%
  mutate(
    across(
      .cols = contains("race"),
      ~ case_when(
        .x == 2 ~ 3, # black
        (.x > 2 & .x < 8) ~ 2, # other
        (.x == 0 | .x == 9 | .x == 8) ~ NA_real_, # NA
        TRUE ~ .x
      )
    )
  ) # white

build <- build %>%
  mutate(
    race_first = case_when(
      relation == 1 ~ race1_head,
      relation == 2 ~ race1_wife
    ),
    race_second = case_when(
      relation == 1 ~ race2_head,
      relation == 2 ~ race2_wife
    ),
    race_third = case_when(
      relation == 1 ~ race3_head,
      relation == 2 ~ race3_wife
    ),
    race_fourth = case_when(
      relation == 1 ~ race4_head,
      relation == 2 ~ race4_wife
    )
  ) %>%
  select(-contains("wife")) %>%
  select(-contains("head"))

build <- build %>%
  mutate(
    max_race = pmax(
      !!!select(., contains("race_")),
      na.rm = TRUE
    ),
    max_race = if_else(max_race == -Inf, NA_real_, max_race)
  )

# According to codebook mention order doesn't matter. Black if black exists, then other then white -Sayer, Cohen, and Casper (2004)
build <- build %>%
  group_by(pid) %>%
  mutate(
    race_year = case_when(
      !is.infinite(max_race) & !is.na(max_race) ~ as.double(year),
      TRUE ~ 0
    ),
    max_race_year = efficient_max(race_year, na.rm = TRUE),
    # Determine race_pid based on max_race_year
    race_pid = case_when(
      max_race_year == year ~ max_race,
      TRUE ~ NA_real_
    ),
    race = efficient_max(race_pid, na.rm = TRUE)
  ) %>%
  mutate(race = na_if(race, -Inf)) %>%
  ungroup()

# Handle race for each pid/year combination
sample_fast <- build %>%                       
  summarise(                                    
    max_head_race  = efficient_max(race[relation == 1], na.rm = TRUE),
    max_other_race = efficient_max(race[relation != 1], na.rm = TRUE),
    .by = c(pid, year)                         
  ) %>% 
  mutate(                                       
    max_person_race = coalesce(
      na_if(max_head_race , Inf),               
      na_if(max_other_race, Inf)               
    )
  )

build <- left_join(build, sample_fast, by = c("pid", "year"))

# Uses a Summary + Join Approach to Improve Speed / Memory Usage
pid_lookup <- build %>%                                
  summarise(                                        
    max_max_race = efficient_max(max_person_race, na.rm = TRUE),
    .by = pid
  )

build <- build %>%
  left_join(pid_lookup, by = "pid") %>%
  mutate(
    race = coalesce(                       
      race, 
      na_if(max_max_race, Inf)                      
    ),
    black = case_when(                          
      race == 3        ~ 1,
      race %in% 1:2    ~ 0,
      TRUE             ~ race
    )
  ) %>%
 select(                                      
    -max_max_race, 
    -starts_with("max_"), 
    -ends_with("_race"),
    -race_first,
    -race_second,
    -race_third,
    -race_fourth,
    -race_pid
  )

# CLEAN FAMILY WEIGHTS -------------------------------------------------------------
# Longitudinal family weights are to be used for cross-sectional family-level analyses
# Stratum and cluster variables are available at the individual level only, however these values are the same for all family members in a given year
build <- build %>%
  mutate(
    fam_weight = case_when(
      year >= 1968 & year <= 1992 ~ fam_longweight_68_92,
      year >= 1993 & year <= 1996 ~ fam_longweight_93_96,
      year >= 1997 & year <= 2023 ~ fam_longweight_97_23,
      TRUE ~ NA_real_
    ) 
  )
    
# Remove old family weights
  build <- build %>%
    select(-starts_with("fam_longweight_")) 


# SAVE CLEAN DATA -------------------------------------------------------------
file.remove(list.files(here("2_clean_panel", "output"), pattern = "\\.rds$", full.names = TRUE))
saveRDS(build, here("2_clean_panel", "output", "clean.rds"))

# Clean Up Temporary Files --------------------------------------------------
rm(sample_fast, psid_data, pid_lookup)

