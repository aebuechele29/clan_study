# INPUTS: 3_calculate_households/output/households.rds
# OUTPUTS: 4_clean_households/output/clean_hs.rds

households <- readRDS(here("3_create_households", "output", "households.rds"))
file.remove(list.files(here("4_clean_households", "output"), pattern = "\\.rds$", full.names = TRUE))

# ADD CLAN DATA TO HOUSEHOLDS ------------------------------------------------------------------
# Add number of households within each clan by year
households <- households %>%
  group_by(year, id1968) %>%
  mutate(numclan = n()) %>%
  ungroup()

# Assign a clan number to each household per year
households <- households %>%
  group_by(id1968, year) %>%
  arrange(fam_id) %>%
  mutate(hh_number = row_number()) %>%
  ungroup() 

# CLEAN HOUSEHOLD DATA BEFORE RESHAPING TO CLANS -----------------------------------
# Check that clan ids are shared between heads and spouses
# Save mismatched cases
  mismatched <- households %>%
    filter(
      !is.na(id1968) & 
      !is.na(id1968_spouse) & 
      id1968 != id1968_spouse
    ) 
  
  saveRDS(mismatched, here("4_clean_households", "output", "mismatched.rds"))

# Remove mismatched from the main dataset
  households <- households %>%
    filter(
      is.na(id1968) | 
       is.na(id1968_spouse) | 
       id1968 == id1968_spouse
   )

# Remove parent and grandparent variables for non-mismatched households
households <- households %>%
  select(
    -starts_with("parent1_"),
    -starts_with("parent2_"),
    -starts_with("grandparent1_"),
    -starts_with("grandparent2_"),
    -starts_with("grandparent3_"),
    -starts_with("grandparent4_"),
    -starts_with("ind_top_"),
    -release,
    -married_pair
  )

# Calculate proportion of house that is each race
# Applies head's race to children and others in the households
households <- households %>%
  mutate(
    num_race1 = case_when(
      !is.na(race_spouse) & race_spouse != race_head ~ 
        pmax(0, (numfu - 1)) * as.integer(race_head == 1) + as.integer(race_spouse == 1),
      TRUE ~ numfu * as.integer(race_head == 1)
    ),
    
    num_race2 = case_when(
      !is.na(race_spouse) & race_spouse != race_head ~ 
        pmax(0, (numfu - 1)) * as.integer(race_head == 2) + as.integer(race_spouse == 2),
      TRUE ~ numfu * as.integer(race_head == 2)
    ),
    
    num_race3 = case_when(
      !is.na(race_spouse) & race_spouse != race_head ~ 
        pmax(0, (numfu - 1)) * as.integer(race_head == 3) + as.integer(race_spouse == 3),
      TRUE ~ numfu * as.integer(race_head == 3)
    ),
    
    prop_race_white = if_else(numfu > 0, num_race1 / numfu, NA_real_),
    prop_race_black = if_else(numfu > 0, num_race2 / numfu, NA_real_),
    prop_race_other = if_else(numfu > 0, num_race3 / numfu, NA_real_)
  ) %>%
  select(-num_race1, -num_race2, -num_race3)


# Create household-level proportional variables
binary_vars <- c("male_", "edu_cat1_", "edu_cat2_", "hs_", "ba_", "ma_")
yob_vars <- "yob_"

get_vars_by_prefix <- function(prefix) {
  grep(paste0("^", prefix), names(df), value = TRUE)
}

all_binary_vars <- unlist(lapply(binary_vars, get_vars_by_prefix))
all_yob_vars <- get_vars_by_prefix(yob_vars)

households <- households %>%
    mutate(
        prop_male = rowSums(across(starts_with("male_"), ~ as.numeric(.)), na.rm = TRUE) / numfu,
        # Education variables are only for head and spouse
        prop_edu_cat1 = rowSums(across(starts_with("edu_cat1_"), ~ as.numeric(.)), na.rm = TRUE) / 2,
        prop_edu_cat2 = rowSums(across(starts_with("edu_cat2_"), ~ as.numeric(.)), na.rm = TRUE) / 2,
        prop_hs = rowSums(across(starts_with("hs_"), ~ as.numeric(.)), na.rm = TRUE) / 2,
        prop_ba = rowSums(across(starts_with("ba_"), ~ as.numeric(.)), na.rm = TRUE) / 2,
        prop_ma = rowSums(across(starts_with("ma_"), ~ as.numeric(.)), na.rm = TRUE) / 2, 
        mean_yob = rowMeans(across(starts_with("yob_"), ~ as.numeric(.)), na.rm = TRUE)
    )

households <- households %>%
  select(-matches("(_head$|_spouse$|child_\\d+$|other_\\d+$)"))

saveRDS(households, here("4_clean_households", "output", "clean_hs.rds"))
