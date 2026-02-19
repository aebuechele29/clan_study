library(here)
library(dplyr)

# Load data
households     <- readRDS(here("3_households", "output", "households.rds"))
neg_households <- readRDS(here("3_households", "output", "neg_households.rds"))  

# Clear outputs
clear_output(here("4_clans", "output"))

clear_output(
  here("3_households", "output"),
  keep = c("households.rds", "neg_households.rds", "mismatched.rds")
)

make_clans <- function(
    households,
    prefix = "",  
    wealth_years = c(1984, 1989, 1994, seq(1999, 2021, by = 2)),
    clans_dir = here("4_clans", "output"),
    hh_dir    = here("3_households", "output")) {

  clans <- households %>%
    group_by(year, id1968) %>%
    summarise(
      numclan = n(),

      sum_all_weights = first(sum_all_weights),
      fam_id          = first(fam_id),
      num_clan_people = first(num_clan_people),
      stratum         = first(stratum),
      cluster         = first(cluster),

      across(
        .cols = c(inc_all, wealth_nohouse, wealth),
        .fns  = list(
          clan   = ~ sum(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          n      = ~ sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      ),

      black_households = sum(black_head, na.rm = TRUE),
      black_clan       = ifelse(black_households / numclan > 0.50, 1, 0),

      clan_weight = sum(fam_weight, na.rm = TRUE) / numclan,

      .groups = "drop"
    ) %>%
    rename(
      inc_all        = inc_all_clan,
      wealth_nohouse = wealth_nohouse_clan,
      wealth         = wealth_clan
    ) %>%
    mutate(
      inc_all_mean        = inc_all / numclan,
      wealth_nohouse_mean = wealth_nohouse / numclan,
      wealth_mean         = wealth / numclan
    )

  # Validate merges
  if (nrow(distinct(households, id1968, year)) != nrow(clans)) {
    stop(paste0(prefix, "Different number of distinct clans in household and clan file. Check merge."))
  }
  if (nrow(clans) != nrow(distinct(clans, id1968, year))) {
    stop(paste0(prefix, "Duplicate (id1968, year) combinations found in clans. Check merge."))
  }

  # Robust versions (include only households that belong to clans with more than one household)
  robust_clans <- clans %>% filter(numclan > 1)

  remove <- clans %>%
    filter(numclan == 1) %>%
    select(year, fam_id)

  robust_households <- households %>%
    anti_join(remove, by = c("year", "fam_id"))

  # Drop fam_id from clans outputs 
  clans        <- clans %>% select(-fam_id)
  robust_clans <- robust_clans %>% select(-fam_id)

  # Keep wealth years in wealth files
  hh_wealth <- households %>%
    filter(year %in% wealth_years)

  r_hh_wealth <- robust_households %>%
    filter(year %in% wealth_years)

  clans_wealth <- clans %>%
    filter(year %in% wealth_years)

  r_clans_wealth <- robust_clans %>%
    filter(year %in% wealth_years)

  # Save
  saveRDS(clans,         file.path(clans_dir, paste0(prefix, "clans.rds")))
  saveRDS(clans_wealth,  file.path(clans_dir, paste0(prefix, "clans_wealth.rds")))
  saveRDS(robust_clans,  file.path(clans_dir, paste0(prefix, "robust_clans.rds")))
  saveRDS(r_clans_wealth,file.path(clans_dir, paste0(prefix, "robust_clans_wealth.rds")))

  saveRDS(robust_households, file.path(hh_dir, paste0(prefix, "robust_households.rds")))
  saveRDS(hh_wealth,         file.path(hh_dir, paste0(prefix, "households_wealth.rds")))
  saveRDS(r_hh_wealth,       file.path(hh_dir, paste0(prefix, "robust_households_wealth.rds")))

  invisible(list(
    clans = clans,
    clans_wealth = clans_wealth,
    robust_clans = robust_clans,
    robust_clans_wealth = r_clans_wealth,
    robust_households = robust_households,
    households_wealth = hh_wealth,
    robust_households_wealth = r_hh_wealth
  ))
}

make_clans(households,     prefix = "")
make_clans(neg_households, prefix = "neg_")

rm(households, neg_households)

# Remove files with negative values that aren't needed
files_to_remove <- c(
  here("4_clans", "output", "neg_clans.rds"),
  here("4_clans", "output", "neg_clans_wealth.rds"),
  here("3_households", "output", "neg_households_wealth.rds"),
  here("3_households", "output", "neg_households.rds")
)

file.remove(files_to_remove)
