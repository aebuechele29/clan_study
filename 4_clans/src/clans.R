library(here)
library(dplyr)

# LOAD DATA ------------------------------------------------------------------
households <- readRDS(
  here("3_households", "output", "households.rds")
)

neg_households <- readRDS(
  here("3_households", "output", "neg_households.rds")
)


# CLEAR OUTPUTS ---------------------------------------------------------------
clear_output(
  here("4_clans", "output")
)

clear_output(
  here("3_households", "output"),
  keep = c(
    "households.rds",
    "neg_households.rds",
    "mismatched.rds"
  )
)


# CREATE CLAN FILES -----------------------------------------------------------
make_clans <- function(
    households,
    prefix = "",
    wealth_years = c(1984, 1989, 1994, seq(1999, 2023, by = 2)),
    clans_dir = here("4_clans", "output"),
    hh_dir = here("3_households", "output")) {

  clans <- households %>%
    group_by(year, id1968) %>%
    summarise(
      # Number of households in the clan
      numclan = n(),

      # Clan and sample identifiers
      sum_all_weights = first(sum_all_weights),
      fam_id = first(fam_id),
      num_clan_people = first(num_clan_people),
      stratum = first(stratum),
      cluster = first(cluster),

      # Clan demographic and family-structure variables
      clan_age_mean = first(clan_age_mean),
      clan_children_hh_mean = first(clan_children_hh_mean),
      clan_other_hh_mean = first(clan_other_hh_mean),
      clan_other_prop = first(clan_other_prop),
      clan_headspouse_prop = first(clan_headspouse_prop),

      # Clan income and wealth variables
      across(
        .cols = c(
          inc_all,
          wealth_nohouse,
          wealth
        ),
        .fns = list(
          clan = ~ sum(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          n = ~ sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      ),

      # Clan race variables
      black_households = sum(black_head, na.rm = TRUE),
      black_clan = ifelse(
        black_households / numclan > 0.50,
        1,
        0
      ),

      # Average household weight within the clan
      clan_weight = sum(fam_weight, na.rm = TRUE) / numclan,

      .groups = "drop"
    ) %>%
    rename(
      inc_all = inc_all_clan,
      wealth_nohouse = wealth_nohouse_clan,
      wealth = wealth_clan
    ) %>%
    mutate(
      inc_all_mean = inc_all / numclan,
      wealth_nohouse_mean = wealth_nohouse / numclan,
      wealth_mean = wealth / numclan
    )


  # VALIDATE CLAN FILE --------------------------------------------------------
  if (nrow(distinct(households, id1968, year)) != nrow(clans)) {
    stop(
      paste0(
        prefix,
        "Different number of distinct clans in household and clan file. ",
        "Check merge."
      )
    )
  }

  if (nrow(clans) != nrow(distinct(clans, id1968, year))) {
    stop(
      paste0(
        prefix,
        "Duplicate (id1968, year) combinations found in clans. ",
        "Check merge."
      )
    )
  }


  # CREATE ROBUST FILES -------------------------------------------------------
  # Include only households belonging to clans with more than one household

  robust_clans <- clans %>%
    filter(numclan > 1)

  remove <- clans %>%
    filter(numclan == 1) %>%
    select(year, fam_id)

  robust_households <- households %>%
    anti_join(
      remove,
      by = c("year", "fam_id")
    )


  # DROP FAMILY ID FROM CLAN OUTPUTS ------------------------------------------
  clans <- clans %>%
    select(-fam_id)

  robust_clans <- robust_clans %>%
    select(-fam_id)


  # CREATE WEALTH-YEAR FILES --------------------------------------------------
  hh_wealth <- households %>%
    filter(year %in% wealth_years)

  r_hh_wealth <- robust_households %>%
    filter(year %in% wealth_years)

  clans_wealth <- clans %>%
    filter(year %in% wealth_years)

  r_clans_wealth <- robust_clans %>%
    filter(year %in% wealth_years)


  # SAVE OUTPUTS --------------------------------------------------------------
  saveRDS(
    clans,
    file.path(
      clans_dir,
      paste0(prefix, "clans.rds")
    )
  )

  saveRDS(
    clans_wealth,
    file.path(
      clans_dir,
      paste0(prefix, "clans_wealth.rds")
    )
  )

  saveRDS(
    robust_clans,
    file.path(
      clans_dir,
      paste0(prefix, "robust_clans.rds")
    )
  )

  saveRDS(
    r_clans_wealth,
    file.path(
      clans_dir,
      paste0(prefix, "robust_clans_wealth.rds")
    )
  )

  saveRDS(
    robust_households,
    file.path(
      hh_dir,
      paste0(prefix, "robust_households.rds")
    )
  )

  saveRDS(
    hh_wealth,
    file.path(
      hh_dir,
      paste0(prefix, "households_wealth.rds")
    )
  )

  saveRDS(
    r_hh_wealth,
    file.path(
      hh_dir,
      paste0(prefix, "robust_households_wealth.rds")
    )
  )


  # RETURN CREATED DATA -------------------------------------------------------
  invisible(
    list(
      clans = clans,
      clans_wealth = clans_wealth,
      robust_clans = robust_clans,
      robust_clans_wealth = r_clans_wealth,
      robust_households = robust_households,
      households_wealth = hh_wealth,
      robust_households_wealth = r_hh_wealth
    )
  )
}


# CREATE FILES WITH BOUNDED AND UNBOUNDED VALUES ------------------------------
make_clans(
  households,
  prefix = ""
)

make_clans(
  neg_households,
  prefix = "neg_"
)

rm(households, neg_households)


# REMOVE UNNEEDED FILES WITH NEGATIVE VALUES ----------------------------------
files_to_remove <- c(
  here("4_clans", "output", "neg_clans.rds"),
  here("4_clans", "output", "neg_clans_wealth.rds"),
  here("3_households", "output", "neg_households_wealth.rds"),
  here("3_households", "output", "neg_households.rds")
)

file.remove(files_to_remove)


