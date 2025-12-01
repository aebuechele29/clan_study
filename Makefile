dt := 0_data/
bp := 1_build_panel/
cp := 2_clean_panel/
hh := 3_households/
cl := 4_clans
su := 5_summary/
gi := 6_calculate_gini/
ra := 7_gini_by_race/
nu := 8_nuclear_family/
fi := 9_figures/
si := 10_clan_size/
fn := functions/
o := output/
s := src/

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST))) # just for script to locate itself
clan_edu := $(abspath $(an)$(o))

# Command to run R scripts
R = Rscript


				
