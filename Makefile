dt := 0_data/
bp := 1_build_panel/
cp := 2_clean_panel/
ch := 3_create_households/
h := 4_clean_households/
cc := 5_create_clans/
c := 6_clean_clans/
an := 7_analysis/
fn := functions/
o := output/
s := src/

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST))) # just for script to locate itself
clan_edu := $(mkfile_path)/../6_clean_clans/output/cl

# Command to run R scripts
R = R --no-save --args "" < "$<"

all: $(clan_edu)

# make -n 				

# Step 1: Build panel
$(bp)(o)build.rds: $(bp)(s)build_panel.R $(dt)cpi.xlsx \
$(dt)fam_ind/J347157.do \
$(dt)fam_ind/J347157.txt \
$(dt)fims/20250415_grandparents.xlsx \
$(dt)fims/20250415_parents.xlsx \
$(dt)psid.xlsx
    $(R)

# Step 2: Clean panel
$(cp)(o)clean.rds: $(cp)(s)clean_panel.R $(bp)(o)build.rds $(fn)functions.R
    $(R)

# Step 3: Create households
$(ch)(o)households.rds: $(ch)(s)households.R $(cp)(o)clean.rds 
    $(R)

# Step 4: Clean households
$(h)(o)clean_hs.rds: $(h)(s)clean_households.R $(ch)(o)households.rds 
    $(R)

# Step 5: Create clans
$(cc)(o)clans.rds: $(cc)(s)clans.R $(h)(o)clean_hs.rds 
    $(R)

# Step 6: Clean clans
$(c)(o)clean_clans.rds: $(c)(s)clean_clans.R $(cc)(o)clans.rds 
    $(R)

# Step 7: Analysis
$(an)(o)analysis.pdf: $(an)(s)analysis.R $(c)(o)clean_clans.rds \
$(h)(o)clean_hs.rds
    $(R)

.PHONY: clean
