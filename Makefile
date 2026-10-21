# ============================================================
# Makefile for clan_project
# ============================================================
# Run from the project root (the folder containing this file).
#
#   make              build everything: pipeline, figures, paper, supplement
#   make help         list all targets
#
# Works with the make that ships with macOS (GNU make 3.81), Linux,
# and Git Bash on Windows. It will NOT work in Command Prompt/PowerShell.
#
# How it decides what to rerun
#   Each step leaves a stamp file in .make/ when it finishes. A step reruns
#   only when its script, a functions/ file it uses, its raw data, or an
#   upstream step has changed since its stamp was written.
#   To force one step to rerun:   make redo-<step>    (e.g. make redo-clans)
#   To rerun everything:          make veryclean && make
# ============================================================

SHELL := /bin/bash
R     := Rscript
STAMP := .make

.DEFAULT_GOAL := all
.DELETE_ON_ERROR:

# ----------------
# Raw data
# (PSID extract filenames vary by user; matched on extension)
# ----------------
PSID_DO   := $(wildcard 0_data/fam_ind/*.do)
PSID_TXT  := $(wildcard 0_data/fam_ind/*.txt)
PSID_XLSX := 0_data/psid.xlsx
CPI_XLSX  := 0_data/cpi/cpi.xlsx

# ----------------
# Function files
# ----------------
F_ALL     := functions/all_functions.R
F_DATA    := functions/data_utils.R
F_GINI    := functions/gini_utils.R
F_WTD     := functions/wtd_stats.R
F_PLOT    := functions/plot_helpers.R
F_TABLE   := functions/table_helpers.R
F_SIMS    := functions/sims.R
FUNCS     := $(F_ALL) $(F_DATA) $(F_GINI) $(F_WTD) $(F_PLOT) $(F_TABLE) $(F_SIMS)

# ----------------
# Step scripts
# ----------------
S1 := 1_build_panel/src/build_panel.R
S2 := 2_clean_panel/src/clean_panel.R
S3 := 3_households/src/households.R
S4 := 4_clans/src/clans.R
S5 := 5_summary/src/summary.R
S6 := 6_calculate_gini/src/gini_all.R
S7 := 7_gini_by_race/src/gini_by_race.R
S8 := 8_nuclear_family/src/calc_nuclear_family.R
S9 := 9_figures/src/all_figures.R
FIG_MAIN     := 9_figures/src/figure_main.R
FIG_APPENDIX := 9_figures/src/figure_appendix.R
SCRIPTS := $(S1) $(S2) $(S3) $(S4) $(S5) $(S6) $(S7) $(S8) $(S9) $(FIG_MAIN) $(FIG_APPENDIX)

# Everything the figure scripts need (they source all_functions.R)
FIG_CODE := $(S9) $(FIG_MAIN) $(FIG_APPENDIX) $(F_ALL) $(F_DATA) $(F_GINI) \
            $(F_WTD) $(F_PLOT) $(F_TABLE)

# ----------------
# Paper
# ----------------
RMD       := 10_draft/write.rmd
SUPP_RMD  := 10_draft/supplemental.rmd
CSL       := 10_draft/council-of-science-editors-brackets.csl
BIB       := 10_draft/bibliography_clan.bib
DRAFT_OUT := 10_draft/output

# ----------------
# Outputs of each step (checked after the step runs)
# ----------------
H := 3_households/output
C := 4_clans/output

OUT1 := 1_build_panel/output/build.rds

OUT2 := 2_clean_panel/output/clean.rds

# households.R also writes neg_households.rds, but clans.R deletes it once the
# neg_ kin-group files are built, so it is not checked as a lasting output.
NEG_HH := $(H)/neg_households.rds
OUT3   := $(H)/households.rds $(H)/mismatched.rds

# clans.R writes kin-group files AND the robust/wealth household files.
# For the negative-value (neg_) versions only the robust files are kept;
# clans.R deletes neg_clans, neg_clans_wealth, neg_households_wealth and
# neg_households, so those are not checked.
OUT4 := $(C)/clans.rds \
        $(C)/clans_wealth.rds \
        $(C)/robust_clans.rds        $(C)/neg_robust_clans.rds \
        $(C)/robust_clans_wealth.rds $(C)/neg_robust_clans_wealth.rds \
        $(H)/robust_households.rds        $(H)/neg_robust_households.rds \
        $(H)/households_wealth.rds \
        $(H)/robust_households_wealth.rds $(H)/neg_robust_households_wealth.rds

OUT5 := $(addprefix 5_summary/output/, \
          summary_statistics.csv \
          inc_family_demo.csv w_family_demo.csv \
          inc_family_demo_full.csv w_family_demo_full.csv \
          income_quartiles.docx wealth_quartiles.docx)

OUT6 := $(addprefix 6_calculate_gini/output/, \
          income.csv wealth_nohouse.csv wealth_withhome.csv)

OUT7 := $(addprefix 7_gini_by_race/output/, \
          income_race.csv wealth_nohouse_race.csv \
          income_race_ratios.csv wealth_nohouse_race_ratios.csv \
          wealth_withhome_race_ratios.csv)

OUT8 := $(addprefix 8_nuclear_family/output/, \
          income_C123.csv wealth_nohouse_C123.csv wealth_C123.csv)

OUT9 := $(addprefix 9_figures/output/, \
          figure1.pdf figure2.pdf figure3.pdf table1.docx \
          appendixB.pdf \
          appendixD1.pdf appendixD2.pdf appendixD3.pdf appendixD4.pdf \
          appendixD5.pdf appendixD6.pdf appendixD7.pdf \
          appendixE1.pdf appendixE2.pdf appendixE3.pdf appendixE4.pdf \
          appendixE5.pdf appendixE6.pdf appendixE7.pdf \
          appendixF2.pdf appendixF3.pdf appendixF4.pdf \
          appendix_tableF1.docx)

PAPER_OUT := $(DRAFT_OUT)/write.pdf $(DRAFT_OUT)/write.docx
SUPP_OUT  := $(DRAFT_OUT)/supplemental.pdf $(DRAFT_OUT)/supplemental.docx

# NOTE: figure_appendix.R reads 7_gini_by_race/output/wealth_withhome_race.csv,
# but gini_by_race.R does not currently write that file (it builds
# wh_by_year_race but never saves it). Until it does, the existing copy is
# treated as a required input and is never deleted by `make veryclean`.
LEGACY_INPUTS := 7_gini_by_race/output/wealth_withhome_race.csv

# ----------------
# Helpers
# ----------------
# Run a script in a fresh R session
define run_step
	@echo ""
	@echo "==> $(1)  ($(2))"
	@$(R) -e "source('$(2)')"
endef

# Same, but load functions/ first. Used for scripts that call helper
# functions without sourcing all_functions.R themselves.
define run_step_fn
	@echo ""
	@echo "==> $(1)  ($(2))"
	@$(R) -e "source('$(F_ALL)'); source('$(2)')"
endef

# Fail loudly if a step did not produce everything it should have
define check_outputs
	@for f in $(1); do \
	  if [ ! -f "$$f" ]; then echo "ERROR: expected output not created: $$f"; exit 1; fi; \
	done
endef

# ----------------
# Main targets
# ----------------
.PHONY: all pipeline figures paper supplemental help \
        build-panel clean-panel households clans summary gini gini-race nuclear-family \
        check-data check-legacy clean clean-figures veryclean

all: figures paper supplemental

pipeline: $(STAMP)/5_summary $(STAMP)/6_calculate_gini $(STAMP)/7_gini_by_race $(STAMP)/8_nuclear_family

figures:      $(STAMP)/9_figures
paper:        $(STAMP)/paper
supplemental: $(STAMP)/supplemental

# Friendly names for single steps (also builds anything upstream that is stale)
build-panel:    $(STAMP)/1_build_panel
clean-panel:    $(STAMP)/2_clean_panel
households:     $(STAMP)/3_households
clans:          $(STAMP)/4_clans
summary:        $(STAMP)/5_summary
gini:           $(STAMP)/6_calculate_gini
gini-race:      $(STAMP)/7_gini_by_race
nuclear-family: $(STAMP)/8_nuclear_family

# Force one step to rerun, e.g. `make redo-clans`. Later steps that depend on
# it will rerun the next time you run `make`.
STEP_build-panel    := 1_build_panel
STEP_clean-panel    := 2_clean_panel
STEP_households     := 3_households
STEP_clans          := 4_clans
STEP_summary        := 5_summary
STEP_gini           := 6_calculate_gini
STEP_gini-race      := 7_gini_by_race
STEP_nuclear-family := 8_nuclear_family
STEP_figures        := 9_figures
STEP_paper          := paper
STEP_supplemental   := supplemental

redo-%:
	@if [ -z "$(STEP_$*)" ]; then echo "Unknown step: $*  (see make help)"; exit 1; fi
	@rm -f $(STAMP)/$(STEP_$*)
	@$(MAKE) --no-print-directory $(STAMP)/$(STEP_$*)

help:
	@echo "Targets:"
	@echo "  make                 everything (figures, paper, supplemental)"
	@echo "  make pipeline        steps 1-8 (data through nuclear-family results)"
	@echo "  make figures         step 9: figures and tables in 9_figures/output"
	@echo "  make paper           10_draft/output/write.pdf and write.docx"
	@echo "  make supplemental    10_draft/output/supplemental.pdf and .docx"
	@echo ""
	@echo "  Single steps (plus anything upstream that is out of date):"
	@echo "    build-panel clean-panel households clans summary gini gini-race nuclear-family"
	@echo "  make redo-<step>     force one step to rerun, e.g. make redo-clans"
	@echo "  make -n <target>     show what would run without running it"
	@echo ""
	@echo "  make clean           remove paper and supplemental outputs"
	@echo "  make clean-figures   remove figure outputs"
	@echo "  make veryclean       remove ALL pipeline outputs (full rerun next time)"

# ----------------
# Checks (order-only: they run first but never force a rebuild)
# ----------------
check-data:
	@echo "==> Checking required inputs"
	@if [ -z "$(PSID_DO)" ];  then echo "ERROR: no .do file in 0_data/fam_ind/ (place your PSID extract there)";  exit 1; fi
	@if [ -z "$(PSID_TXT)" ]; then echo "ERROR: no .txt file in 0_data/fam_ind/ (place your PSID extract there)"; exit 1; fi
	@for f in $(PSID_XLSX) $(CPI_XLSX) $(FUNCS) $(SCRIPTS) $(RMD) $(SUPP_RMD) $(CSL) $(BIB); do \
	  if [ ! -f "$$f" ]; then echo "ERROR: missing required file: $$f"; exit 1; fi; \
	done
	@echo "    all inputs present"

check-legacy:
	@for f in $(LEGACY_INPUTS); do \
	  if [ ! -f "$$f" ]; then \
	    echo "ERROR: $$f is missing."; \
	    echo "       figure_appendix.R reads it, but gini_by_race.R does not write it."; \
	    exit 1; \
	  fi; \
	done

$(STAMP):
	@mkdir -p $(STAMP)

# ----------------
# Pipeline steps
# ----------------
$(STAMP)/1_build_panel: $(S1) $(PSID_XLSX) $(PSID_DO) $(PSID_TXT) | check-data $(STAMP)
	@mkdir -p 1_build_panel/output
	$(call run_step,Step 1: build panel,$(S1))
	$(call check_outputs,$(OUT1))
	@touch $@

$(STAMP)/2_clean_panel: $(STAMP)/1_build_panel $(S2) $(CPI_XLSX) $(F_ALL) $(F_WTD) | check-data $(STAMP)
	@mkdir -p 2_clean_panel/output
	$(call run_step_fn,Step 2: clean panel,$(S2))
	$(call check_outputs,$(OUT2))
	@touch $@

$(STAMP)/3_households: $(STAMP)/2_clean_panel $(S3) $(F_ALL) $(F_WTD) | check-data $(STAMP)
	@mkdir -p 3_households/output
	$(call run_step_fn,Step 3: households,$(S3))
	$(call check_outputs,$(OUT3) $(NEG_HH))
	@touch $@

# clans.R needs neg_households.rds but deletes it when it finishes, so if
# step 4 reruns on its own (e.g. clans.R was edited) step 3 is rerun first
# to recreate it.
$(STAMP)/4_clans: $(STAMP)/3_households $(S4) $(F_ALL) $(F_DATA) | check-data $(STAMP)
	@mkdir -p 4_clans/output
	@if [ ! -f "$(NEG_HH)" ]; then \
	  echo ""; \
	  echo "==> $(NEG_HH) was removed by the last clans run; rerunning step 3 first"; \
	  $(R) -e "source('$(F_ALL)'); source('$(S3)')" || exit 1; \
	  touch $(STAMP)/3_households; \
	fi
	$(call run_step_fn,Step 4: kin groups (clans),$(S4))
	$(call check_outputs,$(OUT4))
	@touch $@

$(STAMP)/6_calculate_gini: $(STAMP)/4_clans $(S6) $(F_ALL) $(F_GINI) | check-data $(STAMP)
	@mkdir -p 6_calculate_gini/output
	$(call run_step_fn,Step 6: Gini coefficients,$(S6))
	$(call check_outputs,$(OUT6))
	@touch $@

# summary.R reads the step 6 Gini files, so it runs after step 6
$(STAMP)/5_summary: $(STAMP)/4_clans $(STAMP)/6_calculate_gini $(S5) | check-data $(STAMP)
	@mkdir -p 5_summary/output
	$(call run_step,Step 5: summary statistics,$(S5))
	$(call check_outputs,$(OUT5))
	@touch $@

$(STAMP)/7_gini_by_race: $(STAMP)/4_clans $(S7) $(F_ALL) $(F_GINI) $(F_WTD) | check-data $(STAMP)
	@mkdir -p 7_gini_by_race/output
	$(call run_step,Step 7: Gini by race,$(S7))
	$(call check_outputs,$(OUT7))
	@touch $@

$(STAMP)/8_nuclear_family: $(STAMP)/4_clans $(S8) $(F_ALL) $(F_GINI) | check-data $(STAMP)
	@mkdir -p 8_nuclear_family/output
	$(call run_step_fn,Step 8: C1/C2/C3 coefficients,$(S8))
	$(call check_outputs,$(OUT8))
	@touch $@

# Everything the figure scripts read
ANALYSIS := $(STAMP)/4_clans $(STAMP)/5_summary $(STAMP)/6_calculate_gini \
            $(STAMP)/7_gini_by_race $(STAMP)/8_nuclear_family

$(STAMP)/9_figures: $(ANALYSIS) $(FIG_CODE) | check-data check-legacy $(STAMP)
	@mkdir -p 9_figures/output
	$(call run_step,Step 9: figures and tables,$(S9))
	$(call check_outputs,$(OUT9))
	@touch $@

# ----------------
# Paper and supplement
# Both formats are rendered explicitly into 10_draft/output. (The knit:
# field in the YAML header is only used by RStudio's Knit button.)
# write.rmd sources all_figures.R itself, so it needs the same inputs as
# the figures step rather than the figure files.
# ----------------
$(STAMP)/paper: $(ANALYSIS) $(FIG_CODE) $(RMD) $(CSL) $(BIB) | check-data check-legacy $(STAMP)
	@mkdir -p $(DRAFT_OUT)
	@echo ""
	@echo "==> Rendering paper"
	@$(R) -e "rmarkdown::render('$(RMD)', output_format = 'all', output_dir = '$(DRAFT_OUT)')"
	$(call check_outputs,$(PAPER_OUT))
	@touch $@
	@echo "==> Wrote: $(PAPER_OUT)"

$(STAMP)/supplemental: $(ANALYSIS) $(FIG_CODE) $(F_SIMS) $(SUPP_RMD) $(CSL) $(BIB) | check-data check-legacy $(STAMP)
	@mkdir -p $(DRAFT_OUT)
	@echo ""
	@echo "==> Rendering supplemental information"
	@$(R) -e "rmarkdown::render('$(SUPP_RMD)', output_format = 'all', output_dir = '$(DRAFT_OUT)')"
	$(call check_outputs,$(SUPP_OUT))
	@touch $@
	@echo "==> Wrote: $(SUPP_OUT)"

# ----------------
# Housekeeping
# ----------------
clean:
	@echo "==> Removing paper and supplemental outputs"
	@rm -f $(PAPER_OUT) $(SUPP_OUT) $(STAMP)/paper $(STAMP)/supplemental

clean-figures:
	@echo "==> Removing figure outputs"
	@rm -f $(OUT9) $(STAMP)/9_figures

veryclean: clean clean-figures
	@echo "==> Removing all pipeline outputs (careful: full rerun takes a while)"
	@rm -f $(OUT1) $(OUT2) $(OUT3) $(NEG_HH) $(OUT4) $(OUT5) $(OUT6) $(OUT7) $(OUT8)
	@rm -rf $(STAMP)