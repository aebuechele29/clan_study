# ============================================================
# Makefile for clan_project
# ============================================================
# NOTE: This Makefile is written for Unix-style environments.
# It works on macOS, Linux, and Windows Git Bash. It will NOT
# work in the native Windows Command Prompt or PowerShell.
# ============================================================

SHELL := /bin/bash
R := Rscript

# ----------------
# Auto-detect PSID data files
# (filenames vary by user/extract; we match on extension only)
# ----------------
PSID_DO  := $(wildcard 0_data/fam_ind/*.do)
PSID_TXT := $(wildcard 0_data/fam_ind/*.txt)

ifeq ($(PSID_DO),)
  $(error No .do file found in 0_data/fam_ind/ — did you place your PSID extract there?)
endif
ifeq ($(PSID_TXT),)
  $(error No .txt file found in 0_data/fam_ind/ — did you place your PSID extract there?)
endif

# ----------------
# Project inputs
# ----------------
FUNCS := functions/all_functions.R \
         functions/data_utils.R \
         functions/gini_utils.R \
         functions/plot_helpers.R \
         functions/table_helpers.R \
         functions/wtd_stats.R \
         functions/sims.R

RMD        := 10_draft/write.rmd
CSL        := 10_draft/council-of-science-editors-brackets.csl
BIB        := 10_draft/bibliography_clan.bib
PAPER_PDF  := 10_draft/output/write.pdf
PAPER_DOCX := 10_draft/output/write.docx

DATA_DEPS := \
  0_data/psid.xlsx \
  0_data/cpi/cpi.xlsx \
  $(PSID_DO) \
  $(PSID_TXT)

# ----------------
# Step scripts
# ----------------
STEP1_SCRIPT := 1_build_panel/src/build_panel.R
STEP2_SCRIPT := 2_clean_panel/src/clean_panel.R
STEP3_SCRIPT := 3_households/src/households.R
STEP4_SCRIPT := 4_clans/src/clans.R
STEP5_SCRIPT := 5_summary/src/summary.R
STEP6_SCRIPT := 6_calculate_gini/src/gini_all.R
STEP7_SCRIPT := 7_gini_by_race/src/gini_by_race.R
STEP8_SCRIPT := 8_nuclear_family/src/calc_nuclear_family.R
STEP9_SCRIPT := 9_figures/src/all_figures.R

# ----------------
# Outputs / stamps
# ----------------
STEP1_OUT := 1_build_panel/output/build.rds
STEP2_OUT := 2_clean_panel/output/clean.rds

STEP3_OUTS := \
  3_households/output/robust_households.rds \
  3_households/output/robust_households_wealth.rds \
  3_households/output/households.rds \
  3_households/output/households_wealth.rds \
  3_households/output/neg_robust_households.rds \
  3_households/output/neg_robust_households_wealth.rds \
  3_households/output/mismatched.rds
STEP3_PRIMARY := 3_households/output/robust_households.rds

STEP4_OUTS := \
  4_clans/output/robust_clans.rds \
  4_clans/output/robust_clans_wealth.rds \
  4_clans/output/clans.rds \
  4_clans/output/clans_wealth.rds \
  4_clans/output/neg_robust_clans.rds \
  4_clans/output/neg_robust_clans_wealth.rds
STEP4_PRIMARY := 4_clans/output/robust_clans.rds

STEP5_OUTS := \
  5_summary/output/summary_statistics.csv \
  5_summary/output/income_quartiles.docx \
  5_summary/output/wealth_quartiles.docx
STEP5_PRIMARY := 5_summary/output/summary_statistics.csv

STEP6_OUTS := \
  6_calculate_gini/output/income.csv \
  6_calculate_gini/output/wealth_nohouse.csv \
  6_calculate_gini/output/wealth_withhome.csv
STEP6_PRIMARY := 6_calculate_gini/output/income.csv

STEP7_OUTS := \
  7_gini_by_race/output/income_race.csv \
  7_gini_by_race/output/wealth_nohouse_race.csv \
  7_gini_by_race/output/wealth_withhome_race.csv \
  7_gini_by_race/output/income_race_ratios.csv \
  7_gini_by_race/output/wealth_withhome_race_ratios.csv \
  7_gini_by_race/output/wealth_nohouse_race_ratios.csv
STEP7_PRIMARY := 7_gini_by_race/output/income_race.csv

STEP8_OUTS := \
  8_nuclear_family/output/income_C123.csv \
  8_nuclear_family/output/wealth_C123.csv \
  8_nuclear_family/output/wealth_nohouse_C123.csv
STEP8_PRIMARY := 8_nuclear_family/output/income_C123.csv

STEP9_OUTS := \
  9_figures/output/figure1.pdf \
  9_figures/output/figure2.pdf \
  9_figures/output/figure3.pdf \
  9_figures/output/appendixC1.pdf \
  9_figures/output/appendixC2.pdf \
  9_figures/output/appendixC3.pdf \
  9_figures/output/appendixC4.pdf \
  9_figures/output/appendixC5.pdf \
  9_figures/output/appendixC6.pdf \
  9_figures/output/appendixD.pdf \
  9_figures/output/appendixE1.pdf \
  9_figures/output/appendixF2.pdf \
  9_figures/output/appendixF3.pdf \
  9_figures/output/appendixF4.pdf \
  9_figures/output/appendixF5.pdf \
  9_figures/output/appendixF6.pdf
STEP9_PRIMARY := 9_figures/output/figure1.pdf

# ----------------
# Phony targets
# ----------------
.PHONY: all pipeline figures paper paper-only check-data check-scripts clean veryclean

all: paper

pipeline: $(STEP8_PRIMARY) $(STEP9_PRIMARY)

figures: $(STEP9_PRIMARY)

paper: $(PAPER_PDF) $(PAPER_DOCX)

paper-only: check-data $(RMD) $(CSL) $(BIB)
	@mkdir -p 10_draft/output
	@echo "==> Rendering paper ONLY (no pipeline)"
	@$(R) -e "rmarkdown::render('$(RMD)')"
	@test -f $(PAPER_PDF)
	@test -f $(PAPER_DOCX)
	@echo "==> Wrote: $(PAPER_PDF) and $(PAPER_DOCX)"

# ----------------
# Sanity checks
# ----------------
check-data:
	@echo "==> Checking required inputs exist"
	@for f in $(DATA_DEPS) $(FUNCS) $(RMD) $(CSL) $(BIB); do \
		if [ ! -f "$$f" ]; then \
			echo "ERROR: Missing required file: $$f"; exit 1; \
		else \
			echo "  ✓ $$f"; \
		fi; \
	done

check-scripts:
	@echo "==> Checking step scripts exist"
	@for f in $(STEP1_SCRIPT) $(STEP2_SCRIPT) $(STEP3_SCRIPT) $(STEP4_SCRIPT) $(STEP5_SCRIPT) $(STEP6_SCRIPT) $(STEP7_SCRIPT) $(STEP8_SCRIPT) $(STEP9_SCRIPT); do \
		if [ ! -f "$$f" ]; then \
			echo "ERROR: Missing script: $$f"; exit 1; \
		else \
			echo "  ✓ $$f"; \
		fi; \
	done

# ----------------
# Helper macro
# ----------------
define run_step
	@echo "==> Running: $(1)"
	@echo "    Script: $(2)"
	@$(R) -e "source('$(2)')"
endef

# ----------------
# Step rules
# ----------------
$(STEP1_OUT): check-data check-scripts $(DATA_DEPS) $(FUNCS) $(STEP1_SCRIPT)
	$(call run_step,1_build_panel,$(STEP1_SCRIPT))
	@test -f $(STEP1_OUT)

$(STEP2_OUT): check-data check-scripts $(STEP1_OUT) $(FUNCS) $(STEP2_SCRIPT)
	$(call run_step,2_clean_panel,$(STEP2_SCRIPT))
	@test -f $(STEP2_OUT)

$(STEP3_PRIMARY): check-data check-scripts $(STEP2_OUT) $(FUNCS) $(STEP3_SCRIPT)
	$(call run_step,3_households,$(STEP3_SCRIPT))
	@for f in $(STEP3_OUTS); do test -f $$f; done

$(STEP4_PRIMARY): check-data check-scripts $(STEP3_PRIMARY) $(FUNCS) $(STEP4_SCRIPT)
	$(call run_step,4_clans,$(STEP4_SCRIPT))
	@for f in $(STEP4_OUTS); do test -f $$f; done

$(STEP6_PRIMARY): check-data check-scripts $(STEP4_PRIMARY) $(FUNCS) $(STEP6_SCRIPT)
	$(call run_step,6_calculate_gini,$(STEP6_SCRIPT))
	@for f in $(STEP6_OUTS); do test -f $$f; done

$(STEP5_PRIMARY): check-data check-scripts $(STEP6_PRIMARY) $(FUNCS) $(STEP5_SCRIPT)
	$(call run_step,5_summary,$(STEP5_SCRIPT))
	@for f in $(STEP5_OUTS); do test -f $$f; done

$(STEP7_PRIMARY): check-data check-scripts $(STEP6_PRIMARY) $(FUNCS) $(STEP7_SCRIPT)
	$(call run_step,7_gini_by_race,$(STEP7_SCRIPT))
	@for f in $(STEP7_OUTS); do test -f $$f; done

$(STEP8_PRIMARY): check-data check-scripts $(STEP7_PRIMARY) $(FUNCS) $(STEP8_SCRIPT)
	$(call run_step,8_nuclear_family,$(STEP8_SCRIPT))
	@for f in $(STEP8_OUTS); do test -f $$f; done

$(STEP9_PRIMARY): check-data check-scripts $(STEP8_PRIMARY) $(FUNCS) $(STEP9_SCRIPT)
	$(call run_step,9_figures,$(STEP9_SCRIPT))
	@for f in $(STEP9_OUTS); do test -f $$f; done

# ----------------
# Render paper — both PDF and DOCX produced in one render call
# via the knit: function defined in the Rmd header
# ----------------
$(PAPER_PDF) $(PAPER_DOCX): check-data $(STEP9_PRIMARY) $(RMD) $(CSL) $(BIB)
	@mkdir -p 10_draft/output
	@echo "==> Rendering paper"
	@$(R) -e "rmarkdown::render('$(RMD)')"
	@test -f $(PAPER_PDF)
	@test -f $(PAPER_DOCX)
	@echo "==> Wrote: $(PAPER_PDF) and $(PAPER_DOCX)"

# ----------------
# Housekeeping
# ----------------
clean:
	@echo "==> Removing paper outputs"
	@rm -f $(PAPER_PDF) $(PAPER_DOCX)

veryclean: clean
	@echo "==> Removing pipeline outputs (careful)"
	@rm -f $(STEP1_OUT) $(STEP2_OUT)
	@rm -f $(STEP3_OUTS) $(STEP4_OUTS)
	@rm -f $(STEP5_OUTS) $(STEP6_OUTS) $(STEP7_OUTS) $(STEP8_OUTS)
	@rm -f $(STEP9_OUTS)