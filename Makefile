# ============================================================
# Makefile for clan_project (robust to variable output prefixes)
# - Runs each step in a fresh R session
# - Uses file targets when outputs are stable
# - Uses stamp (.done) targets when outputs can vary (prefix-based)
# - Always builds appendix for pipeline + paper
# - Renders 11_draft/write.rmd with CSL + bib
# ============================================================

SHELL := /bin/bash
R := Rscript

# ----------------
# Project inputs
# ----------------
FUNCS := functions/functions.R

RMD       := 11_draft/write.rmd
CSL       := 11_draft/council-of-science-editors-brackets.csl
BIB       := 11_draft/bibliography_clan.bib
PAPER_PDF := 11_draft/output/paper.pdf

DATA_DEPS := \
  0_data/psid.xlsx \
  0_data/cpi/cpi.xlsx \
  0_data/fam_ind/J357217.do \
  0_data/fam_ind/J357217.txt

# ----------------
# Step scripts
# ----------------
STEP1_SCRIPT  := 1_build_panel/src/build_panel.R
STEP2_SCRIPT  := 2_clean_panel/src/clean_panel.R
STEP3_SCRIPT  := 3_households/src/households.R
STEP4_SCRIPT  := 4_clans/src/clans.R
STEP5_SCRIPT  := 5_summary/src/summary.R
STEP6_SCRIPT  := 6_calculate_gini/src/gini_all.R
STEP7_SCRIPT  := 7_gini_by_race/src/gini_by_race.R
STEP8_SCRIPT  := 8_nuclear_family/src/calc_nuclear_family.R
STEP9_SCRIPT  := 9_figures/src/figures.R
STEP10_SCRIPT := 10_appendix/src/appendix.R

# ----------------
# Outputs / stamps
# ----------------
STEP1_OUT := 1_build_panel/output/build.rds
STEP2_OUT := 2_clean_panel/output/clean.rds

# Step 3/4 have variable/expanding outputs (and step 4 uses prefix)
STEP3_DONE := 3_households/output/.done
STEP4_DONE := 4_clans/output/.done

# Step 5 stable outputs
STEP5_OUTS := \
  5_summary/output/summary_statistics.csv \
  5_summary/output/income_quartiles.docx \
  5_summary/output/wealth_quartiles.docx
STEP5_PRIMARY := 5_summary/output/summary_statistics.csv

# Step 6 stable outputs
STEP6_OUTS := \
  6_calculate_gini/output/income.csv \
  6_calculate_gini/output/wealth_nohouse.csv \
  6_calculate_gini/output/wealth_withhome.csv
STEP6_PRIMARY := 6_calculate_gini/output/income.csv

# Step 7 stable outputs
STEP7_OUTS := \
  7_gini_by_race/output/income_race.csv \
  7_gini_by_race/output/wealth_nohouse_race.csv \
  7_gini_by_race/output/wealth_withhome_race.csv
STEP7_PRIMARY := 7_gini_by_race/output/income_race.csv

# Step 8 stable outputs
STEP8_OUTS := \
  8_nuclear_family/output/income_C123.csv \
  8_nuclear_family/output/wealth_nohouse_C123.csv
STEP8_PRIMARY := 8_nuclear_family/output/income_C123.csv

# Step 9 figure outputs
FIG_DIR := 9_figures/output
FIG_OUTS := \
  $(FIG_DIR)/figure2.pdf \
  $(FIG_DIR)/figure3.pdf \
  $(FIG_DIR)/table4.docx

# Table1 casing can vary across systems; accept either
TABLE1_CANON := $(FIG_DIR)/Table1.docx
TABLE1_ALT   := $(FIG_DIR)/table1.docx

# Step 10 appendix output
APPENDIX_OUT := 10_appendix/output/appendices.docx

# ----------------
# Phony targets
# ----------------
.PHONY: all pipeline figures appendix paper paper-only check-data check-scripts clean veryclean table1

all: paper

# Pipeline now includes appendix (Step 10)
pipeline: $(APPENDIX_OUT)

figures: $(FIG_OUTS) table1
appendix: $(APPENDIX_OUT)

# Paper ALWAYS depends on appendix (and figures)
paper: $(PAPER_PDF)

paper-only: check-data $(APPENDIX_OUT) $(RMD) $(CSL) $(BIB)
	@mkdir -p 11_draft/output
	@echo "==> Rendering paper ONLY (no pipeline except appendix)"
	@$(R) -e "rmarkdown::render('$(RMD)', output_file='paper.pdf', output_dir='11_draft/output')"
	@test -f $(PAPER_PDF)
	@echo "==> Wrote: $(PAPER_PDF)"

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
	@for f in $(STEP1_SCRIPT) $(STEP2_SCRIPT) $(STEP3_SCRIPT) $(STEP4_SCRIPT) $(STEP5_SCRIPT) $(STEP6_SCRIPT) $(STEP7_SCRIPT) $(STEP8_SCRIPT) $(STEP9_SCRIPT) $(STEP10_SCRIPT); do \
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
	@$(R) -e "source('$(FUNCS)'); source('$(2)')"
endef

# ----------------
# Step rules
# ----------------

# Step 1 produces a stable output file
$(STEP1_OUT): check-data check-scripts $(DATA_DEPS) $(FUNCS) $(STEP1_SCRIPT)
	$(call run_step,1_build_panel,$(STEP1_SCRIPT))
	@test -f $(STEP1_OUT)

# Step 2 produces a stable output file
$(STEP2_OUT): check-data check-scripts $(STEP1_OUT) $(FUNCS) $(STEP2_SCRIPT)
	$(call run_step,2_clean_panel,$(STEP2_SCRIPT))
	@test -f $(STEP2_OUT)

# Step 3: variable outputs → stamp
$(STEP3_DONE): check-data check-scripts $(STEP2_OUT) $(FUNCS) $(STEP3_SCRIPT)
	$(call run_step,3_households,$(STEP3_SCRIPT))
	@mkdir -p 3_households/output
	@touch $(STEP3_DONE)

# Step 4: prefix-based outputs → stamp
$(STEP4_DONE): check-data check-scripts $(STEP3_DONE) $(FUNCS) $(STEP4_SCRIPT)
	$(call run_step,4_clans,$(STEP4_SCRIPT))
	@mkdir -p 4_clans/output
	@touch $(STEP4_DONE)

# Step 5+: stable outputs
$(STEP6_PRIMARY): check-data check-scripts $(STEP4_DONE) $(FUNCS) $(STEP6_SCRIPT)
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

# Step 9 figures: any of these outputs triggers the same run, then we verify all
$(FIG_OUTS): check-data check-scripts $(STEP8_PRIMARY) $(FUNCS) $(STEP9_SCRIPT)
	$(call run_step,9_figures,$(STEP9_SCRIPT))
	@for f in $(FIG_OUTS); do test -f $$f; done

# Table 1 check (case-insensitive friendly)
table1: check-data check-scripts $(STEP8_PRIMARY) $(FUNCS) $(STEP9_SCRIPT)
	@if [ -f "$(TABLE1_CANON)" ] || [ -f "$(TABLE1_ALT)" ]; then \
		echo "==> Table1 exists"; \
	else \
		echo "==> Table1 missing; re-running 9_figures"; \
		$(R) -e "source('$(FUNCS)'); source('$(STEP9_SCRIPT)')"; \
	fi
	@if [ -f "$(TABLE1_CANON)" ]; then \
		echo "  ✓ $(TABLE1_CANON)"; \
	elif [ -f "$(TABLE1_ALT)" ]; then \
		echo "  ✓ $(TABLE1_ALT)"; \
	else \
		echo "ERROR: Missing Table1 output (expected $(TABLE1_CANON) or $(TABLE1_ALT))"; \
		exit 1; \
	fi

# Step 10 appendix: stable output docx
$(APPENDIX_OUT): check-data check-scripts $(STEP8_PRIMARY) $(STEP6_PRIMARY) $(FUNCS) $(STEP10_SCRIPT)
	$(call run_step,10_appendix,$(STEP10_SCRIPT))
	@test -f $(APPENDIX_OUT)

# ----------------
# Render paper (ALWAYS includes appendix)
# ----------------
$(PAPER_PDF): check-data $(FIG_OUTS) table1 $(APPENDIX_OUT) $(RMD) $(CSL) $(BIB)
	@mkdir -p 11_draft/output
	@echo "==> Rendering paper"
	@$(R) -e "rmarkdown::render('$(RMD)', output_file='paper.pdf', output_dir='11_draft/output')"
	@test -f $(PAPER_PDF)
	@echo "==> Wrote: $(PAPER_PDF)"

# ----------------
# Housekeeping
# ----------------
clean:
	@echo "==> Removing paper PDF"
	@rm -f $(PAPER_PDF)

veryclean: clean
	@echo "==> Removing pipeline outputs + stamps (careful)"
	@rm -f $(STEP1_OUT) $(STEP2_OUT)
	@rm -f $(STEP3_DONE) $(STEP4_DONE)
	@rm -f $(STEP5_OUTS) $(STEP6_OUTS) $(STEP7_OUTS) $(STEP8_OUTS)
	@rm -f $(FIG_OUTS) $(TABLE1_CANON) $(TABLE1_ALT)
	@rm -f $(APPENDIX_OUT)

