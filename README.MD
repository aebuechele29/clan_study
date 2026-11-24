This repository contains the R source code that produces every result, figure, and table in the paper and its supplementary information. The code starts from raw Panel Study of Income Dynamics (PSID) public-use data, builds households and kin groups (extended families), and computes Gini coefficients and racial wealth ratios for both. A single command, `make`, runs the full pipeline and renders the manuscript.

## Contents

1. [System requirements](#1-system-requirements)
2. [Installation guide](#2-installation-guide)
3. [Demo, Instructions for Use, and Reproduction](#3-demo-instructions-)
4. [Troubleshooting](#7-troubleshooting)
5. [Additional information](#6-additional-information)

---

## 1. System requirements

### Operating systems

| | |
|---|---|
| **Tested on** | macOS Sequoia 15.4.1, Apple silicon (arm64) |
| **Should also work on** | Linux; Windows using Git Bash with Rtools. These have not been tested. |

### Software dependencies

| Software | Version tested | Purpose |
|---|---|---|
| R | 4.4.2 | Runs all analysis code |
| GNU Make | 3.81 (ships with macOS) | Runs the pipeline steps in order |
| pandoc | 3.8.2.1 | Renders the manuscript from R Markdown |
| LaTeX (TinyTeX recommended) | TeX Live 2025 | Builds the PDF versions of the manuscript |
| renv | 1.1.5 | Installs the exact R package versions below |

**R packages.** All R package versions are pinned in `renv.lock` and installed automatically (see [Installation guide](#2-installation-guide)). The packages the code uses directly are:

| Package | Version | | Package | Version |
|---|---|---|---|---|
| convey | 1.0.1 | | officer | 0.6.10 |
| cowplot | 1.1.3 | | openxlsx | 4.2.8 |
| data.table | 1.18.2.1 | | psidR | 2.3 |
| dplyr | 1.1.4 | | purrr | 1.0.4 |
| easyPSID (GitHub) | 0.1.1 | | readr | 2.1.5 |
| flextable | 0.9.9 | | rlang | 1.1.7 |
| ggplot2 | 3.5.1 | | rmarkdown | 2.29 |
| gridExtra | 2.3 | | scales | 1.4.0 |
| here | 1.0.1 | | survey | 4.4-2 |
| ineq | 0.2-13 | | tibble | 3.2.1 |
| knitr | 1.50 | | tidyr | 1.3.1 |
| Matrix | 1.7-1 | | tidyverse | 2.0.0 |

### Hardware

No non-standard hardware is required. The code runs on a standard desktop or laptop computer. 

---

## 2. Installation guide

### Instructions

1. **Install R 4.4.x** from https://cran.r-project.org.
2. **Install GNU Make.**
   - macOS: run `xcode-select --install` in a terminal.
   - Linux: usually already installed.
   - Windows: install Rtools, and run every command in this README in Git Bash.
3. **Install pandoc** from https://pandoc.org (on macOS: `brew install pandoc`). RStudio includes pandoc, but a terminal often cannot find RStudio's copy.
4. **Install LaTeX.** The easiest option is TinyTeX: in R, run `install.packages("tinytex")` and then `tinytex::install_tinytex()`.
5. **Download this repository** and open a terminal in its top folder (the one containing the `Makefile`).
6. **Install the R packages** by running:

   ```bash
   make setup
   ```

   This installs the exact package versions in `renv.lock` into a library inside the project, so other R projects are not affected. It then reports the versions of R, pandoc, and LaTeX it found. It needs internet access, including to GitHub (for `easyPSID`).

### Typical install time

About 2 minutes on a normal desktop computer. 

---

## 3. Demo, Instructions for Use, and Reproduction

These steps run the code on PSID data. 

### Step 1: Download code and data structure
Download all scripts and the file structure via Github. First, go to https://github.com/aebuechele29/clan_study.git and click the blue "<>Code" button on the top right. Second, click "Download Zip."

### Step 2: Get the PSID data

The PSID's Conditions of Use do not allow us to share the data, including extracts and files derived from them. Anyone can get the same data for online:

1. **Register** for a free account at https://psidonline.isr.umich.edu and accept the Conditions of Use.
2. **Access our data cart** at https://simba.isr.umich.edu/DC/c.aspx. Use the email amandabuechele2029@u.northwestern.edu to locate the data extract titled  20260923clandata.
3. **Download** in the format *ASCII data with Stata statements*, and unzip it.
4. **Copy the `.do` file and the `.txt` data file** into `0_data/raw/`. File names do not matter, but the folder must contain exactly one `.do` file and one `.txt` file and nothing else.

Two more input files are already included in the repository:

- `0_data/psid.xlsx`: the PSID cross-year variable index, from the PSID website. It maps each variable to its name in every survey year. If you use a newer extract, replace it with the current version. (Click on the Excel link at https://simba.isr.umich.edu/DC/i.aspx)
- `0_data/cpi/cpi.xlsx`: the Consumer Price Index (U.S. Bureau of Labor Statistics, CPI-U, all items, U.S. city average, annual averages, series CUUR0000SA0). All dollar amounts are converted to 2023 dollars.

### Step 3: Run the pipeline

From the top folder of the repository:

```bash
make
```

`make` prints a line such as `==> Step 4: kin groups (clans)` as each step starts. If a step fails to create a file it should, `make` stops and names the missing file. Results go in each step's `output/` folder. The manuscript and supplement go in `10_draft/output/`.

The full pipeline should take about 5 minutes to run.

Every number quoted in the text is calculated when the manuscript renders, not typed by hand. The only output that will differ from the submitted version is the date on the title page. The one random step, the simulation in Appendix C2, uses a fixed seed (`set.seed(123)`).

Our extract was downloaded on 2026-09-23. The PSID sometimes re-releases files with corrections, so a later download may give slightly different numbers.

### What each step does

| Step | Script | What it does |
|---|---|---|
| 1 | `1_build_panel/src/build_panel.R` | Reads the raw PSID files into one person-by-year dataset |
| 2 | `2_clean_panel/src/clean_panel.R` | Cleans the data, restricts the sample, adjusts for inflation |
| 3 | `3_households/src/households.R` | Builds households |
| 4 | `4_clans/src/clans.R` | Links households into kin groups and builds the analysis samples |
| 5 | `5_summary/src/summary.R` | Calculates summary statistics |
| 6 | `6_calculate_gini/src/gini_all.R` | Calculates Gini coefficients by year |
| 7 | `7_gini_by_race/src/gini_by_race.R` | Calculates Gini coefficients and wealth ratios by race |
| 8 | `8_nuclear_family/src/calc_nuclear_family.R` | Calculates Nuclear-family (C1/C2/C3) coefficients |
| 9 | `9_figures/src/all_figures.R` | Generates all figures and tables |
| 10 | `10_draft/write.rmd`, `10_draft/supplemental.rmd` | Renders the manuscript and supplement (PDF and Word versions available) |

Step 6 runs before step 5, because the summary statistics use the Gini results. Helper functions shared by several steps are in `functions/`.

### Useful commands

| Command | What it does |
|---|---|
| `make` | Run everything |
| `make pipeline` | Steps 1–8 only |
| `make figures` | Step 9 only |
| `make paper` / `make supplemental` | Render the manuscript / supplement |

`make` only reruns steps whose code or inputs have changed. Do not use parallel make (`make -j`), because several steps write to the same folders.

### Where each result is produced

Figures and tables are saved in `9_figures/output/`. Main-text exhibits come from `9_figures/src/figure_main.R` and appendix exhibits from `9_figures/src/figure_appendix.R`.

| Exhibit | Output file |
|---|---|
| Figure 1. Income and wealth inequality over time | `figure1.pdf` |
| Figure 2. Lorenz curves at the household and kin group levels | `figure2.pdf` |
| Figure 3. Black / non-Black median wealth ratio | `figure3.pdf` |
| Table 1. Characteristics of households and kin groups | `table1.docx` |
| Appendix A. Comparison with external estimates | rendered directly in `supplemental.rmd` |
| Appendix B. Alternative inequality measures | `appendixB.pdf` |
| Appendix C1–C2. Simulated Gini coefficients | `appendixC1.pdf`, `appendixC2.pdf` |
| Appendix D1–D7. Sensitivity analyses: Gini coefficients | `appendixD1.pdf` to `appendixD7.pdf` |
| Appendix E1–E8. Sensitivity analyses: race ratios | `appendixE1.pdf` to `appendixE8.pdf` |
| Appendix F1. Characteristics in 1984 and 2023 | `appendix_tableF1.docx` |
| Appendix F2–F4. Size, age, and sample size over time | `appendixF2.pdf` to `appendixF4.pdf` |

---

## 4. Troubleshooting

| Problem | Fix |
|---|---|
| `there is no package called ...` | Run `make setup`. |
| `make setup` fails while installing a package | The package has to be compiled, and your computer is missing a compiler or library (see below). Install it, then run `make setup` again. It picks up where it stopped. |
| `make setup` says packages do not match `renv.lock` | In R, run `renv::status()` to see which packages differ, then `renv::restore()`. `make` still runs, but results may differ slightly from the tested setup. |
| A package still will not install | As a last resort, run `renv::install("packagename")` in R to install the current version. If you report a replication problem, mention which package you replaced. |
| `pandoc ... not found` | Install pandoc (Installation guide, step 3). |
| LaTeX error about a missing `.sty` file | Install TinyTeX (Installation guide, step 4). |
| `0_data/raw/ must hold exactly one .do and one .txt file` | Remove any other files from that folder. |

**Tools for compiling packages.** The error message usually names what is missing.

- **Windows:** [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for your version of R (Rtools44 for R 4.4).
- **macOS:** run `xcode-select --install`, and install gfortran from <https://mac.r-project.org/tools/>. If a library is missing, install it with Homebrew, e.g. `brew install harfbuzz fribidi`.

## 5. Additional information

### License

See LICENSE.txt

### Code repository

https://github.com/aebuechele29/clan_study

### Description of the code's functionality

The code's functionality is described in the **Methods section**.

### Data citation

Panel Study of Income Dynamics, public use dataset. Produced and distributed by the Survey Research Center, Institute for Social Research, University of Michigan, Ann Arbor, MI (2026).

---