---
output:
  pdf_document: default
  html_document: default
---
# Prejudice Reduction: Progress and Challenges (Paluck, Porat, Clark and Green 2020)

This repository contains the code and data for reproducing the results in the accompanying paper.

## To reproduce all results:
From the `/code` directory, execute either `0-main.R` or, if you prefer, `bash run.sh`.

## Description of programs

### Analysis scripts

**`0-main.R`** - Runs the entire pipeline from the top (R scripts 1-6, in order) and creates all output.

**`1-clean-data.R`** - Cleans and transforms `raw_data.csv`  into `prejudice_meta_data.rds`.

**`2-cohens-d.R`** - Converts the unstandardized effects into Cohen's D, variance of D, and Standard Error of D, and appends estimates of each to the main dataset.

**`3-sub-group.R`** - Calculates all  meta-analytic effect sizes that are presented in the paper and saves them as `csv` files within (subfolders of) `data/meta-analytic/*`.

**`4-paper-stats.Rmd`** - Provides code and data that produce each quantitative claim in the paper. It builds on `3-sub-group-R`, so if the `csv` files created by that script haven't yet been created, this script will not run. 

**`5-figures.Rmd`** -  The creates all the figures used in the paper and some additional exploratory visualizations.

**`6-methods-check.Rmd`** - This script simulates a dataset and uses it to check the paper's procedures for estimating Cohen's D.

### Additional files in `code/`:
**`run.sh`** - A `bash` script that executes `0-main.R` (which runs all scripts in order). 

**`prejudice_reduction.Rproj`** - An `R project` file used to navigate directories for reproduciblity across machines.

## Functions and helper scripts in `functions/`:

**`dplot.R`** - A function for creating scatterplots of D by N_treatment (used in `5-figures.Rmd`).

**`factor-levels.R`** - Declares the names of the numeric variable categories so that they can be converted to factors.

**`forest_functions.R`** - Additional functions for the forest plots in `5-figures.Rmd`.

**`main-meta-function.R`** - Declares the function `meta_analyze`, which is used to calculate the meta-analytic effects in `3-sub-group.R`.

**`make_bib.R`** - Provides examples of how to use [RefManageR](https://github.com/ropensci/RefManageR/) to convert DOIs in a dataset into a complete bibliography. 

**`ResultsStandardizeR.R`** - Declares the function `stand_result`, used primarily in the script `2-cohens-d.R` to convert unstandardized effect sizes into Cohen's D.

**`write_dockerfile.R`** - Converts the output from `sessioninfo::package_info()` into a [Dockerfile for long-term reproducibility](https://arxiv.org/pdf/1410.0846.pdf). 

## Description of data

**`lai_data.rds`** - A dataset of estimates from Lai et al. (2014, 2016) discussed in a robustness check in the appendix.

**`meta-analytic/*`** - intermediate datasets created by `code/3-sub-group.R`. This folder will be empty until you run that script.

**`prejudice_meta_data.dta`** - This is the `dta` equivalent of `prejudice_meta_data.rds` written for analysis in Stata 15.

**`prejudice_meta_data.rds`** - The maind dataset used in the meta-analysis. `1-clean-data.R` transforms `raw-data.csv` into this, and then `2-cohens-d.R`appends Cohen's D, variance of D, and standard error of D to each row. A data dictionary of the variables can be found in `documentation/`.

**`raw-data.csv`** - The raw data, before cleaning or the addition of Cohen's D columns.

**`sim_data.rds`** - The simulated data for use in `6-methods-check.Rmd`. 

## Output

**`4-paper-stats.html, 5-figures.html, 6-methods-check.html`**  - Rendered HTML files, interspersing code, data and text, created from the R scripts with the corresponding titles in `code/`. All rendering commands are in `0-main.R`.

**`figs/`** - Contains figures saved from `5-figures.Rmd`. 

**`vtable.html`** - provides a statistical overview of the variables in  `prejudice_meta_data.rds`.

## Documentation

**`all-manuscripts-bibliography.bib`**, **`all-manuscripts-bibliography.pdf`** - A `.bib` file (and compiled PDF) of the 309 manuscripts in the main meta-analysis.

**`codebook.csv`** - a codebook for all variables in `raw-data.csv`. 

**`Dockerfile`** -- A Dockerfile (for long-term reproducibility) produced by `code/functions/write_dockerfile.R`.