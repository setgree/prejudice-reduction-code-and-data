#!/usr/bin/Rscript --vanilla

# Loading libraries
# note: if you go through the scripts one by one, Rstudio should 
# tell you which packages you need as they come up and help you install them.
# If you wish to install them all at once, run the following code (you may need 
# to install `pacman` first)
# library(pacman)
# pacman::p_load('beepr', 'dplyr', 'forcats', 'ggplot2', 'ggpubr', 'ggrepel',
#                'ggthemes', 'glue', 'haven', 'here', 'janitor', 'kableExtra',
#                'knitr', 'lubridate', 'Matrix', 'metafor', 'openxlsx', 'purrr',
#                'readr', 'readxl', 'rmarkdown', 'scales', 'sessioninfo',
#                'stringr', 'textclean', 'tidyr', 'vtable')

library(here)
library(rmarkdown)
library(sessioninfo)

# prelim setup

# set directories using here package for robust path management
knitr::opts_knit$set(root.dir = here::here())

# check if paths exist and make them if are not found
dir.create(here("output", "figs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "meta-analytic", "field"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "meta-analytic", "lab"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "meta-analytic", "online"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "meta-analytic", "overall"), recursive = TRUE, showWarnings = FALSE)

# Step 1: clean
source(here("code", "1-clean-data.R"))

# step 2: cleaning for Cohen's D and Standard Error calcs
source(here("code", "2-cohens-d.R"))

# Step 3: subgroup analysis
source(here("code", "3-sub-group.R"))

# Step 4: paper stats
rmarkdown::render(input = here("code", "4-paper-stats.Rmd"),
                  output_dir = here("output"), clean = TRUE)

# Step 5: all plots
rmarkdown::render(input = here("code", "5-figures.Rmd"),
                  output_dir = here("output"), clean = TRUE)

# Step 6: methods check
rmarkdown::render(input = here("code", "6-methods-check.Rmd"),
                  output_dir = here("output"), clean = TRUE)

# Wrapping up: for long term reproducibility, convert package info -> Dockerfile
source(here("code", "functions", "write_dockerfile.R"))
write_dockerfile(dir = here("documentation"))

