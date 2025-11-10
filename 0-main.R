#!/usr/bin/Rscript --vanilla

# Loading libraries
# note: if you go through the scripts one by one, Rstudio should 
# tell you which packages you need as they come up and help you install them.
# If you wish to install them all at once, run the following code (you may need 
# to install `pacman` first)
# library(pacman)
# pacman::p_load('beepr', 'dplyr', 'forcats', 'ggplot2', 'ggpubr', 'ggrepel', 
#                'ggthemes', 'glue', 'haven', 'janitor', 'kableExtra', 'knitr',
#                'lubridate', 'Matrix', 'metafor', 'openxlsx', 'purrr', 'readr',
#                'readxl', 'rmarkdown', 'rprojroot', 'scales', 'sessioninfo',
#                'stringr', 'textclean', 'tidyr', 'vtable')

library(rmarkdown)
library(rprojroot)
library(sessioninfo)

# prelim setup  

# set directories and knitted output
wd <- rprojroot::find_rstudio_root_file()
knitr::opts_knit$set(root.dir = wd)
setwd(wd)

# check if paths exist and make them if are not found
system('mkdir -p output/figs')
system('mkdir -p data/meta-analytic/field')
system('mkdir -p data/meta-analytic/lab')
system('mkdir -p data/meta-analytic/online')
system('mkdir -p data/meta-analytic/overall')

# Step 1: clean
source('./1-clean-data.R')

# step 2: cleaning for Cohen's D and Standard Error calcs
source('./2-cohens-d.R')

# Step 3: subgroup analysis
source('./3-sub-group.R')

# Step 4: paper stats
rmarkdown::render(input = './4-paper-stats.Rmd',
                  output_dir = 'output', clean = T)

# Step 5: all plots
rmarkdown::render(input = './5-figures.Rmd',
                  output_dir = 'output', clean = T)

# Step 6: methods check
rmarkdown::render(input = './6-methods-check.Rmd',
                  output_dir =  'output', clean = T)

# Wrapping up: for long term reproducibility, convert package info -> Dockerfile
source('./functions/write_dockerfile.R')
write_dockerfile(dir = 'documentation/')

