#' ---
#' title: Subgroup Analyses
#' author: John-Henry Pezzuto
#' output:
#'  html_document
#' ---

rm(list=ls())

# Load beepr only if installed (used for audio notifications)
if (requireNamespace("beepr", quietly = TRUE)) {
  library(beepr)
}
library(dplyr)
library(glue)
library(janitor)
library(kableExtra)
library(metafor)
library(parallel)
library(purrr)
library(readr)
library(scales)
library(stringr)
library(tidyr)

dat <- readRDS('../data/prejudice_meta_data.rds')

dat <- dat |> 
  mutate(n_treatment_high_low = case_when(n_treatment <= 25 ~ "=< 25", # low quintile
                              n_treatment >= 78 ~ ">= 78", # high quintile
                              TRUE ~ "middle")) 

source("./functions/factor-levels.R")
source("./functions/main-meta-function.R")

options(Ncpus = parallel::detectCores())


### files for 4-paper-stats.Rmd
### meta analysis files output -----------------------------------
#### intervention approach -----------------------------------
meta_analyze(output_file = T, drop_clusters = F) # overall 
meta_analyze(intervention_approach, drop_clusters = F, output_file = T) # iv (intervention approach)
meta_analyze(intervention_approach, prejudice_type, output_file = T, drop_clusters = F) # iv_x_pt
meta_analyze(intervention_approach, outcome_type, output_file = T, drop_clusters = F) # iv_x_o
meta_analyze(intervention_approach, time_measurement, drop_clusters = F, output_file = T) # iv_x_tm
meta_analyze(intervention_approach, setting, drop_clusters = F, output_file = T)# iv_x_s
meta_analyze(intervention_approach, intervention_type, output_file = T, drop_clusters = F) # iv_x_it
meta_analyze(intervention_approach, treatment_size_eqlgrtr_than = 78, drop_clusters = T, output_file = T) # iv_large

meta_analyze(time_measurement, n_treatment_high_low, drop_clusters = F, output_file = T) # greenwald question

#### outcome -----------------------------------
meta_analyze(outcome_type, output_file = T, drop_clusters = F) # outcome

#### light touch -----------------------------------
meta_analyze(light_touch, output_file = T, drop_clusters = F) # lt
meta_analyze(light_touch, time_measurement, output_file = T, drop_clusters = F) # lt_x_tm

##### treatment category -----------------------------------
meta_analyze(n_treatment_category, output_file = T, drop_clusters = T) # ntc
meta_analyze(n_treatment_category, light_touch, output_file = T, drop_clusters = T) # ntc_x_lt
meta_analyze(n_treatment_category, lab_only = T, output_file = T, drop_clusters = T) # ntcl
meta_analyze(n_treatment_category, online_only = T, output_file = T, drop_clusters = T) # ntco
meta_analyze(n_treatment_category, outcome_type, output_file = T, drop_clusters = T) # ntc_x_o


#### many labs addition -----------------------------------
meta_analyze(intervention_approach, treatment_size_eqlgrtr_than = 78, add_many_labs = T, drop_clusters = T, output_file = T) # ml_large
meta_analyze(intervention_approach, add_many_labs = T, drop_clusters = F, output_file = T) # ml

### lai papers addition: 
# first, overall and without many labs
meta_analyze(add_many_labs = F, add_lai_data =T, drop_clusters = F, output_file = T)
# separated by outcome type
meta_analyze(outcome_type, add_many_labs = F, add_lai_data =T,
             drop_clusters = F, output_file = T)
# now with both many labs and the lai papers: overall
meta_analyze(add_many_labs = T, add_lai_data =T, drop_clusters = F, output_file = T)
# both groups, by outcome type
meta_analyze(outcome_type, add_many_labs = T, add_lai_data =T,
             drop_clusters = F, output_file = T)
# sesssion info for reproducibility
sessionInfo()
