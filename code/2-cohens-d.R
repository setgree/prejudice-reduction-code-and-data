#' ---
#' title: Cohen's D calculations
#' author: Seth Green
#' output:
#'  pdf_document
#' ---

rm(list =ls())
library(purrr)
library(dplyr)
library(tidyr)
library(haven)
library(textclean)
library(stringr)

dat <- readRDS(file = '../data/prejudice_meta_data.rds')

#' meta-analytic function:
#' adapted from 'The Contact Hypothesis Re-evaluated', Paluck et al. 2018
source(file = './functions/Results_standardizeR.R')

#' This line prevents duplicate calculations (and the inclusion of duplicate columns)
if('var_d' %in% colnames(dat)) dat <- dat %>% select(-d, -var_d, -st_err_d)
# safely_stand_result <- safely(stand_result) # JH: If we don't need this, let's take it out

dat_clean <- dat %>%
  mutate(test_statistic = as.numeric(test_statistic)) %>%
  nest(data = c(effect_type, test_statistic, sd_control, n_treatment,
                n_treatment_clusters, n_control, n_control_clusters)) %>% 
           mutate(standardized_d = 
           purrr::map_if(.x = data,
                  .p = (is.na(dat$n_treatment_clusters)),
                  .f =  ~stand_result(eff_type = .x$effect_type, 
                                      n_t = .x$n_treatment, 
                                      n_c = .x$n_control,
                                      ctrl_sd = .x$sd_control,
                                      raw_effect_size = as.numeric(
                                        as.character(.x$test_statistic))), 
                  .else = ~stand_result(eff_type = .x$effect_type,
                                        n_t = as.numeric(
                                          as.character(.x$n_treatment_clusters)),
                                        n_c = as.numeric(
                                          as.character(.x$n_control_clusters)),
                                                      ctrl_sd = .x$sd_control,
                                                      raw_effect_size = as.numeric(
                                                        as.character(
                                                          .x$test_statistic)))))  %>%
  unnest(data) %>%
  unnest(standardized_d) %>%
  select(d, var_d, st_err_d, effect_type, test_statistic, sd_control,
         n_treatment, n_treatment_clusters, 
         n_control, n_control_clusters,
         everything()) %>%
  mutate(d = abs(d) * effect_direction)

# test for if any variance of d is missing: 0 -> no missing variances
nrow(dat_clean %>% filter(is.na(var_d)))

#" Save files
saveRDS(object = dat_clean, file = '../data/prejudice_meta_data.rds')

# save for Stata users:
dat_clean %>%
  mutate_if(.predicate = is.character, .funs = replace_non_ascii) %>%
  mutate_if(.predicate = is.character, ~str_trunc(string = ., width =  30)) %>%
  write_dta('../data/prejudice_meta_data.dta', version = 15)

 sessionInfo()
