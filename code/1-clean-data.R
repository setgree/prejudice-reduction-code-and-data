#' ---
#' title: Cleaning Prejudice Data
#' author: John-Henry Pezzuto & Seth Green
#' output:
#'  pdf_document
#' ---

rm(list = ls())
library(dplyr)
library(forcats)
library(glue)
library(lubridate)
library(janitor)
library(openxlsx)
library(readr)
library(readxl)
library(stringr)
library(textclean)
library(tidyr)
library(vtable)


## load raw data
dat <- read_csv(file = '../data/raw-data.csv')

## drop the unnamed first column (row numbers from original export)
dat <- dat %>% select(-`...1`)

## put general information columns towards the end
dat <- dat %>% select(-publication_type, -publication_title,
                      -abstract, -doi, -url,
                      everything())

## n manuscripts coded
# NOTE: the terms paper, article, and manuscript are used interchangeably 
# in these files and in Paluck et al. 2020

dat %>% 
  summarise(n_papers = n_distinct(unique_paper_id))

#' # Cleaning ------------------------------------------------------------------
dat <- readr::type_convert(dat)

#' # fix variable types ------------------------------------------
dat$year <- gsub(pattern = "2017', '2016", replacement = 2017.0, x = dat$year)
dat$year <- gsub(pattern = "2015', '2014", replacement = 2015.0, x = dat$year)
dat$year <- gsub(pattern = "2016', '2015", replacement = 2016.0, x = dat$year)

#' one setting change 
dat$setting <- gsub("8\\(year of service\\)" , '', dat$setting)

dat$year <- as.integer(dat$year) 

#' # Country:
dat$country <- gsub('-', NA, dat$country)
dat$country <- gsub(',', ' &', dat$country)
dat$country <- gsub('/', ' &', dat$country)
dat$country <- gsub('/', ' &', dat$country)
dat$country <- gsub('australia', 'Australia', dat$country)
dat$country <- gsub('austalia', 'Australia', dat$country)
dat$country <- gsub('austria', 'Austria', dat$country)
dat$country <- gsub('canada', 'Canada', dat$country)
dat$country <- gsub('china', 'China', dat$country)
dat$country <- gsub('cypress', 'Cyprus', dat$country)
dat$country <- gsub('england', 'England', dat$country)
dat$country <- gsub('germany', 'Germany', dat$country)
dat$country <- gsub('israel', 'Israel', dat$country)
dat$country <- gsub('usa', 'USA', dat$country)
dat$country <- gsub('^US$', 'USA', dat$country)
dat$country <- gsub('^us$', 'USA', dat$country)
dat$country <- gsub('^us $', 'USA', dat$country)
dat$country <- gsub('^Us$', 'USA', dat$country)
dat$country <- gsub('greece', 'Greece', dat$country)
dat$country <- gsub('south africa', 'South Africa', dat$country)
dat$country <- gsub('nigeria', 'Nigeria', dat$country)
dat$country <- gsub('rwanda', 'Rwanda', dat$country)
dat$country <- gsub('sweden', 'Sweden', dat$country)
dat$country <- gsub('^uk$', 'UK', dat$country)
dat$country <- gsub('italy', 'Italy', dat$country)
dat$country <- gsub('turkey', 'Turkey', dat$country)
dat$country <- gsub('spain', 'Spain', dat$country)
dat$country <- gsub('hungary', 'Hungary', dat$country)
dat$country <- gsub('us  & hong kong', 'USA & Hong Kong', 
                    dat$country)
dat$country <- gsub('hong kong', 'Hong Kong', dat$country)
dat$country <- gsub('poland', 'Poland', dat$country)
dat$country <- gsub('palestine', 'Palestine', dat$country)
dat$country <- gsub('indonesia', 'Indonesia', dat$country)
dat$country <- gsub('portugal', 'Portugal', dat$country)
unique(dat$country)

#' authors:
#' # remove non-ascii characters from author rows, and redo a few names
dat$author <- gsub(pattern = "', '",replacement = ', ', x = dat$author)
dat$author <- gsub(pattern = '\\["|"\\]',replacement = '', x = dat$author)
dat$author <- gsub('Boyle M.P., Dioguardi L., Pate J.E.',
                   replacement = 'Boyle, M.P. and Dioguardi, L. and Pate, J.E.',
                   x = dat$author)

dat$author <- gsub(pattern = 'McWaters S.C., Hawkins R.', 
                   replacement = 'McWaters, S.C. and Hawkins, R',
                   x = dat$author)

dat$author <- gsub(pattern = 'J. Gu, A. Mueller, I. Nielsen, J. Shachat and R. Smyth', 
                   replacement = 'Gu, J. and Mueller, A. and Nielsen, J. and Schachat and Smyth, R.', 
                   x = dat$author)


#' create second_country and third_country variables for multi-country studies
dat <- dat %>%
  separate(col = 'country', into = c('country1', 'country2', 'country3'), 
           sep = ' & ', remove = TRUE)
# note -doing sep = ' & ' with spaces because each string has spaces

#' # type of prejudice ----------------------------------------------------
#'  space things out by prejudice type, first, second, third, etc.
dat <- dat %>%
  separate(col = prejudice_type,
           into = c('prejudice_type1', 'prejudice_type2', 
                    'prejudice_type3', 'prejudice_type4', 
                    'prejudice_type5'),
           sep = ',', remove = TRUE)

#' # outcome_type ----------------------------------------------------
dat$outcome_type <- gsub(pattern = ', ', ',', dat$outcome_type)
dat$outcome_type <- gsub(pattern = '\n', '', dat$outcome_type)

#' # separate out outcome type into type 1 and type 2 ----------------
dat <- dat %>%
  mutate(mediator = str_detect(outcome_type, "9"),
         outcome_type = str_remove_all(outcome_type, "9")) %>%
  separate(col = outcome_type, into = c('outcome_type1', 'outcome_type2'),
           sep = ',', remove = TRUE) %>%
  mutate(outcome_type1 = parse_number(outcome_type1),
         outcome_type2 = parse_number(outcome_type2))

#' # intervention_approach ----------------------------------------------
#' convert the words 'PEER TRAINER', 'FRIENDS', and PEERS' to category 3

unique(dat$intervention_approach)
dat$intervention_approach <- gsub('FRIENDS', '3', 
                                  dat$intervention_approach)
dat$intervention_approach <- gsub('PEER TRAINER', '3',
                                  dat$intervention_approach)
dat$intervention_approach <- gsub('PEERS', '3',
                                  dat$intervention_approach)
unique(dat$intervention_approach) 

#' remove any commas that will break the sep() pattern

dat$intervention_approach <- gsub('confrontation about prejudice,', 
                                  'confrontation about prejudice:', 
                                  dat$intervention_approach)
dat$intervention_approach <- gsub('pure information,', 
                                  'pure information:',
                                  dat$intervention_approach)
dat$intervention_approach <- gsub('14\\(', '14 \\(', 
                                  dat$intervention_approach)
dat$intervention_approach <- gsub(', ', ',',
                                  dat$intervention_approach)
dat$intervention_approach <- gsub(' and ', ',',
                                  dat$intervention_approach)

#' separate intro approach 1/2 for multi-approach studies:
dat <- dat %>%
  separate(col = intervention_approach, 
           into = c('intervention_approach1', 
                    'intervention_approach2'),
           sep = ',', remove = TRUE)

#' # Effect Types ------------------------------------------

dat$effect_type <- gsub('"did"', 'd_i_d', dat$effect_type)
dat$effect_type <- gsub('t.test\n', 't_test', dat$effect_type)
dat$effect_type <- gsub('partial eta-squared\n', 'partial_eta_squared',
                        dat$effect_type)
dat$effect_type <- gsub('partial-eta-squared', 'partial_eta_squared',
                        dat$effect_type)
dat$effect_type <- gsub('"t.test"', 't_test', dat$effect_type)
dat$effect_type <- gsub('\"dim\"', 'd_i_m', dat$effect_type)
dat$effect_type <- gsub('t.est', 't_test', dat$effect_type)
dat$effect_type <- gsub('f.test', 'f_test', dat$effect_type)
dat$effect_type <- gsub('dim', 'd_i_m', dat$effect_type)
dat$effect_type <- gsub('did', 'd_i_d', dat$effect_type)
dat$effect_type <- gsub('t.test', 't_test', dat$effect_type)
dat$effect_type <- gsub('reg.coef', 'reg_coef', dat$effect_type)
dat$effect_type <- gsub('partial eta-squared', 'partial_eta_squared', 
                        dat$effect_type)
dat$effect_type <- gsub('eta-squared', 'eta_squared', dat$effect_type)
dat$effect_type <- gsub('odds.ratio', 'odds_ratio', dat$effect_type)
dat$effect_type <- gsub('log.odds_ratio', 'log_odds_ratio', dat$effect_type)
dat$effect_type <- gsub('\n', '', dat$effect_type)

#' # intervention span --------------------------------------------------------
dat$intervention_span <- gsub(pattern = '1 Day', '1 day', 
                              dat$intervention_span)
dat$intervention_span <- gsub(pattern = '14 week\\>', '14 weeks', 
                              dat$intervention_span)
dat$intervention_span <- gsub(pattern = '\\<3 day\\>', '3 days', 
                              dat$intervention_span)
dat$intervention_span <- sub(pattern = '^1$', '1 day'
                             ,dat$intervention_span)
table(dat$intervention_span) # looks good

#' ### convert everything to days (rounding up to nearest day)
dat <- dat %>%
  mutate(intervention_span_days = 
           round(as.integer(lubridate::as.duration(intervention_span))/ 86400))
table(dat$intervention_span_days)

#' ### add unique_study_id variable -------------------------------------------
dat <- dat %>%
  mutate(unique_study_id = group_indices(., unique_paper_id, study_num))

#' Add delay category variable
class(dat$delay)
dat <- dat %>%
  mutate(delay = str_replace_all(delay, "0|0.0|-", "1 second"),
         delay = lubridate::as.duration(delay),
         delay = round(as.numeric(delay, "days"), 0),
         delay_category = case_when(delay < 1 ~ "Less than one day",
                                    delay >= 1 & delay <= 7 ~ "One day to one week",
                                    delay > 7 & delay <= 31 ~ "One week to one month",
                                    delay > 31 ~ "Greater than one month"))

#' Create factors (will be lost in csv format)
source("./functions/factor-levels.R") 

#' Apply factor levls to dataset
dat <- dat %>% 
  separate(setting, into = c("setting1", "setting2", "setting3", "setting4")) %>% 
  separate(intervention_type, into = c("intervention_type1", "intervention_type2")) %>% 
  mutate(intervention_approach1 = as.character(parse_number(intervention_approach1)), # intervention approach
         intervention_approach1 = factor(intervention_approach1, 
                                         levels = names(intervention_approach_levels),
                                         labels = intervention_approach_levels),
         intervention_approach1 = fct_collapse(intervention_approach1, # for combining categories
                                               `Diversity Trainings` =
                                                 c("sensitivity, cultural competence for health and law",
                                                   "diversity training")),
         intervention_approach2 = as.character(parse_number(intervention_approach2)), # intervention approach
         intervention_approach2 = factor(intervention_approach2, 
                                         levels = names(intervention_approach_levels),
                                         labels = intervention_approach_levels),
         intervention_approach2 = fct_collapse(intervention_approach2, # for combining categories
                                               `Diversity Trainings` =
                                                 c("sensitivity, cultural competence for health and law",
                                                   "diversity training"))) %>% 
  mutate(time_measurement = factor(time_measurement,  # time measurement
                                   levels = names(time_measurement_levels),
                                   labels = time_measurement_levels),
         outcome_type1 = factor(outcome_type1, # outcome type
                                levels = names(outcome_type_levels),
                                labels = outcome_type_levels),
         outcome_type2 = factor(outcome_type2, # outcome type
                                levels = names(outcome_type_levels),
                                labels = outcome_type_levels),
         light_touch = factor(light_touch,  # light touch
                              levels = names(light_touch_levels),
                              labels = light_touch_levels),
         prejudice_type1 = as.character(parse_number(prejudice_type1)), # prejudice type
         prejudice_type1 = factor(prejudice_type1, 
                                  levels = names(prejudice_type_levels), 
                                  labels = prejudice_type_levels),
         prejudice_type2 = as.character(parse_number(prejudice_type2)), # prejudice type
         prejudice_type2 = factor(prejudice_type2, 
                                  levels = names(prejudice_type_levels), 
                                  labels = prejudice_type_levels),
         prejudice_type3 = as.character(parse_number(prejudice_type3)), # prejudice type
         prejudice_type3 = factor(prejudice_type3, 
                                  levels = names(prejudice_type_levels), 
                                  labels = prejudice_type_levels),
         prejudice_type4 = as.character(parse_number(prejudice_type4)), # prejudice type
         prejudice_type4 = factor(prejudice_type4, 
                                  levels = names(prejudice_type_levels), 
                                  labels = prejudice_type_levels),
         prejudice_type5 = as.character(parse_number(prejudice_type5)), # prejudice type
         prejudice_type5 = factor(prejudice_type5, 
                                  levels = names(prejudice_type_levels), 
                                  labels = prejudice_type_levels)) %>% 
  mutate(setting1 = as.character(parse_number(setting1)), # setting
         setting1 = factor(setting1, 
                           levels = names(setting_levels),
                           labels = setting_levels),
         setting2 = as.character(parse_number(setting2)), # setting
         setting2 = factor(setting2, 
                           levels = names(setting_levels),
                           labels = setting_levels),
         setting3 = as.character(parse_number(setting3)), # setting
         setting3 = factor(setting3, 
                           levels = names(setting_levels),
                           labels = setting_levels),
         setting4 = as.character(parse_number(setting4)), # setting
         setting4 = factor(setting4, 
                           levels = names(setting_levels),
                           labels = setting_levels),
         intervention_type1 = as.character(parse_number(intervention_type1)), # intervention type labelling
         intervention_type1 = factor(intervention_type1, 
                                     levels = names(intevention_type_levels),
                                     labels = intevention_type_levels),
         intervention_type2 = as.character(parse_number(intervention_type2)), # intervention type labelling
         intervention_type2 = factor(intervention_type2, 
                                     levels = names(intevention_type_levels),
                                     labels = intevention_type_levels))

# add quintiles -----------------------------------
quintile_n <- 
  dat %>% 
  transmute(author,
            unique_study_id, 
            time_measurement,
            n_treatment_clusters, # n_treatment clusters needs to remain character to join again
            n_treatment = round(n_treatment)) %>% 
  filter(author != "Butler D.M., Crabtree C.", # drop outlier size study
         is.na(as.numeric(n_treatment_clusters)),  # drop effects with clusters; should cause warning we are searching NA's intentionally
         time_measurement == "same day") %>% # drop effects with delayed outcome
  group_by(unique_study_id, time_measurement, n_treatment_clusters) %>% 
  summarise(n_treatment = mean(n_treatment), .groups = "keep") %>% # collapse sample sizes by study
  ungroup()



### proof of concept quintile -----------------------------------
cutoff_points <- round_half_up(quantile(x = quintile_n$n_treatment, 
                                        probs = seq(0, 1, length.out = 6)), 
                               0) # cut-off points at integer

quintile_groups <- # place into groups based on cutoff_points
cut(quintile_n$n_treatment, 
    include.lowest = T,
    breaks = cutoff_points
    )

### next make the labels more read-able
# str = quintile_groups
reformat_labels <- function(str){
  
  clean_number <- function(str){
    x <- parse_number(str)
    as.integer(sprintf("%f", x)) # get rid of scientific notation
  }
  
  ld <- str_extract(str, "\\d+") # left digit
  ld <- as.character(ifelse(str_detect(str, "\\["), parse_number(ld), parse_number(ld) + 1)) # change number based on boundary
  rd <- str_extract(str, ",\\d.*") # right digit
  rd <- str_c(ld, " - ", ifelse(str_detect(str, "\\]"), clean_number(rd), clean_number(rd) + 1)) # change number based on boundary
  
  return(rd)
} 
reformat_labels(quintile_groups) # we get nice boundaries

quintile_categories <- 
quintile_n %>% 
  mutate(n_treatment_category = 
           cut(n_treatment, 
               include.lowest = T,
               breaks = cutoff_points
           )
  ) %>%
  mutate(n_treatment_category = reformat_labels(n_treatment_category)) %>% 
  mutate(n_treatment_category = factor(n_treatment_category)) %>% 
  mutate(n_treatment_category = case_when(as.numeric(n_treatment_category) == 1 ~ str_c("≤", str_extract(n_treatment_category, " \\d+")), # less than
                                          as.numeric(n_treatment_category) == max(as.numeric(n_treatment_category)) ~ str_c("≥ ", str_extract(n_treatment_category, "\\d+ ")), # greater than
                                          TRUE ~ as.character(n_treatment_category))) # don't modify rest



q_labels <- # create vector of labels
  quintile_categories %>%
  distinct(n_treatment_category) %>% 
  arrange(n_treatment_category) %>% 
  mutate(n_treatment_category = factor(n_treatment_category),
         n_treatment_category = relevel(n_treatment_category, 2),
         n_treatment_category = fct_shift(n_treatment_category)) %>% 
  arrange(desc(n_treatment_category)) %>% 
  pull()

dat <- # add to dataset based on study_id, time_measurement, and cluster
  left_join(dat, select(quintile_categories, -n_treatment)) %>% 
  mutate(n_treatment_category = factor(n_treatment_category, 
                                       levels = levels(q_labels)))

dat %>% # correctly categorized -- everything in n_treatment should fall within categories in n_treatment_category
  select(n_treatment, n_treatment_clusters, n_treatment_category) %>% 
  head(10)

# correctly specify nulls as 'unspecified nulls'
dat$effect_type <- gsub('null', 'unspecified null', dat$effect_type) # 0 means null
dat$effect_type <- gsub('-', 'unspecified null', dat$effect_type) # what's - 0 means null
dat$effect_type <- gsub('partial_eta_squared', 'eta_squared', dat$effect_type)

#' convert SD to numeric
dat$sd_control <- as.numeric(as.character(dat$sd_control))
#' note: NAs induced by conversion represent missing data or 
#' test statistics for which the SD is superfluous in the Cohen's D calculator

#' convert clusters to numeric
dat <- dat %>% 
  mutate(n_control_clusters = as.numeric(n_control_clusters), 
         n_treatment_clusters = as.numeric(n_treatment_clusters))

#' convert test statistic to 0.001 where it's currently zero
#'to distinguish it from "unspecified null" , which we're calling 0
dat$test_statistic <- gsub('^-$', 0.001, dat$test_statistic)

#' # Preparing to save ----------------------------------------------
#' ## remove non-ascii
vtable(dat, file = '../output/vtable.html', out = 'kable') 

# save as rds file
write_rds(dat, '../data/prejudice_meta_data.rds')

sessionInfo()


