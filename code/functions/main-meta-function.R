library(dplyr)
library(purrr)
library(metafor)
library(tidyr)
library(glue)

# Load beepr only if installed (used for audio notifications)
if (requireNamespace("beepr", quietly = TRUE)) {
  library(beepr)
}

# main meta function -----------------------------------

#' internal function for `meta_analyze()` (defined below)
#' here the data is nested into mini-datasets based on `group_vars`
#' data is extracted from mini-datas and then meta analysis is run
#' statistics are then extracted from meta-analysis object
meta_engine <- function(df, group_vars){
  df %>%   
    group_by(!!!group_vars) %>% # !!!group_vars ; group_vars = intervention_approach, intervention_type ; cross tabs to analyze
    nest() %>% # make everything into a dataset
    ungroup() %>% 
    mutate(n_effect_sizes = map_int(data, nrow),
           n_studies = map_int(data, ~dplyr::n_distinct(.x$unique_study_id)),
           n_articles = map_int(data, ~dplyr::n_distinct(.x$unique_paper_id))) %>% 
    filter(n_studies >= 2, # drop if fewer than 2 studies
           n_articles > 1) %>% # drop if only one article
    mutate(meta_main = map(data, ~rma.uni(yi = .x$d, vi = .x$var_d))) %>%
    mutate(robust_meta = map2(.x = meta_main,
                              .y = data,
                              .f = ~robust(x = .x, cluster = .y[["unique_paper_id"]])), # cluster on paper level
           beta = map_dbl(robust_meta, ~unclass(.x)$beta),
           se = map_dbl(robust_meta, ~unclass(.x)$se),
           t = map_dbl(robust_meta, ~unclass(.x)$zval),
           pval = map_dbl(robust_meta, ~unclass(.x)$pval),
           ci.lb = map_dbl(robust_meta, ~unclass(.x)$ci.lb),
           ci.ub = map_dbl(robust_meta, ~unclass(.x)$ci.ub)
    ) %>%
    dplyr::arrange(!!!group_vars)
}



#' main meta analysis function for paper
#' this functions serves (3) main purposes:
#' 1) Prepare the data into subgroups for `meta_engine` 
#' main data set can be filtered using options N, cluster, intervention type.
#' Effects that are multi-coded (e.g., multiple settings) are duplicated **once per respective setting**
#' This assures that when `meta_analyze()` nests the data into subgroups each effect is included in it's respective group
#' Data can be sub-divied into any subgroups so long at it corresponds with a variable in the dataset
#' Multi column vars should be referred to by parent name. e.g., setting1, setting2 ... should just be called "setting"
#' This allows for unlimited inputs via the ... (= group_vars) input, so long as there are enough papers and articles  to conduct the analysis
#' 
#' 2) Call `meta_engine` function where main analysis is produced
#' 
#' 3) Export the output into a .csv file with consistent naming conventions
#' File name will be a function of inputs
meta_analyze <- function(..., treatment_size_eqlgrtr_than = 0, 
                         treatment_size_less_than = Inf, drop_clusters = F, 
                         keep_data = F, add_lai_data = F, add_many_labs = F, 
                         lab_only = F, online_only = F, field_only = F, 
                         output_file = F){
  
  group_vars <- enquos(...)
  vars <- map_chr(group_vars, quo_name)
  
  if(add_lai_data){
    dat <- full_join(dat, readRDS(file = '../data/lai_data.rds')) %>%
      select(-n_treatment_high_low) %>% #redo this calculation just in case
      mutate(n_treatment_high_low = case_when(n_treatment <= 25 ~ "=< 25", # low quintile
                                              n_treatment >= 78 ~ ">= 78", # high quintile
                                              TRUE ~ "middle")); lai_label = "_lai_studies"}
  else(lai_label = "")
  if(lab_only) {dat <- filter(dat, intervention_type1 == "lab" | intervention_type2 == "lab"); it_label = "_lab_studies"; folder = "lab"}
  if(online_only) {dat <- filter(dat, intervention_type1 == "online" | intervention_type2 == "online"); it_label = "_online_studies"; folder = "online"}
  if(field_only) {dat <- filter(dat, intervention_type1 == "field" | intervention_type2 == "field"); it_label = "_field_studies"; folder = "field"}
  if(!lab_only & !online_only & !field_only) {folder = "overall"; it_label = ""}

  if(add_many_labs){dat <- add_row(dat, 
                                   d = .10, 
                                   var_d = 0.05^2, 
                                   n_treatment = 999999, 
                                   intervention_approach1 = "extended and imaginary contact",
                                   unique_paper_id = 99999, 
                                   unique_study_id = 99999); many_lab_label = "_many_lab"}
  else(many_lab_label = "")
  if(drop_clusters) {dat <- dat %>% filter(is.na(n_treatment_clusters)); cluster = "_drop_cluster"
  } else {cluster = ""}
  
  if (treatment_size_eqlgrtr_than > 0) {t_great = glue("_t>e{treatment_size_eqlgrtr_than}")
  } else {t_great = ""}
  if (treatment_size_less_than < Inf) {t_less = glue("_t<{treatment_size_less_than}")
  } else {t_less = ""}
  
  if(any(vars %in% "intervention_approach")){
    dat <- dat %>%
      pivot_longer(cols = intervention_approach1:intervention_approach2, # double count
                   names_repair = "unique",
                   values_to = "intervention_approach",
                   values_drop_na = T)
  }
  
  if(any(vars %in% "setting")){
    dat <- dat %>% 
      pivot_longer(cols = setting1:setting4,
                   names_repair = "unique",
                   values_to = "setting", 
                   values_drop_na = T) 
  }
  
  if(any(vars %in% "intervention_type")){
    dat <- dat %>% 
      pivot_longer(cols = intervention_type1:intervention_type2,
                   names_repair = "unique",
                   values_to = "intervention_type", 
                   values_drop_na = T) 
  }
  
  if(any(vars %in% "prejudice_type")){
    dat <- dat %>% 
      pivot_longer(cols = prejudice_type1:prejudice_type5, 
                   names_repair = "unique",
                   values_to = "prejudice_type", 
                   values_drop_na = T) 
  }
  
  if(any(vars %in% "outcome_type")){
    dat <- dat %>% 
      pivot_longer(cols = outcome_type1:outcome_type2, 
                   names_repair = "unique",
                   values_to = "outcome_type", 
                   values_drop_na = T)
  }
  
  
  df_collapse <- 
    dat %>%
    filter(n_treatment >= treatment_size_eqlgrtr_than) %>%  # big studies only
    filter(n_treatment < treatment_size_less_than) %>%  # small studies only
    group_by(unique_paper_id, unique_study_id, !!!group_vars) %>% # collapse by **unique_study_id** and relevant vars
    dplyr::summarise(d = mean(d),
              var_d = mean(var_d),
              n_treatment = mean(n_treatment),
              st_err_d = mean(st_err_d),
              .groups = "keep") %>% 
    ungroup()
  
  output <- meta_engine(df_collapse, group_vars)
  
  if(!keep_data) output <- output %>% select(-meta_main, -data, -robust_meta)
  if(lab_only | online_only | field_only) output <- mutate(output, intervention_setting = str_sub(str_c(it_label, "_only"), 2))
  filename <- vars %>% paste(collapse = "_x_")
  if(length(group_vars) == 0) filename = "overall_meta"
  out_filename <- "../data/meta-analytic/{folder}/{filename}{many_lab_label}{lai_label}{t_great}{t_less}{cluster}{it_label}.csv"
  
  print(glue(out_filename))
  if(output_file) write_csv(output, path = glue(out_filename))

  # Play sound notification if beepr is installed
  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(sound = 2)
  }
  
  return(output)
}
