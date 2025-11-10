library(broom)
library(dplyr)
library(ggplot2)
library(stringr)

dplot  <- function(setting = 'all', prej_data = dat, dot_size = 1,
                   dot_color = T, title = T){
  
  # subset if setting is set to lab, field, or online (default is 'all')
  if(setting %in% c('field', 'lab', 'online')){
    prej_data <- prej_data %>% 
      filter(intervention_type1 == setting | intervention_type2 == setting)}
  
  # condense down to one estimate per study
  prej_data <- prej_data %>%
    group_by(unique_study_id) %>% 
    mutate(d = mean(d),
           n_treatment = mean(n_treatment)) %>% 
    ungroup() %>% distinct(unique_study_id, .keep_all = T)
  
  # helper functions for statistical summaries
  add_parenthesis <- function(vector){
    out <- as.vector(paste0("(", vector, ")")); out
  }
  coef_label_n <- function(df){
    coef <- round(tidy(lm(d ~ log(n_treatment), data = df))[,2], digits = 2) %>% 
      pull()
    paste0("ŷ = ", str_c(coef, collapse = " + "), "se")
  }
  se_label_n <- function(df){
    round(tidy(lm(d ~ log(n_treatment), data = df))[,3], digits = 2) %>% 
      pull() %>% add_parenthesis() %>% paste(., collapse = "  ")
  }
  
  # compute statistical summaries and combine into annotation dataset
  dcor <- paste("R = ", round(cor(prej_data$d, log(prej_data$n_treatment)),
                              digits = 2))
  coef <- coef_label_n(prej_data)
  se_labs <- se_label_n(prej_data)
  rows <- paste("n = ", nrow(prej_data))
  xrng <- range(log(prej_data$n_treatment))[1]
  yrng <- range(prej_data$d)[2]
  annotations_df <- data.frame(x = xrng, y = yrng,
                               labels = paste(dcor, '\n', coef, '\n', 
                                              se_labs, '\n', rows))
  # plot
  dplot <- prej_data %>%
    ggplot2::ggplot(data = prej_data,
                    mapping = aes(y = d, x = log(n_treatment))) + 
    {if(dot_color) geom_point(aes(color = prejudice_type1), size = dot_size)} + #size = 3, 
    {if(dot_color == F) geom_point(aes(colour = 'black'), size = 1)} + # size = 3, 
    geom_smooth(method = 'lm') +
    xlab("log(N of treatment)") + 
    ylab("Effect Sizes") +
    guides(size = F) +
    {if(isTRUE(title)) ggtitle(paste0("Effect sizes by log(N treatment) for ",
                                      setting, " experiments"))} +
    {if(dot_color) labs(colour = "Prejudice Type")} +
    theme(plot.title =  element_text(hjust = 0.5))
  
  # add annotations
  dplot + geom_text(data = annotations_df, 
                    mapping = aes(x = x, y = y, label = labels), 
                    hjust = 'inward', vjust = 'inward')
}
