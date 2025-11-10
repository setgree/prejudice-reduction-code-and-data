add_parenthesis <- function(vector){
  out <- as.vector(paste0("(", vector, ")"))
  out
}

all_forest_template <- function(df_all, label_pos = 1.15){
  label_pos = label_pos
  
  df_all %>%
    ggplot(aes(beta, label, col = col)) +
    scale_color_manual(values = c("black", "red3")) +
    geom_point(size = 3.6) +
    geom_errorbar(aes(xmin = ci.lb, xmax = ci.ub), width = .3, size = 1.3) +
    geom_vline(xintercept = 0, linetype = 5) +
    labs(title = "Meta Analytic Results By Intervention Approach",
         subtitle = "95% Confidence Intervals. Full Sample",
         x  = "Meta Analytic Effect Size (Cohen's D)",
         y = "") +
    geom_text(aes(x = label_pos, label = beta),
              nudge_y = 0.12) +
    geom_text(aes(x = label_pos, label = ci_label),
              nudge_y = -0.12) +
    scale_x_continuous(breaks = c(0, .2, .5, .8, 1.4), expand = c(0, .1)) +
    theme(legend.position = "none", 
          axis.text.y = element_text(size = 12.5),
          axis.text.x = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank())
}

big_forest_template <- function(df_big, label_pos = 2){
  label_pos = label_pos
  
  df_big %>% 
    ggplot(aes(beta, label, col = col)) +
    scale_color_manual(values = c("black", "red3")) +
    geom_point(size = 3.6) +
    geom_errorbar(aes(xmin = ci.lb, xmax = ci.ub), width = .3, size = 1.3) +
    geom_vline(xintercept = 0, linetype = 5) +
    labs(title = "Meta Analytic Results By Intervention Approach",
         subtitle = expression("95% Confidence Intervals. "*N[t]*" ≥ 78"), #"95% Confidence Intervals. N_treatment > 78",
         x  = "Meta Analytic Effect Size (Cohen's D)",
         y = "") +
    geom_text(aes(x = label_pos, label = beta),
              nudge_y = 0.12) +
    geom_text(aes(x = label_pos, label = ci_label),
              nudge_y = -0.12) +
    scale_x_continuous(breaks = c(0, .2, .5, .8, 1.4), expand = c(0, .3)) +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 12.5),
          axis.text.x = element_text(size = 12),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank())
}

coef_label <- function(df){
  coef <- 
    broom::tidy(lm(d ~ st_err_d, data = df))[,2] %>% 
    mutate(estimate = round(estimate, 2)) %>%
    pull()
  
  paste0("ŷ = ", str_c(coef, collapse = " + "), "se")
}

two_digits <- function(data){
  out <- as.vector(trimws(format(round(data, 2), nsmall = 2)))
  out
}

prep_meta_data <- function(subfield_df, overall_df = overall, subfield_var){
  x <- enquo(subfield_var)
  
  subfield_df %>% 
    bind_rows(overall_df) %>% 
    mutate(!!x := fct_explicit_na(!!x, "Overall"),
           col = ifelse(!!x == "Overall", 1, 0),
           col = factor(col, nmax = 4),
           beta = round(beta, 3),
           label = fct_inorder(paste0(str_to_title(!!x), " (n = ", n_studies, ")")),
           label = fct_reorder(label, beta),
           ci_label = str_c("[", round(ci.lb, 2),"-",round(ci.ub, 2), "]"),
           order = row_number(beta)) 
}

save_forest <- function(name, width = 13, height = 8){
  ggsave(name, width = 13, height = 7)
}

  
se_label <- function(df){
  broom::tidy(lm(d ~ st_err_d, data = df))[,3] %>% 
    mutate(std.error = round(std.error, 2)) %>%
    pull() %>%
    add_parenthesis() %>% 
    str_c(., collapse = "  ")
}
