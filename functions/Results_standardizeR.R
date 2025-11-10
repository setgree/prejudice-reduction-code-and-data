stand_result <- 
  function(eff_type, raw_effect_size, ctrl_sd, n_t, n_c){
    ## calculations generally taken from Cooper, Hedges, and Valentine (2009)
    
    # difference in differences
    if (eff_type == 'd_i_d'){ 
      d <- round(raw_effect_size / ctrl_sd, digits = 3)
    }
    
    # difference in means
    else if (eff_type == 'd_i_m'){
      d <- round(raw_effect_size / ctrl_sd, digits = 3)
    }
    
    # reporting of change of SDs in text:
    else if (eff_type == 'd'){
      d <- raw_effect_size
    }
    
    # unspecified null
    else if (eff_type == 'unspecified null'){
      d <- 0
    }
    
    # eta squared
    else if (eff_type == 'eta_squared'){
      d <- round((2 * sqrt(raw_effect_size) / sqrt(1 - raw_effect_size)),
                 digits = 3)
      }
  
    # regression coefficient
  else if (eff_type == 'reg_coef'){
    d <- round(raw_effect_size / ctrl_sd, digits = 3)
    }
  
    # t test
    else if (eff_type == 't_test'){
      d <- round(raw_effect_size * sqrt( (n_t + n_c ) / (n_t * n_c)), 
                 digits = 3)
    }
    
    # f test
  else if (eff_type == 'f_test'){
    d <- round(sqrt( ( raw_effect_size * (n_t + n_c)) / (n_t * n_c)), 
               digits = 3)
    }
    
    # difference in proportions
    # Don Green provided  calculations for SD based on control group proportion
    else if (eff_type == 'd_i_p'){
    d <- round(raw_effect_size / ctrl_sd, digits = 3)
    }
    
    # odds ratio
    # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
    # # Also in Cooper Hedges & Valentine p. 233
    else if (eff_type == 'odds_ratio'){
      d <- log(raw_effect_size) * sqrt(3) / pi
      }
  
    # log odds ratio
    else if (eff_type == 'log_odds_ratio'){
      d <- raw_effect_size * sqrt(3) / pi 
      }
    # compute variance of the estimated effect size

    ust_var_d <- (((n_t + n_c)
                 / (n_t * n_c))
                +
                  ((d^2) / (2 * (n_t + n_c))))

    # Apply hedge's g correction
  hedge_g <- 1 - (3 / (4*(n_t + n_c -2 ) -1))
  
  var_d <- round((hedge_g^2) * ust_var_d, digits = 3)
  
  # standard error is the square root of variance
  st_err_d <- round(sqrt(var_d), digits = 3)
  
  # print everything out
  results <- c(d, var_d, st_err_d)
  
  col_names <- c('Standardized Effect (Cohens D)', 
                 'Variance of D', 'Standard Error of D')
  
  results_tableble <-tibble::tribble(~d, ~var_d, ~st_err_d,
                         d, var_d, st_err_d)
  
  #print(results_tableble)
  return(results_tableble)
}
