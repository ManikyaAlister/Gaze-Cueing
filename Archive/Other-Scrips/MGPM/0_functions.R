MGPM_likelihood = function(data, run) {
  if(!run %in% c(3, 4)){   # models without valence gain
    tmp = data %>%
      mutate(
        exp_1 = 1 / (1+exp(-time_sensitivity * (TA_1 - TR_1))),
        exp_2 = 1 / (1+exp(-time_sensitivity * (TA_2 - TR_2))),
        util_1 = val_1 * exp_1 / ( 1 + discount_rate * TA_1),
        util_2 = val_2 * exp_2 / ( 1 + discount_rate * TA_2),
        attract_1_mean = (util_1 * 0.8) + 10e-6,
        attract_2_mean = (util_2 * 0.8) + 10e-6,
        attract_diff_mean = attract_1_mean - attract_2_mean,
        attract_1_var = 0.8 * (util_1 - attract_1_mean)^2 + 0.2 * (0 - attract_1_mean)^2,
        attract_2_var = 0.8 * (util_2 - attract_2_mean)^2 + 0.2 * (0 - attract_2_mean)^2,
        attract_diff_var = attract_1_var + attract_2_var + 10e-6,
        prob_prioritize_1 = 1 / (1 + exp(-2 * (attract_diff_mean / sqrt(attract_diff_var)) * threshold)),
        likelihood = if_else(choice_1 == 1, prob_prioritize_1, 1-prob_prioritize_1)
      )
    
  } else{ # models with valence gain
    tmp = data %>%
      mutate(
        val_1 = val_1 ^ valence_gain,
        val_2 = val_2 ^ valence_gain,
        exp_1 = 1 / (1+exp(-time_sensitivity * (TA_1 - TR_1))),
        exp_2 = 1 / (1+exp(-time_sensitivity * (TA_2 - TR_2))),
        util_1 = val_1 * exp_1 / ( 1 + discount_rate * TA_1),
        util_2 = val_2 * exp_2 / ( 1 + discount_rate * TA_2),
        attract_1_mean = (util_1 * 0.8) + 10e-6,
        attract_2_mean = (util_2 * 0.8) + 10e-6,
        attract_diff_mean = attract_1_mean - attract_2_mean,
        attract_1_var = 0.8 * (util_1 - attract_1_mean)^2 + 0.2 * (0 - attract_1_mean)^2,
        attract_2_var = 0.8 * (util_2 - attract_2_mean)^2 + 0.2 * (0 - attract_2_mean)^2,
        attract_diff_var = attract_1_var + attract_2_var + 10e-6,
        prob_prioritize_1 = 1 / (1 + exp(-2 * (attract_diff_mean / sqrt(attract_diff_var)) * threshold)),
        likelihood = if_else(choice_1 == 1, prob_prioritize_1, 1-prob_prioritize_1)
      )
  }
  return(tmp)
}
