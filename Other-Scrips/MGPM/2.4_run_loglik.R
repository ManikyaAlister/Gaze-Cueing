rm(list=ls())

args = commandArgs(trailingOnly = TRUE)
run = as.numeric(  gsub("--","",args) ) #arg passed from terminal

print(run)

library(tidyverse)
library(rstan)
source('/30days/s4590609/exp_2_analysis/0_functions.R')

load("/30days/s4590609/exp_2_analysis/0_data/analysis_data.RData")
fit_file = paste0("/30days/s4590609/exp_2_analysis/1.8_output/fit_", run, ".RData")
print(fit_file)
load(fit_file)

data = analysis_data %>% 
  mutate(subject = as.integer(subject),
         condition = as.numeric(factor(condition, levels=c("high", "low"))))

posteriors = extract(fit)
nsubjects = max(data$subject)
nsamples = nrow(posteriors$lp__)
nobs = nrow(data)
log_lik = matrix(NA,nrow=nobs,ncol=nsamples)

for(sample in 1:nsamples){
  if(sample %% 100 == 0) { print(sample) }
  #this creates a mini dataset containing the parameters for each subject
  if(run == 1){ # baseline model
    subject_parms = tibble(
      subject = 1:nsubjects,
      time_sensitivity = posteriors$time_sensitivity[sample,],
      discount_rate = posteriors$discount_rate[sample,],
      threshold = posteriors$threshold[sample,]
    )
  }else if(run == 2){ # threshold model
    subject_parms = tibble(
      subject = 1:nsubjects,
      time_sensitivity = posteriors$time_sensitivity[sample,],
      discount_rate = posteriors$discount_rate[sample,],
      threshold__high = posteriors$threshold_high[sample,],
      threshold__low = threshold__high + posteriors$threshold_diff_low_high[sample,]
    ) %>%
      gather(key="key", value="value", threshold__high:threshold__low) %>% 
      separate(col="key",into=c("parameter","condition"),sep="__") %>%
      spread(key=parameter,value=value) %>%
      mutate(condition = as.numeric(factor(condition,levels=c("high","low"))))
  }else if(run == 3){ # deadline model
    subject_parms = tibble(
      subject = 1:nsubjects,
      discount_rate = posteriors$discount_rate[sample,],
      threshold = posteriors$threshold[sample,],
      time_sensitivity__high = posteriors$time_sensitivity_high[sample,],
      time_sensitivity__low = time_sensitivity__high + posteriors$time_sensitivity_diff_low_high[sample,],
      valence_gain__high = pnorm(posteriors$valence_gain_high_raw[sample,] * posteriors$valence_gain_high_sd[sample] + posteriors$valence_gain_high_mean[sample]),
      valence_gain__low = 1,
    ) %>%
      gather(key="key", value="value", time_sensitivity__high:valence_gain__low) %>% 
      separate(col="key",into=c("parameter","condition"),sep="__") %>%
      spread(key=parameter,value=value) %>%
      mutate(condition = as.numeric(factor(condition,levels=c("high","low"))))
  }else if(run == 4){ # expectancy model
    subject_parms = tibble(
      subject = 1:nsubjects,
      discount_rate__high = posteriors$discount_rate_high[sample,],
      discount_rate__low = discount_rate__high + posteriors$discount_rate_diff_low_high[sample,],
      valence_gain__high = pnorm(posteriors$valence_gain_high_raw[sample,] * posteriors$valence_gain_high_sd[sample] + posteriors$valence_gain_high_mean[sample]),
      valence_gain__low = 1,
      time_sensitivity = posteriors$time_sensitivity[sample,],
      threshold = posteriors$threshold[sample,]
    ) %>% 
      gather(key="key", value="value", discount_rate__high:valence_gain__low) %>% 
      separate(col="key",into=c("parameter","condition"),sep="__") %>%
      spread(key=parameter,value=value) %>%
      mutate(condition = as.numeric(factor(condition,levels=c("high","low"))))
  }else if(run == 5){
    subject_parms = tibble(
      subject = 1:nsubjects,
      discount_rate__high = posteriors$discount_rate_high[sample,],
      discount_rate__low = discount_rate__high + posteriors$discount_rate_diff_low_high[sample,],
      time_sensitivity__high = posteriors$time_sensitivity_high[sample,],
      time_sensitivity__low = time_sensitivity__high + posteriors$time_sensitivity_diff_low_high[sample,],
      threshold = posteriors$threshold[sample,]
    ) %>% 
      gather(key="key", value="value", discount_rate__high:time_sensitivity__low) %>% 
      separate(col="key",into=c("parameter","condition"),sep="__") %>%
      spread(key=parameter,value=value) %>%
      mutate(condition = as.numeric(factor(condition,levels=c("high","low"))))
  }
  
  #join subject parameters into big dataset and calculate likelihood
  if(run == 1){
    tmp = left_join(data,subject_parms,by="subject")
  } else{
    tmp = left_join(data,subject_parms,by=c("subject", "condition"))
  }
  model_preds = MGPM_likelihood(tmp, run)

  log_lik[ , sample ] = log(model_preds$likelihood)
}

save(log_lik, file=paste0("/30days/s4590609/exp_2_analysis/2.5_loglik_output/log_lik_", run, ".Rdata"))
print('saved')
