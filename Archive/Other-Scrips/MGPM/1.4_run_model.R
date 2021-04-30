rm(list=ls())

args <- commandArgs(trailingOnly = T)

run <- as.numeric(  gsub("--","",args[1]) )
chain <- as.numeric(  gsub("--","",args[2]) )

print(paste0("Run: ",run,"; Chain: ",chain))

#load libraries
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# define path for model
model = c(
  "/30days/s4590609/exp_2_analysis/0_models/mgpm_baseline_hier.stan",
  "/30days/s4590609/exp_2_analysis/0_models/mgpm_threshold_hier.stan",
  "/30days/s4590609/exp_2_analysis/0_models/mgpm_deadline_hier.stan",
  "/30days/s4590609/exp_2_analysis/0_models/mgpm_expectancy_hier.stan",
  "/30days/s4590609/exp_2_analysis/0_models/mgpm_valence_hier.stan"
)[run]

#load data
load("/30days/s4590609/exp_2_analysis/0_data/analysis_data.RData")

#Convert time pressure condition to integer
data = analysis_data %>% 
  mutate(condition = as.numeric(factor(condition, levels=c("high", "low"))))

#############################
### Prepare data for Stan ###
#############################

#Create data for map_rect function
Ntotal = length(data$subject)
Nsubj = length(unique(data$subject))

#Number of observations for each subject
Obs_each_subj = data %>%
  group_by(subject) %>%
  summarise(count = n()) %>% .$count

#Maximum number of observations for any one subject
Max_obs_per_subj = max(Obs_each_subj)

#matrix for storing real valued variables (TA, TR, and valence)
real_data = matrix(0, nrow = Nsubj, ncol = Max_obs_per_subj*6)

#matrix for storing integer data (choice)
int_data = matrix(0, nrow = Nsubj, ncol = 2 + Max_obs_per_subj*2)

for (subj in sort(unique(data$subject)) ) {
  
  #select predictor variables
  tmp = filter(data,subject==subj) %>%
    select(TA_1,TA_2,TR_1,TR_2,val_1,val_2)
  
  #create matrix of 999s to pad rows
  tmp2 = matrix(999,nrow=Max_obs_per_subj-nrow(tmp),6)
  
  #create matrix of data and combine with padding
  tmp3 = rbind(data.matrix(tmp,rownames.force = NA),tmp2)
  
  #reshape matrix to vector and store
  real_data[subj,] = as.vector(tmp3) #stacks variables on top of each other.
  
  #get choices for relevant subject
  choice = filter(data,subject==subj) %>% .$choice_1
  
  #get vector of 999s to pad choices
  choice_padded = c(choice,rep(999,Max_obs_per_subj - Obs_each_subj[subj]))
  
  #get condition for relevant observations
  condition = filter(data,subject==subj) %>% .$condition
  
  #get vector of 999s to pad choices
  condition_padded = c(condition,rep(999,Max_obs_per_subj - Obs_each_subj[subj]))
  
  int_data[subj, ] = c(Obs_each_subj[subj],
                       Max_obs_per_subj,
                       choice_padded,
                       condition_padded)
}

stan_list = list(Nsubj = Nsubj,
                 Max_obs = Max_obs_per_subj,
                 Ntotal = sum(Obs_each_subj),
                 real_data = real_data,
                 int_data = int_data,
                 subject = dat$subject,
                 choice = dat$choice,
                 TA_1 = dat$data,
                 TA_2,
                 TR_1,
                 TR_2,
                 VAL_1,
                 VAL_2)


#Set number of threads
Sys.setenv(STAN_NUM_THREADS=11)

set.seed(123)
#Fit to data in stan
fit=stan(file=model,
              data=stan_list,
              iter=4000,
              cores=1,
              chains=1,
              pars = c("theta","phi"),
              include = FALSE,
              refresh=1,
              chain_id = chain,
              seed=12345)

save(fit,file=paste0("/30days/s4590609/exp_2_analysis/1.8_output/fit_",run,"_",chain,".RData"))