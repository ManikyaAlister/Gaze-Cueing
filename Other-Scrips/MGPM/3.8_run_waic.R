rm(list=ls())

args <- commandArgs(trailingOnly = T)
run <- as.numeric(  gsub("--","",args[1]) )

library(tidyverse)
library(rstan)
library(loo)

model_name = c("baseline", "threshold", "deadline", "expectancy", "valence")[run]
path = "/30days/s4435475/exp_2_analysis/2.5_loglik_output/"

load(paste0(path, "log_lik_", run, ".Rdata"))

# computes WAIC using log_lik and assigns result to the model name
assign(model_name,
       waic(log_lik))

save(list = model_name, file=paste0("/30days/s4435475/exp_2_analysis/3.5_loo_output/", run, "_", model_name, "_waic.RData"))

print("saved")