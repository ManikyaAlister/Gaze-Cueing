rm(list=ls())

args <- commandArgs(trailingOnly = T)
run <- as.numeric(  gsub("--","",args[1]) )

library(tidyverse)
library(rstan)
library(loo)
options(mc.cores = parallel::detectCores())

print(paste0("Run: ", run, "; Cores: ", getOption("mc.cores", 1)))

# determine model name
model_name = c("baseline", "threshold", "deadline", "expectancy", "valence")[run]
path = "/30days/s4590609/exp_2_analysis/2.5_loglik_output/"

load(paste0(path, "log_lik_", run, ".Rdata"))

# computes LOO-CV using log_lik and assigns result to the model name
assign(model_name,
       loo(log_lik, cores=24))

save(list = model_name, file=paste0("/30days/s4590609/exp_2_analysis/3.5_loo_output/", run, "_", model_name, "_loo.RData"))

print("saved")