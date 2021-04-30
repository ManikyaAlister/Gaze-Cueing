rm(list=ls())

library(loo)
library(rstan)
options(mc.cores = parallel::detectCores())

path = "/30days/s4590609/exp_2_analysis/3.5_loo_output/"

model_list = list()

# load loo files and add to model_list
for(i in 1:5){
  model_name = c("baseline", "threshold", "deadline", "expectancy", "valence")[i]
  file_name = paste0(i, "_", model_name, "_loo.RData")
  
  print(paste0("File: ", file_name))
  load(paste0(path, file_name))
  
  model_list[[i]] = get(model_name)
}

# model comparison
model_comp = loo_compare(model_list)
print("model_comp created")

# print result
print(model_comp, simplify=FALSE, digits=3)

save(model_comp, file="/30days/s4590609/exp_2_analysis/4.5_comparison_output/model_comparison.RData")
print("model_comp saved")