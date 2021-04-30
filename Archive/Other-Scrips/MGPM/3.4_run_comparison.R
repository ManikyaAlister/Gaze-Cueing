rm(list=ls())

library(loo)
library(rstan)
options(mc.cores = parallel::detectCores())

path = "/30days/s4435475/e2_analysis_ll/2.5_waic_output/"

all_names = c("baseline", "threshold", "deadline", "expectancy", "valence")

waic_list = list()
DIC_mat = matrix(data=NA, nrow=5)

for(i in 1:5){
  model_name = all_names[i]
  waic_name = paste0(i, "_", model_name, "_waic.RData")
  dic_name = paste0(i, "_", model_name, "_dic.RData")
  
  load(paste0(path, waic_name))
  load(paste0(path, dic_name))
  
  waic_list[[i]] = get(model_name)
  DIC_mat[i,] = DIC
}

names(waic_list) = all_names
rownames(DIC_mat) = all_names
colnames(DIC_mat) = "DIC"

# model comparison
waic_comp = loo_compare(waic_list)
print("comp files created")

print(waic_comp, simplify=FALSE, digits=3)
print(DIC_mat)

save(waic_comp, file="/30days/s4435475/e2_analysis_ll/3.5_comparison_output/waic_comparison.RData")
save(DIC_mat, file="/30days/s4435475/e2_analysis_ll/3.5_comparison_output/dic_comparison.RData")
print("comp files saved")