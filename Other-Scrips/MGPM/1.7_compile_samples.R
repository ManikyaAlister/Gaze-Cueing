library(rstan)

args <- commandArgs(trailingOnly = T)
run <- as.numeric(  gsub("--","",args[1]) )

print(paste0("Run: ",run))


#load individual stanfit objects from each chain
sflist = list()
for(chain in 1:4){
  load(paste0("/30days/s4590609/exp_2_analysis/1.8_output/fit_",run,"_",chain,".RData"))
  sflist[[chain]] = fit
  rm(fit)
}

fit = sflist2stanfit(sflist)

#save fit object
save(fit,file=paste0("/30days/s4590609/exp_2_analysis/1.8_output/fit_",run,".RData"))
