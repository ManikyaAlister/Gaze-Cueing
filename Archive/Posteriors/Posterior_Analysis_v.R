rm(list = ls())
#### Posterior analysis for the complex model

setwd("~/Dropbox/2021/Gaze Cueing")

# v = drift rate 
# z = starting point

posteriors_v = load("Fits_v/P1V_Model.Rdata")
posterior_means = apply(theta, 2, mean)
posterior_means

#      a        t0       v.1       v.2         z 
#  1.3479702 0.1855065 4.4700309 3.8434472 0.4982504 

#With 5 p's
#    a        t0       v.1       v.2         z 
# 1.4909011 0.1870943 3.1950911 2.9568069 0.5038038 
simData=list(Time=NULL,Cond=NULL,Resp=NULL)

blah=apply(theta,2,mean)

for (cond in conds) {
  currParams=c(blah["a"],0.5,blah[paste("v", cond, sep = ".")],blah["t0"])
  names(currParams)=c("a","z","v","t0")

tmp=rdiffusion(n=1000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"])
simData$Time=c(simData$Time,tmp$rt)
simData$Resp=c(simData$Resp,tmp$response)
simData$Cond=c(simData$Cond,rep(cond,length(tmp$rt)))
}

#Putting the data in matrix form
sim_matrix_v = as.data.frame(simData)

save(sim_matrix_v, file = "Data/model_predictions/P5_v.RData")

n.pars = ncol(theta)

AIC_v = -2*max(weight)+ 2*n.pars 
save(AIC_v, file = "Comparisons/AIC_v.RData")
BIC_v = log(length(data$Time))*n.pars-2*max(weight)
save(BIC_v, file = "Comparisons/BIC_v.RData")