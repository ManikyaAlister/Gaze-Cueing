rm(list = ls())
#### Posterior analysis for the complex model

setwd("~/Dropbox/2021/Gaze Cueing")

# v = drift rate 
# z = starting point

posteriors_v = load("Fits_z/P1z_Model.Rdata")
posterior_means = apply(theta, 2, mean)
posterior_means


#    a        t0         v       z.1       z.2 
#2.0640540 0.3020748 3.0260441 0.4950239 0.4992810 

#With 5 P's
#       a        t0         v         z 
#1.5333056 0.1896924 2.9777662 0.4669533 

simData=list(Time=NULL,Cond=NULL,Resp=NULL)

blah=apply(theta,2,mean)

for (cond in conds) {
  currParams=c(blah["a"],NA,blah["v"],blah["t0"])
  names(currParams)=c("a","z","v","t0")
  ## Below omitted for simple and V model
  if (cond=="Valid") {
    currParams["z"]=blah["z"]
  } else {
    currParams["z"]=(1-blah["z"])
  }
  
  tmp=rdiffusion(n=1000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"])
  simData$Time=c(simData$Time,tmp$rt)
  simData$Resp=c(simData$Resp,tmp$response)
  simData$Cond=c(simData$Cond,rep(cond,length(tmp$rt)))
}

sim_matrix_z = as.data.frame(simData)

save(sim_matrix_z, file = "Data/model_predictions/P5_z.RData")



#AIC and BIC comparisons
n.pars = ncol(theta) + 1 #I think because of the way "z" is calculated it isn't included in the theta variable

AIC_z = -2*max(weight)+ 2*n.pars 
save(AIC_z, file = "Comparisons/AIC_z.RData")
BIC_z = log(length(data$Time))*n.pars-2*max(weight)
save(BIC_z, file = "Comparisons/BIC_z.RData")
