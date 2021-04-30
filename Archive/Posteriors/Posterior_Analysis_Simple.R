rm(list =ls())

#Posterior analysis for the simplest model 

setwd("~/Dropbox/2021/Gaze Cueing")

nSub = 5

for(useSub in 1:nSub) {
  


load(paste("Fits_Simple/P",useSub,"Simple_Model.Rdata", sep = ""))
posterior_means = apply(theta, 2, mean)



#     a        t0         v         z 
# 1.3635816 0.1847956 4.1837735 0.4972824 

# With 5 ps
#    a        t0         v         z 
# 1.4881333 0.1870670 3.0661956 0.5026556 

simData=list(Time=NULL,Cond=NULL,Resp=NULL)

#blah=apply(theta,2,mean)

tmp1=apply(weight,2,max)
tmp2=which.max(tmp1)
tmp3=which.max(weight[,tmp2])

blah=theta[tmp2,,tmp3]

for (cond in conds) {
  currParams=c(blah["a"],0.5,blah["v"],blah["t0"])
  names(currParams)=c("a","z","v","t0")
  

tmp=rdiffusion(n=1000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"])
simData$Time=c(simData$Time,tmp$rt)
simData$Resp=c(simData$Resp,tmp$response)
simData$Cond=c(simData$Cond,rep(cond,length(tmp$rt)))}

sim = as.data.frame(simData)

save(sim, file = paste("Data/model_predictions/P",useSub,"_simple.RData", sep = ""))


# Get AIC and BIC
n.pars = ncol(theta) #Get the the number of parameters for the simple model

AIC_S = -2*max(weight)+ 2*n.pars 
#save(AIC_S, file = paste("Comparisons/P",useSub,"AIC_S.RData", sep = ""))
BIC_S = log(length(data$Time))*n.pars-2*max(weight)
#save(BIC_S, file = paste("Comparisons/P",useSub,"BIC_S.RData", sep = ""))

}
