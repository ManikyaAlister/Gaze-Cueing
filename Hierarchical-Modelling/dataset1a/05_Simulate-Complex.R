rm(list = ls())
#### Script to generate predictided data with the complex model ###
library(rtdists)
setwd("~/Dropbox/2021/Gaze-Cueing")

# v = drift rate 
# z = starting point

load("Hier_Complex_Model.Rdata") #Load parameters

posterior_means = apply(theta, c(2,3), mean) #Check parameters

simData=list() #Prepare a list for the simulated data to fill 

for (s in 1:S) {

simData[[s]]=list(Time=NULL,Cond=NULL,Resp=NULL) #For each participant 's', fill in the 's' item in the list with reaction time, response, and condition

#blah=apply(theta,2,mean)

tmp1=apply(weight[,,s],2,max) 
tmp2=which.max(tmp1)
tmp3=which.max(weight[,tmp2,s])

blah=theta[tmp2,,s,tmp3]




for (cond in conds) {
  currParams=c(blah["a"],NA,blah[paste("v", cond, sep = ".")],blah["t0"])
  names(currParams)=c("a","z","v","t0")
  ## Below omitted for simple and V model
  if (cond=="Valid") {
    currParams["z"]=blah["z"]
  } else {
    currParams["z"]=(1-blah["z"])
  }
  
  tmp=rdiffusion(n=1000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"])
  simData[[s]]$Time=c(simData[[s]]$Time,tmp$rt)
  simData[[s]]$Resp=c(simData[[s]]$Resp,tmp$response)
  simData[[s]]$Cond=c(simData[[s]]$Cond,rep(cond,length(tmp$rt)))
}

#simData = list(simData)

}

save(simData, file = "Data/Hier-Model-Predictions/complex.RData")


# For later 
# max(weight)
#use function to change to BIC/AIC
#Plot differences for posteriors of z and v in complex model. theta where z1 is culumn name - theta where zt is a column name

# diffDistZ=theta[,"z.1",]-theta[,"z.2",]

# Get AIC and BIC
#6 because 6 params

#AIC_C = -2*apply(weight,3,max)+ 2*6 
#save(AIC_C, file = "Comparisons/AIC_C.RData")
#BIC_C = log(length(data$Time))*6-2*apply(weight,3,max)
#save(BIC_C, file = "Comparisons/BIC_C.RData")
