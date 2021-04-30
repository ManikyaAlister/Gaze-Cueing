rm(list = ls())
#### Posterior analysis for the complex model

setwd("~/Dropbox/2021/Gaze Cueing")

# v = drift rate 
# z = starting point

posteriors_complex = load("Fits_Complex/P1Complex_Model.Rdata")
posterior_means = apply(theta, 2, mean)

#    a        t0       v.1       v.2         z 
# 1.5488920 0.1379236 5.1725264 4.5703711 0.6618674 

#With 5 p's
#        a        t0       v.1       v.2         z 
#   1.5373468 0.1896028 3.1090698 2.8734029 0.4668789 


simData=list(Time=NULL,Cond=NULL,Resp=NULL)

#blah=apply(theta,2,mean)

tmp1=apply(weight,2,max)
tmp2=which.max(tmp1)
tmp3=which.max(weight[,tmp2])

blah=theta[tmp2,,tmp3]




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
  simData$Time=c(simData$Time,tmp$rt)
  simData$Resp=c(simData$Resp,tmp$response)
  simData$Cond=c(simData$Cond,rep(cond,length(tmp$rt)))
}

sim_matrix_complex = as.data.frame(simData)

save(sim_matrix_complex, file = "Data/model_predictions/P5_complex.RData")


# For later 
# max(weight)
#use function to change to BIC/AIC
#Plot differences for posteriors of z and v in complex model. theta where z1 is culumn name - theta where zt is a column name

# diffDistZ=theta[,"z.1",]-theta[,"z.2",]

# Get AIC and BIC
#6 because 6 params

AIC_C = -2*max(weight)+ 2*6 
save(AIC_C, file = "Comparisons/AIC_C.RData")
BIC_C = log(length(data$Time))*6-2*max(weight)
save(BIC_C, file = "Comparisons/BIC_C.RData")
