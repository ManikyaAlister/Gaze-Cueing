
rm(list = ls())
setwd("~/Dropbox/2021/Gaze-Cueing/Recovery_zinter")

# Set up empty data frames for generated parameters (allGenParamas) and estimated parameters (allMeanTheta)

allGenParams=NULL
allMeanTheta=NULL

#Define how many data sets to use
n = 100

for (p in 1:n) { #Loop in each data set
  load(paste("Fits_recovery/zinter_fits_recovery_P",p,".RData", sep = ""))
  
  #Rearrange and take out unnecessary values from the generated parameters 
  tmp = c(genParams[2,2], genParams[3,2], genParams[4,1], genParams[4,2], genParams[1,2])
  names(tmp) = c("a", "t0", "v.-0.5", "v.0.5", "z")
  
  #Create a large data set which combines the mean generated parameters from all data sets
  allGenParams=rbind(allGenParams,tmp)
  #Create a large data set which combines the mean estimated parameters from all data sets
  allMeanTheta=rbind(allMeanTheta,apply(theta,2,mean))
}
#Create vDiff columns 

allGenParams= as.data.frame(allGenParams)
allMeanTheta= as.data.frame(allMeanTheta)
allGenParams$vDiff = allGenParams$v.0.5 - allGenParams$`v.-0.5`
allMeanTheta$vDiff = allMeanTheta$v.0.5 - allMeanTheta$`v.-0.5`

#Save the generated parameters
save(allGenParams, file = "All_Fits/zinter_Generated_Paramaters.RData")
save(allMeanTheta, file = "All_Fits/zinter_Estimated_Paramaters.RData")

#If I want to reload
load("~/Dropbox/2021/Gaze-Cueing/Recovery/All_Fits/zinter_Estimated_Paramaters.RData")
load("~/Dropbox/2021/Gaze-Cueing/Recovery/All_Fits/zinter_Generated_Paramaters.RData")

cor_zinter = cor(allGenParams[,"z"], allMeanTheta["z"])

plot(allGenParams[,"z"],allMeanTheta[,"z"], 
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_zinter),
     main = "z zInter")


cor_vNeg = cor(allGenParams[,"v.-0.5"],allMeanTheta[,"v.-0.5"])
plot(allGenParams[,"v.-0.5"],allMeanTheta[,"v.-0.5"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_vNeg),
     main = "V.-0.5 zinter")
cor_vPos = cor(allGenParams[,"v.0.5"],allMeanTheta[,"v.0.5"])
plot(allGenParams[,"v.0.5"],allMeanTheta[,"v.0.5"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_vPos),
     main = "V.0.5 zinter")

cor_vDiff = cor(allGenParams[,"vDiff"],allMeanTheta[,"vDiff"])
plot(allGenParams[,"vDiff"],allMeanTheta[,"vDiff"],
     xlab = "Generated Parameter", 
     ylab = "Estimated Parameter", 
     sub = paste("r =",cor_vDiff),
     main = "vDiff zinter")


res_vDiff = allGenParams[,"vDiff"] - allMeanTheta[,"vDiff"]
res_z = allGenParams[,"v.0.5"] - allMeanTheta[,"v.0.5"]
cor_res = cor(res_vDiff, res_z)
plot(res_vDiff, res_z,
     sub = paste("r =",cor_res),
     main = "Residuals zinter")


cor_a = cor(allGenParams[,"a"],allMeanTheta[,"a"])
cor_t0 = cor(allGenParams[,"t0"],allMeanTheta[,"t0"])

all_cor = cbind(cor_zinter, cor_vNeg, cor_vPos, cor_a, cor_t0)

#MAYBE spearman's rho


rbind(allGenParams, allMeanTheta)
