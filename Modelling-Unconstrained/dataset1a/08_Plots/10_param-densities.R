
nSub = 41 
rm(list = ls())
setwd("~/Dropbox/2021/Gaze-Cueing")

outputs_t0 = list()
for (useSub in 1:nSub) {
  
  load(paste("Modelling-Unconstrained/dataset1a/07_Outputs/P",useSub,"_t0_Model.RData",sep=""))
  
  outputs_t0[[useSub]] = theta
  
}

CI_t0 = quantile(theta[,"t0.1",]-theta[,"t0.2",],c(0.025,0.975))
plot(density(theta[,"t0.1",]-theta[,"t0.2",],n = 4096), 
     main = "theta t0 Congruent - theta t0 Incongrunet Unconstrained", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")


outputs_ap = list()
for (useSub in 1:nSub) {
  
  load(paste("Modelling-Unconstrained/dataset1a/07_Outputs/P",useSub,"_all-params_Model.RData",sep=""))
  
  outputs_ap[[useSub]] = theta
  
}

CI_t0 = quantile(theta[,"t0.1",]-theta[,"t0.2",],c(0.025,0.975))
plot(density(theta[,"t0.1",]-theta[,"t0.2",],n = 4096), 
     main = "theta t0 Congruent - theta t0 Incongrunet Unconstrained", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")


CI_z = quantile(theta[,"z",],c(0.025,0.975))
plot(density(theta[,"z",],n = 4096),
     main = "theta Z Mean Unconstrained", sub = paste("CI Lower = ", round(CI_z[1], 2),",", "CI Upper = ", round(CI_z[2],2)))
abline(v=0.5,col="red")

CI_v = quantile(theta[,"v.1",]-theta[,"v.2",],c(0.025,0.975))
plot(density(theta[,"v.1",]-theta[,"v.2",],n = 4096), 
     main = "theta v Congruent - theta v Incongrunet Unconstrained", sub = paste("CI Lower = ", round(CI_v[1], 2),",", "CI Upper = ", round(CI_v[2],2)))
abline(v=0,col="red")



load("~/Dropbox/2021/Gaze-Cueing/Hierarchical-Modelling/dataset1a/07_Output/Hier_Complex_Model_Unconstrained.Rdata")

CI_z = quantile(theta[,"z",],c(0.025,0.975))
plot(density(theta[,"z",],n = 4096),
     main = "theta Z Mean Unconstrained", sub = paste("CI Lower = ", round(CI_z[1], 2),",", "CI Upper = ", round(CI_z[2],2)))
abline(v=0.5,col="red")

CI_t0 = quantile(theta[,"t0.1",]-theta[,"t0.2",],c(0.025,0.975))
plot(density(theta[,"t0.1",]-theta[,"t0.2",],n = 4096), 
     main = "theta t0 Congruent - theta t0 Incongrunet Unconstrained", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")

CI_t0 = quantile(theta[,"t0.1",]-theta[,"t0.2",],c(0.025,0.975))
plot(density(theta[,"t0.1",]-theta[,"t0.2",],n = 4096), 
     main = "theta t0 Congruent - theta t0 Incongrunet Unconstrained", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")

CI_v = quantile(theta[,"v.1",]-theta[,"v.2",],c(0.025,0.975))
plot(density(theta[,"v.1",]-theta[,"v.2",],n = 4096), 
     main = "theta v Congruent - theta v Incongrunet Unconstrained", sub = paste("CI Lower = ", round(CI_v[1], 2),",", "CI Upper = ", round(CI_v[2],2)))
abline(v=0,col="red")
