
rm(list = ls())
setwd("~/cloudstor/Gaze-Cueing/")
#load("Hierarchical-Modelling/07_Output/Hier_Complex_Model.RDdta")
load("Hierarchical-Modelling/dataset2/07_Output/Hier_Complex_Model.Rdata")

CI_z = quantile(phi[,"z.mu",],c(0.025,0.975))
plot(density(phi[,"z.mu",],n = 4096),
main = "Phi Z Mean", sub = paste("CI Lower = ", round(CI_z[1], 2),",", "CI Upper = ", round(CI_z[2],2)))
abline(v=0.5,col="red")

CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 4096), 
     main = "Phi t0 Congruent - Phi t0 Incongrunet", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")

CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 4096), 
     main = "Phi t0 Congruent - Phi t0 Incongrunet", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")

CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",],n = 4096), 
     main = "Phi v Congruent - Phi v Incongrunet", sub = paste("CI Lower = ", round(CI_v[1], 2),",", "CI Upper = ", round(CI_v[2],2)))
abline(v=0,col="red")

