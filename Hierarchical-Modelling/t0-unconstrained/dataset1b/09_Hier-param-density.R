rm(list = ls())

load("~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/t0-unconstrained/dataset1b/07_Output/Hier_Complex_Model.Rdata")

pdf("Hierarchical-Modelling/t0-unconstrained/dataset1b/08_Plots/z.pdf",width=4,height=4)
CI_z = quantile(phi[,"z.mu",],c(0.025,0.975))
plot(density(phi[,"z.mu",],n = 4096),
main = "Phi Z Mean", sub = paste("CI Lower = ", round(CI_z[1], 2),",", "CI Upper = ", round(CI_z[2],2)))
abline(v=0.5,col="red")
dev.off()

pdf("Hierarchical-Modelling/t0-unconstrained/dataset1b/08_Plots/t0.pdf",width=4,height=4)
CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 4096), 
     main = "Phi t0 Congruent - Phi t0 Incongrunet", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")
dev.off()

CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 4096), 
     main = "Phi t0 Congruent - Phi t0 Incongrunet", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")

pdf("Hierarchical-Modelling/t0-unconstrained/dataset1b/08_Plots/v.pdf",width=4,height=4)
CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",],n = 4096), 
     main = "Phi v Congruent - Phi v Incongrunet", sub = paste("CI Lower = ", round(CI_v[1], 2),",", "CI Upper = ", round(CI_v[2],2)))
abline(v=0,col="red")
dev.off()
