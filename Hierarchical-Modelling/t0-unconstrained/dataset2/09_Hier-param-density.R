
#plot(allQ[,2,1,],allQ_sim[,2,1,])
#lines(x=c(0,10),y=c(0,10),col="red")
#print(cor(as.vector(allQ[,2,1,]),as.vector(allQ_sim[,2,1,])))

#plot(allQ[,2,2,],allQ_sim[,2,2,])
#lines(x=c(0,10),y=c(0,10),col="red")
#print(cor(as.vector(allQ[,2,2,]),as.vector(allQ_sim[,2,2,])))

rm(list = ls())

#load("Hierarchical-Modelling/07_Output/Hier_Complex_Model.RDdta")
load("~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/t0-unconstrained/dataset2/07_Output/Hier_Complex_Model.Rdata")

pdf("Hierarchical-Modelling/t0-unconstrained/dataset2/08_Plots/z.pdf",width=4,height=4)
CI_z = quantile(phi[,"z.mu",],c(0.025,0.975))
plot(density(phi[,"z.mu",],n = 4096),
main = "Phi Z Mean", sub = paste("CI Lower = ", round(CI_z[1], 2),",", "CI Upper = ", round(CI_z[2],2)))
abline(v=0.5,col="red")
dev.off()

pdf("Hierarchical-Modelling/t0-unconstrained/dataset2/08_Plots/t0.pdf",width=4,height=4)
CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 4096), 
     main = "Phi t0 Congruent - Phi t0 Incongrunet", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")
dev.off()

CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 4096), 
     main = "Phi t0 Congruent - Phi t0 Incongrunet", sub = paste("CI Lower = ", round(CI_t0[1], 2),",", "CI Upper = ", round(CI_t0[2],2)))
abline(v=0,col="red")

pdf("Hierarchical-Modelling/t0-unconstrained/dataset2/08_Plots/v.pdf",width=4,height=4)
CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",],n = 4096), 
     main = "Phi v Congruent - Phi v Incongrunet", sub = paste("CI Lower = ", round(CI_v[1], 2),",", "CI Upper = ", round(CI_v[2],2)))
abline(v=0,col="red")
dev.off()

#phi[,"z.mu"]
#hist(phi[,"z.mu",])
#abline(v=0.5,col="red")
#quantile(phi[,"z.mu",],c(0.025,0.975))

#hist(phi[,"v.1.mu",]-phi[,"v.2.mu",])
#abline(v=0,col="red")
#quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))


#phi.v.mu = phi[,"v.1.mu",]
