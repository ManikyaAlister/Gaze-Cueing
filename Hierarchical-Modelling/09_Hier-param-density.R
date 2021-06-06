
#plot(allQ[,2,1,],allQ_sim[,2,1,])
#lines(x=c(0,10),y=c(0,10),col="red")
#print(cor(as.vector(allQ[,2,1,]),as.vector(allQ_sim[,2,1,])))

#plot(allQ[,2,2,],allQ_sim[,2,2,])
#lines(x=c(0,10),y=c(0,10),col="red")
#print(cor(as.vector(allQ[,2,2,]),as.vector(allQ_sim[,2,2,])))

load("07_Output/Hier_Complex_Model.RDdta")
load("~/Dropbox/2021/Gaze-Cueing/Hierarchical-Modelling/07_Output/Hier_Complex_Model.Rdata")

CI_z = quantile(phi[,"z.mu",],c(0.025,0.975))
plot(density(phi[,"z.mu",]),
main = "Phi Z Mean", sub = paste("CI Lower = ", round(CI_z[1], 2),",", "CI Upper = ", round(CI_z[2],2)))
abline(v=0.5,col="red")

CI_t0 = CI_v = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",]), 
     main = "Phi t0 Congruent - Phi t0 Incongrunet", sub = paste("CI Lower = ", round(CI_v[1], 2),",", "CI Upper = ", round(CI_v[2],2)))
abline(v=0,col="red")

CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",]), 
     main = "Phi v Congruent - Phi v Incongrunet", sub = paste("CI Lower = ", round(CI_v[1], 2),",", "CI Upper = ", round(CI_v[2],2)))
abline(v=0,col="red")


#phi[,"z.mu"]
#hist(phi[,"z.mu",])
#abline(v=0.5,col="red")
#quantile(phi[,"z.mu",],c(0.025,0.975))

#hist(phi[,"v.1.mu",]-phi[,"v.2.mu",])
#abline(v=0,col="red")
#quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))


#phi.v.mu = phi[,"v.1.mu",]
