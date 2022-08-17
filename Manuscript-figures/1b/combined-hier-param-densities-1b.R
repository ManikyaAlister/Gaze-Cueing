setwd("~/cloudstor/Gaze-Cueing/")
pdf("Manuscript-figures/combined-hier-param-densities.pdf",width=3*4,height=(3*4))
#create matrix "m" which is the size of how many plots I'm going to have. 0 is where there's no plots,
m=matrix(1:18,nrow=6,byrow=T)
m=rbind(0,cbind(0,m,0),0)
m=cbind(m[,1:2],0,m[,3],0,m[,4:5])
m=rbind(m[1:2,],0,m[3,],0,m[4,],0,m[5,])
layout(mat=m,
       widths=c(0,1,0,1,0,1,0),
       heights=c(0,1,0,1,0,1,0,1,0,1,0,1,0))
#par(mar=rep(0,4))
#par(mar=c(2.1,4.6,6,0.5))
#par(mar=rep(0,4))
par(mar=c(6,6,3,3), ann =  FALSE)

datasets = c("Dataset 1a", "Dataset 1b", "Dataset 2", "Dataset 3")



#### Dataset 1a ######

load("Hierarchical-Modelling/dataset1a/07_Output/Hier_Complex_Model.Rdata")

CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.5, 0.975), na.rm = TRUE)
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 4965), main = "", ylab = "", na.rm = T)
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_t0[2], 3),", CI Lower = ", round(CI_t0[1], 3),",", " CI Upper = ", round(CI_t0[3],3), sep =""),cex=1, font = 1)
mtext(side=2,line=3,datasets[1],cex=1, font = 2)
mtext(side=3,line=0.8,"Non-decision time (t0) difference",cex=1, font = 2)

CI_z = quantile(phi[,"z.mu",]-(1-phi[,"z.mu",]),c(0.025,0.5, 0.975))
plot(density(phi[,"z.mu",]-(1-phi[,"z.mu",]),n = 7000), main = "", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_z[2], 3),", CI Lower = ", round(CI_z[1], 3),",", " CI Upper = ", round(CI_z[3],3), sep =""),cex=1, font = 1)
mtext(side=3,line=0.8,"Starting point (z) mean",cex=1, font = 2)

CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.5, 0.975))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",],n = 7000),main ="", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_v[2], 3), ", CI Lower = ", round(CI_v[1], 3),",", " CI Upper = ", round(CI_v[3],3),sep =""),cex=1, font = 1)
mtext(side=3,line=0.8,"Drift rate (v) difference",cex=1, font = 2)

#### t0 #####
#And same principle for t0:

###### Dataset 1b ##########

load("Hierarchical-Modelling/dataset1b/07_Output/Hier_Complex_Model.Rdata")


CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.5, 0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 7000), main = "", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_t0[2], 3),", CI Lower = ", round(CI_t0[1], 3),",", " CI Upper = ", round(CI_t0[3],3), sep =""),cex=1, font = 1)
mtext(side=2,line=3,datasets[2],cex=1, font = 2)

CI_z = quantile(phi[,"z.mu",]-(1-phi[,"z.mu",]),c(0.025,0.5, 0.975))
plot(density(phi[,"z.mu",]-(1-phi[,"z.mu",]),n = 7000), main = "", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_z[2], 3),", CI Lower = ", round(CI_z[1], 3),",", " CI Upper = ", round(CI_z[3],3), sep =""),cex=1, font = 1)

CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.5, 0.975))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",],n = 7000),main ="", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_v[2], 3), ", CI Lower = ", round(CI_v[1], 3),",", " CI Upper = ", round(CI_v[3],3),sep =""),cex=1, font = 1)




###### Dataset 2 ##########

load("Hierarchical-Modelling/dataset2/07_Output/Hier_Complex_Model.Rdata")

CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.025,0.5, 0.975))
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 7000), main = "", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_t0[2], 3),", CI Lower = ", round(CI_t0[1], 3),",", " CI Upper = ", round(CI_t0[3],3), sep =""),cex=1, font = 1)
mtext(side=2,line=3,datasets[3],cex=1, font = 2)

CI_z = quantile(phi[,"z.mu",]-(1-phi[,"z.mu",]),c(0.025,0.5, 0.975))
plot(density(phi[,"z.mu",]-(1-phi[,"z.mu",]),n = 7000), main = "", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_z[2], 3),", CI Lower = ", round(CI_z[1], 3),",", " CI Upper = ", round(CI_z[3],3), sep =""),cex=1, font = 1)

CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.5, 0.975))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",],n = 7000),main ="", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_v[2], 3), ", CI Lower = ", round(CI_v[1], 3),",", " CI Upper = ", round(CI_v[3],3),sep =""),cex=1, font = 1)




#### Dataset 3 ######

load("Hierarchical-Modelling/dataset3/07_Output/Hier_Complex_Model.Rdata")

CI_t0 = quantile(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],c(0.05,0.5, 0.95), na.rm = TRUE)
plot(density(phi[,"t0.1.mu",]-phi[,"t0.2.mu",],n = 10000), main = "", ylab = "", na.rm = T)
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_t0[2], 3),", CI Lower = ", round(CI_t0[1], 3),",", " CI Upper = ", round(CI_t0[3],3), sep =""),cex=1, font = 1)
mtext(side=2,line=3,datasets[4],cex=1, font = 2)
#mtext(side=3,line=0.8,"Non-decision time (t0) difference",cex=1, font = 2)

CI_z = quantile(phi[,"z.mu",]-(1-phi[,"z.mu",]),c(0.025,0.5, 0.975))
plot(density(phi[,"z.mu",]-(1-phi[,"z.mu",]),n = 7000), main = "", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_z[2], 3),", CI Lower = ", round(CI_z[1], 3),",", " CI Upper = ", round(CI_z[3],3), sep =""),cex=1, font = 1)
#mtext(side=3,line=0.8,"Starting point (z) mean",cex=1, font = 2)

CI_v = quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.05,0.5, 0.95))
plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",],n = 7000),main ="", ylab = "")
abline(v=0,col="red")

mtext(side=1,line=3.5,paste("M = ",round(CI_v[2], 3), ", CI Lower = ", round(CI_v[1], 3),",", " CI Upper = ", round(CI_v[3],3),sep =""),cex=1, font = 1)
#mtext(side=3,line=0.8,"Drift rate (v) difference",cex=1, font = 2)




dev.off()
