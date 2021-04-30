
#plot(allQ[,2,1,],allQ_sim[,2,1,])
#lines(x=c(0,10),y=c(0,10),col="red")
#print(cor(as.vector(allQ[,2,1,]),as.vector(allQ_sim[,2,1,])))

#plot(allQ[,2,2,],allQ_sim[,2,2,])
#lines(x=c(0,10),y=c(0,10),col="red")
#print(cor(as.vector(allQ[,2,2,]),as.vector(allQ_sim[,2,2,])))

phi[,"z.mu"]
hist(phi[,"z.mu",])
abline(v=0.5,col="red")
quantile(phi[,"z.mu",],c(0.025,0.975))

plot(density(phi[,"z.mu",]),
main = "Phi Z Mean")
abline(v=0.5,col="red")
quantile(phi[,"z.mu",],c(0.025,0.975))

phi.v.mu = phi[,"v.1.mu",]

hist(phi[,"v.1.mu",]-phi[,"v.2.mu",])
abline(v=0,col="red")
quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))

plot(density(phi[,"v.1.mu",]-phi[,"v.2.mu",]), 
     main = "Phi v Congruent - Phi v Incongrunet" )
abline(v=0,col="red")
quantile(phi[,"v.1.mu",]-phi[,"v.2.mu",],c(0.025,0.975))

