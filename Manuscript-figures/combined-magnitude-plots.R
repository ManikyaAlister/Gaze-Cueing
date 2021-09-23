setwd("~/Documents/2021/Gaze-Cueing/")
pdf(paste("Manuscript-figures/combined-magnitudes.pdf",sep=""),width=3*4,height=(6*4)+3)
#create matrix "m" which is the size of how many plots I'm going to have. 0 is where there's no plots,
m=matrix(1:18,nrow=6,byrow=T)
m=rbind(0,cbind(0,m,0),0)
m=cbind(m[,1:2],0,m[,3],0,m[,4:5])
m=rbind(m[1:2,],0,m[3,],0,m[4,],0,m[5,],0,m[6,],0,m[7:8,])
layout(mat=m,
       widths=c(0,1,0,1,0,1,0),
       heights=c(0,1,0,1,0,1,0,1,0,1,0,1,0))
#par(mar=rep(0,4))
par(mar=c(2.1,4.6,6,1))


plot(x=100,y=100,xlim=c(0,S),ylim=c(-0.1,0.1),xlab="",ylab="",main= "Dataset 1a",xaxt="n",yaxt="n")
load("Data/dataset1a/derived/magnitudes-params-incl.probs.RData")
for (i in 1:S) {
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,table[i,"magnitude"], border = col, col = col)
}
axis(side=2, at=seq(-0.1,0.1,0.05), labels=seq(-0.1,0.1,0.05),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

plot(x=100,y=100,xlim=c(0,S),ylim=c(-0.1,0.1),xlab="",ylab="",main= "Dataset 1b",xaxt="n",yaxt="n")
load("Data/dataset1b/derived/magnitudes-params-incl.probs.RData")
for (i in 1:S) {
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,table[i,"magnitude"], border = col, col = col)
}
axis(side=2, at=seq(-0.1,0.1,0.05), labels=seq(-0.1,0.1,0.05),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)



plot(x=100,y=100,xlim=c(0,S),ylim=c(-0.1,0.1),xlab="",ylab="",main= "Dataset 1c",xaxt="n",yaxt="n")
load("Data/dataset1c/derived/magnitudes-params-incl.probs.RData")
for (i in 1:S) {
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,table[i,"magnitude"], border = col, col = col)
}
axis(side=2, at=seq(-0.1,0.1,0.05), labels=seq(-0.1,0.1,0.05),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

 dev.off()
 