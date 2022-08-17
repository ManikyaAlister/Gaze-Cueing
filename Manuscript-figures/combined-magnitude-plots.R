setwd("~/cloudstor/Gaze-Cueing/")
pdf(paste("Manuscript-figures/combined-magnitudes.pdf",sep=""),width=2*4,height=(2*2)+5)
#create matrix "m" which is the size of how many plots I'm going to have. 0 is where there's no plots,
m=matrix(1:12,nrow=6,byrow=T)
m=rbind(0,cbind(0,m,0),0)
m=cbind(m[,1:2],0, m[,3:4])#,m[],0,m[,4:5])
m=rbind(m[1:2,],0, m[3:4,])#,m[3,],0)#,m[4,],0,m[5,],0,m[6,],0,m[7:8,])
layout(mat=m,
       widths=c(0,1,0,1,0),#,1,0),
       heights=c(0,1,0,1,0,1,0))#,1,0,1,0,1,0))
#par(mar=rep(0,4))
par(mar=c(4.6,4.6,6,1))

S = 41
plot(x=100,y=100,xlim=c(0,S),ylim=c(-100,100),xlab="",ylab="Magnitude",main= "Dataset 1",xaxt="n",yaxt="n")
load("Data/dataset1a/derived/magnitudes-params-incl.probs.RData")
table$magnitude = table$magnitude*1000
for (i in 1:S) {
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,table[i,"magnitude"], border = col, col = col)
}
axis(side=2, at=seq(-100,100,50), labels=seq(-100,100,50),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

S =50
plot(x=100,y=100,xlim=c(0,S),ylim=c(-100,100),xlab="",ylab="Magnitude",main= "Dataset 2",xaxt="n",yaxt="n")
load("Data/dataset2/derived/magnitudes-params-incl.probs.RData")
#table$magnitude = table$magnitude*-1
table$magnitude = table$magnitude*1000
for (i in 1:S) {
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,table[i,"magnitude"], border = col, col = col)
}
axis(side=2, at=seq(-100,100,50), labels=seq(-100,100,50),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

S =71
plot(x=100,y=100,xlim=c(0,S),ylim=c(-100,100),xlab="",ylab="Magnitude",main= "Dataset 3",xaxt="n",yaxt="n")
load("Data/dataset3/derived/magnitudes-params-incl.probs.RData")
table$magnitude = table$magnitude*1000
for (i in 1:S) {
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,table[i,"magnitude"], border = col, col = col)
}
axis(side=2, at=seq(-100,100,50), labels=seq(-100,100,50),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

 dev.off()
 
