setwd("~/cloudstor/Gaze-Cueing/")
pdf("Manuscript-figures/combined-inclusion-prob-BIC.pdf",width=3*4,height=(4*4)+3)
#create matrix "m" which is the size of how many plots I'm going to have. 0 is where there's no plots,
m=matrix(1:18,nrow=6,byrow=T)
m=rbind(0,cbind(0,m,0),0)
m=cbind(m[,1:2],0,m[,3],0,m[,4:5])
m=rbind(m[1:2,],0,m[3,],0,m[4,])
layout(mat=m,
       widths=c(0,1,0,1,0,1,0),
       heights=c(0,1,0,1,0,1,0,1,0,1,0,1,0))
#par(mar=rep(0,4))
#par(mar=c(2.1,4.6,6,0.5))
#par(mar=rep(0,4))
par(mar=c(2.1,6,6,1))

datasets = c("Dataset 1", "Dataset 2", "Dataset 3")


#### Dataset 1a ######

S = 41 #n participants

#### t0 #####
#And same principle for t0:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")
load("Data/dataset1a/derived/BIC-Weights.RData")

for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","z", "v_z", "v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("t0","z_t0", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=2,line=4.2,datasets[1],cex=1, font = 2)
mtext(side=3,line=0.8,"Non-decision time (t0)",cex=1, font = 2)

### z ###
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")
load("Data/dataset1a/derived/BIC-Weights.RData")

for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","v","t0", "v_t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("z","v_z", "complex", "z_t0")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=3,line=0.8,"Starting point (z)",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

##### v ######
#And same principle for v:
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",main=,xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","z", "z_t0", "t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("v","v_z", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=3,line=0.8,"Drift rate (v)",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

#### Dataset 2 ######

S = 50 #n participants

#### t0 #####
#And same principle for t0:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")
load("Data/dataset2/derived/BIC-Weights.RData")

for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","z", "v_z", "v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("t0","z_t0", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=2,line=4.2,datasets[2],cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

### z ###
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")
load("Data/dataset2/derived/BIC-Weights.RData")

for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","v","t0", "v_t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("z","v_z", "complex", "z_t0")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

##### v ######
#And same principle for v:
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",main=,xaxt="n",yaxt="n")
load("Data/dataset2/derived/BIC-Weights.RData")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","z", "z_t0", "t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("v","v_z", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

#### Dataset 3 ######

S = 71 #n participants

#### t0 #####
#And same principle for t0:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")
load("Data/dataset3/derived/BIC-Weights.RData")

for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","z", "v_z", "v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("t0","z_t0", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=2,line=4.2,datasets[3],cex=1, font = 2)
mtext(side=3,line=0.8,"Non-decision time (t0)",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

### z ###
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")
load("Data/dataset3/derived/BIC-Weights.RData")

for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","v","t0", "v_t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("z","v_z", "complex", "z_t0")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=3,line=0.8,"Starting point (z)",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

##### v ######
#And same principle for v:
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",main=,xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","z", "z_t0", "t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("v","v_z", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=3,line=0.8,"Drift rate (v)",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)


dev.off()
