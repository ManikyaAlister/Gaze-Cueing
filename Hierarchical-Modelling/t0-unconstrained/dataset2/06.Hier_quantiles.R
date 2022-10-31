rm(list = ls())
setwd("~/cloudstor/Gaze-Cueing")
library(tidyverse)
library(jtools)
nSub = 50
all.data=list()

####### Observed Data ########
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset2/clean/P",useSub,".Rdata",sep=""))

  
  all.data[[useSub]]=data
  
}
rm(data)

qs=seq(0.1,0.9,0.1) #Define quantiles

tmp=lapply(all.data,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:nSub) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ=array(unlist(tmp),c(length(qs),2,2,nSub))


tmp=lapply(all.data,function(x) tapply(x$Resp==2,x$Cond,mean))

allP=array(unlist(tmp),c(2,nSub))

#Means for congruent condition
q.mean.2.1=apply(allQ[,2,1,],1,mean)
p.mean.2.1=mean(allP[1,])

#Means for incongruent condition
q.mean.2.2=apply(allQ[,2,2,],1,mean) 
p.mean.1.1=mean(allP[2,]) #I'm not sure if this is indexed properly. See below. 

########### Redo everything for simData (simulated data) ###########

# Load in predicted data generated from complex DDM (simData)
load("Data/dataset2/hier-model-predictions/t0-unconstrained/complex.RData")


tmp=lapply(simData,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:nSub) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}


allQ_sim=array(unlist(tmp),c(length(qs),2,2,nSub))


tmp=lapply(simData,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_sim=array(unlist(tmp),c(2,nSub))

#Means for congruent cue condition
q.mean.2.1_Sim=apply(allQ_sim[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_sim[,2,COND,]...)
p.mean.2.1_Sim=mean(allP_sim[1,])

#Means for incongruent cue condition
q.mean.2.2_Sim=apply(allQ_sim[,2,2,],1,mean) 
p.mean.1.1_Sim=mean(allP_sim[2,]) 




#------------------------------#
#----------- Plotting ---------#
#------------------------------#

pdf("Hierarchical-Modelling/t0-unconstrained/dataset2/08_plots/quantiles.pdf",width=7,height=7)

plot(q.mean.2.1,qs*p.mean.1.1,pch=16, cex = 0.9, cex.lab = .9, xlab = "", ylab = "", main = "Hier. Complex Model", xlim = c(.2,.6)) 
points(q.mean.2.1_Sim,qs*p.mean.1.1_Sim,pch=17, cex = 0.9, type = "l")
points(q.mean.2.1_Sim,qs*p.mean.1.1_Sim,pch=17, cex = 0.9)
points(q.mean.2.2,qs*p.mean.2.1,pch=1, cex = 0.9) 
points(q.mean.2.2_Sim,qs*p.mean.2.1_Sim,pch=2, cex = 0.9)
points(q.mean.2.2_Sim,qs*p.mean.2.1_Sim,pch=2, type = "l", cex = 0.9)

dev.off()

