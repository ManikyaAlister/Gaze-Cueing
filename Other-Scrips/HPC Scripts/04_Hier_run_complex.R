
rm(list=ls())
library(tidyverse)
library(rtdists)
library(msm)
source("/30days/s4435475/Gaze-Cueing/01_Hier_Megabackground.R")



conds=c(1,2)


nSub = 41
S=nSub

all.data=list()


####################
####Complex Model###
####################


for (useSub in 1:nSub) {
  
  load(paste("/30days/s4435475/Gaze-Cueing/Data/Gregory-and-Jackson-(2020)/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  all.data[[useSub]]=data
  
}

data=all.data
rm(all.data)


log.dens.like = function (x,data,par.names) {
  out=0
  names(x)=par.names
  for (cond in conds) {
    a=x["a"]
    t0=x["t0"]
    v=x[paste("v",cond,sep=".")]
    if (cond=="Valid") {
      z=x["z"]
    } else {
      z=(1-x["z"])
    }
    
    #z=x[paste("z",cond,sep=".")]
    sv=0
    sz=0
    st0=0
    s=1
    tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=data$Resp[data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
    out=out+sum(log(pmax(tmp,1e-10)))
    #tmp1=ddiffusion(rt=data$Time[data$Stim=="Left" & data$Cond==cond],response=data$Resp[data$Stim=="Left" & data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
    #tmp2=ddiffusion(rt=data$Time[data$Stim=="Right" & data$Cond==cond],response=data$Resp[data$Stim=="Right" & data$Cond==cond],z=(1-z)*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
    #out=out+sum(log(pmax(c(tmp1,tmp2),1e-10)))
  }
  out
  
  
}





theta.names=c("a","t0",
              paste("v",conds,sep="."), "z")

phi.names=paste(rep(theta.names,each=2),rep(c("mu","sigma"),length(theta.names)),sep=".")

savefile=paste("/30days/s4435475/Gaze-Cueing/05_output/Hier_Complex_Model.Rdata",sep="")

source("/30days/s4435475/Gaze-Cueing/02_Hier_Background.R")
source("/30days/s4435475/Gaze-Cueing/03_Hier_iterative.R")


save(theta,phi,weight,data,burnin,nmc,n.chains,theta.names,phi.names,conds,S,#logMarginalLikelihood,
     file=savefile)


hist(phi[,"z.mu",])
hist(phi[,"v.1.mu",]-phi[,"v.2.mu",])

