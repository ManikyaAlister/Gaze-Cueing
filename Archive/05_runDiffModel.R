
rm(list=ls())
library(tidyverse)
library(rtdists)
library(msm)
source("Modelling/02_megaBackground.R")

load("Data/Gregory and Jackson (2020)/P5.RData")

conds=c(1,2)

nSub = max(P5$ID)

data=P5
data$Time=as.numeric(data$Time)
data$Resp=as.numeric(data$Resp)
####################
####Complex Model###
####################


for (useSub in 1:nSub) {
  
  #load(paste("Data/Gregory and Jackson (2020)/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=x[paste("v",cond,sep=".")]
      z=x[paste("z",cond,sep=".")]
      sv=0
      sz=0
      st0=0
      s=1
      tmp1=ddiffusion(rt=data$Time[data$Stim=="Left" & data$Cond==cond],response=data$Resp[data$Stim=="Left" & data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      tmp2=ddiffusion(rt=data$Time[data$Stim=="Right" & data$Cond==cond],response=data$Resp[data$Stim=="Right" & data$Cond==cond],z=(1-z)*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(c(tmp1,tmp2),1e-10)))
    }
    out
  }
  
  
  
  
  
  theta.names=c("a","t0",
                paste("v",conds,sep="."),
                paste("z",conds,sep="."))
  
  savefile=paste("Fits_Complex/P",useSub,"Complex_Model.Rdata",sep="")
  
  source("Modelling/03_background.R")
  source("Modelling/04_runIterativeProcess.R")
  
  
  save(theta,weight,data,burnin,nmc,n.chains,theta.names,conds,#logMarginalLikelihood,
       file=savefile)
}
####################
####################

for (useSub in 1:nSub) {
  
  load(paste("Data/Gregory and Jackson (2020)/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=x["v"]
      z=x["z"]
      sv=0
      sz=0
      st0=0
      s=1
      tmp1=ddiffusion(rt=data$Time[data$Stim=="left" & data$Cond==cond],response=data$Resp[data$Stim==1 & data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      tmp2=ddiffusion(rt=data$Time[data$Stim=="right" & data$Cond==cond],response=data$Resp[data$Stim==2 & data$Cond==cond],z=(1-z)*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(c(tmp1,tmp2),1e-10)))
    }
    out
  }
  
  
  
  
  
  theta.names=c("a","t0",
                "v","z")
  
  savefile=paste("Fits_Complex/P",useSub,"Simple_Model.Rdata",sep="")
  
  source("Modelling/03_background.R")
  source("Modelling/04_runIterativeProcess.R")
  
  
  save(theta,weight,data,burnin,nmc,n.chains,theta.names,conds,#logMarginalLikelihood,
       file=savefile)
}

####################
###### V Model######
####################

for (useSub in 1:nSub) {
  
  load(paste("Data/Gregory and Jackson (2020)/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=x[paste("v",cond,sep=".")]
      z=x["z"]
      sv=0
      sz=0
      st0=0
      s=1
      tmp1=ddiffusion(rt=data$Time[data$Stim=="left" & data$Cond==cond],response=data$Resp[data$Stim==1 & data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      tmp2=ddiffusion(rt=data$Time[data$Stim=="right" & data$Cond==cond],response=data$Resp[data$Stim==2 & data$Cond==cond],z=(1-z)*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(c(tmp1,tmp2),1e-10)))
    }
    out
  }
  
  
  
  
  
  theta.names=c("a","t0",
                paste("v",cond,sep="."),"z")
  
  savefile=paste("Fits_Complex/P",useSub,"v_Model.Rdata",sep="")
  
  source("Modelling/03_background.R")
  source("Modelling/04_runIterativeProcess.R")
  
  
  save(theta,weight,data,burnin,nmc,n.chains,theta.names,conds,#logMarginalLikelihood,
       file=savefile)
}

####################
####### Z Model#####
####################

for (useSub in 1:nSub) {
  
  load(paste("Data/Gregory and Jackson (2020)/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=x["v"]
      z=x[paste("z",cond,sep=".")]
      sv=0
      sz=0
      st0=0
      s=1
      tmp1=ddiffusion(rt=data$Time[data$Stim=="left" & data$Cond==cond],response=data$Resp[data$Stim==1 & data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      tmp2=ddiffusion(rt=data$Time[data$Stim=="right" & data$Cond==cond],response=data$Resp[data$Stim==2 & data$Cond==cond],z=(1-z)*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(c(tmp1,tmp2),1e-10)))
    }
    out
  }
  
  
  
  
  
  theta.names=c("a","t0",
                "v",
                paste("z",cond,sep="."))
  
  savefile=paste("Fits_Complex/P",useSub,"z_Model.Rdata",sep="")
  
  source("Modelling/03_background.R")
  source("Modelling/04_runIterativeProcess.R")
  
  
  save(theta,weight,data,burnin,nmc,n.chains,theta.names,conds,#logMarginalLikelihood,
       file=savefile)
}
