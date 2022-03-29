
rm(list=ls())
library(tidyverse)
library(rtdists)
library(msm)
setwd("~/cloudstor/Gaze-Cueing/Recovery")

source("04_megaBackground.R")



conds=c(-0.5,0.5)





nSub = 100


####################
####Complex Model###
####################


for (useSub in 1:nSub) {
  
  
  load(paste("Datasets/RECOVERY_DATA-DIFF_LHS-",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  isNonFinishTrial=data$Resp==-1
  
  data$Time=data$Time[!isNonFinishTrial]
  data$Resp=data$Resp[!isNonFinishTrial]
  data$Cond=data$Cond[!isNonFinishTrial]
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    for (cond in conds) {
      a=x["a"]
      t0=x[paste("t0",cond,sep=".")]
      v=x[paste("v",cond,sep=".")]
      if (cond==conds[2]) { #Because incongruent is first
        z=x["z"]
      } else if (cond==conds[1]) {
        z=(1-x["z"])
      } else {
        stop("Error")
      }
  
      #z=x[paste("z",cond,sep=".")]
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=(3-data$Resp[data$Cond==cond]),z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(tmp,1e-10)))
      #tmp1=ddiffusion(rt=data$Time[data$Stim=="Left" & data$Cond==cond],response=data$Resp[data$Stim=="Left" & data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      #tmp2=ddiffusion(rt=data$Time[data$Stim=="Right" & data$Cond==cond],response=data$Resp[data$Stim=="Right" & data$Cond==cond],z=(1-z)*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      #out=out+sum(log(pmax(c(tmp1,tmp2),1e-10)))
    }
    out
  }
  
  
  
  
  theta.names=c("a",paste("t0",conds,sep="."),
                paste("v",conds,sep="."), "z")
  
  savefile=paste("Fits_recovery/fits_recovery_P",useSub,".RData",sep="")
  
  source("03_background.R")
  source("05_runIterativeProcess.R")
  
  
  save(theta,weight,data,burnin,nmc,n.chains,theta.names,conds,genParams,#logMarginalLikelihood,
       file=savefile)
}


