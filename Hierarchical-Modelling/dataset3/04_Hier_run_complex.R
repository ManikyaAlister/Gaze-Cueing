rm(list=ls())
library(tidyverse)
library(rtdists)
library(msm)
library(here)
setwd(here())
source("Hierarchical-Modelling/dataset3/01_Hier_Megabackground.R")



conds=c(1,2)


nSub = 71


all.data=list()


####################
####Complex Model###
####################


for (useSub in 1:nSub) {
  load(paste("Data/dataset3/clean/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  all.data[[useSub]]=data

}


all.data[sapply(all.data, is.null)] = NULL #remove null values 
data=all.data
rm(all.data)
S=length(data)

log.dens.like = function (x,data,par.names) {
  out=0
  names(x)=par.names
  if (x[paste("v",conds[1],sep=".")] < x[paste("v",conds[2],sep=".")]) {
    return(-Inf)
  } 
  if (x["z"] < 0.5) {  
    return(-Inf)
  }
  if (x[paste("t0",conds[1],sep=".")] > x[paste("t0",conds[2],sep=".")]) {
    return(-Inf)
  } 
  for (cond in conds) {
    a=x["a"]
    t0=x[paste("t0",cond,sep=".")]
    v=x[paste("v",cond,sep=".")]
    if (cond==conds[1]) {
      z=x["z"]
    } else if (cond==conds[2]) {
      z=(1-x["z"])
    } else {
      stop("Error")
    }
    
    sv=0
    sz=0
    st0=0
    s=1
    tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=data$Resp[data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
    out=out+sum(log(pmax(tmp,1e-10)))
  }
  out
  
  
}





theta.names=c("a",paste("t0",conds,sep="."),
              paste("v",conds,sep="."), "z")

phi.names=paste(rep(theta.names,each=2),rep(c("mu","sigma"),length(theta.names)),sep=".")

savefile=paste("Hierarchical-Modelling/dataset3/07_Output/Hier_Complex_Model.Rdata",sep="")




source("Hierarchical-Modelling/dataset3/02_Hier_Background.R")
source("Hierarchical-Modelling/dataset3/03_Hier_iterative.R")

save(theta,phi,weight,data,burnin,nmc,n.chains,theta.names,phi.names,conds,S,#logMarginalLikelihood,
     file=savefile)



