

rm(list=ls())
setwd("~/Dropbox/2021/Gaze-Cueing")
library(tidyverse)
library(rtdists)
library(msm)
source("Modelling/02_megaBackground.R")

conds=c(1,2)

####################
####Z Model###
####################


n_datasets = 1 #number of data sets being run 
dataset_lengths = c(41) #number of participants in each data set

for (dataset in 1:n_datasets) {
  
  nSub = dataset_lengths[dataset]

for (useSub in 1:nSub) {
  
  load(paste("Data/Gregory-and-Jackson-(2020)/P",useSub,".Rdata",sep=""))
  
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    if (x["z"] < 0.5) { # =Make sure that starting point is never less than 0.5 for congruent condition
      return(-Inf)
    }
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=x["v"]
      if (cond==conds[2]) { #Z not estimated in the incongruent condition
        z=0.5
      } else if (cond==conds[1]) {
        z=x["z"]
      } else {
        stop("Error")
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
                "v",
                "z")
  
  savefile=paste("Modelling/07_Outputs/DS",dataset,"_P",useSub,"_zfacil_Model.Rdata",sep="")
  
  source("Modelling/03_background.R")
  source("Modelling/04_runIterativeProcess.R")
  
  n.pars = length(theta.names)
  
  # Calculate AIC and BIC for each participant 
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  
  
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,#logMarginalLikelihood,
       file=savefile)
}

##### Simulate Data Using Parameters ####
conds = c(1,2)
for(useSub in 1:nSub) {
  
  
  load(paste("Modelling/07_Outputs/DS",dataset,"_P",useSub,"_zfacil_Model.Rdata",sep="")) #Loads through the datasets of each participant in nSub
  #posterior_means = apply(theta, 2, mean) #This code just gets the mean parameter estimates of each data set (not necessary for the loop)
  
  
  simData=list(Time=NULL,Cond=NULL,Resp=NULL) #Sets up a list with the correct headings in preparation for the simulation
  
  blah=apply(theta,2,mean)
  
  tmp1=apply(weight,2,max)
  tmp2=which.max(tmp1)
  tmp3=which.max(weight[,tmp2])
  
  blah=theta[tmp2,,tmp3]
  
  for (cond in conds) { # Loops through each cue condition (congruent and incongruent)
    currParams=c(blah["a"],NA,blah["v"],blah["t0"]) # Sets the value of parameters. 
    names(currParams)=c("a","z","v","t0")  # Sets the names of the parameters
    if (cond==conds[1]) {
      currParams["z"]=0.5
    } else if (cond==conds[2]) {
      currParams["z"]=(1-blah["z"])
    } else {
      stop("Error")
    }
    
    tmp=rdiffusion(n=1000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"]) # Runs diffusion model to generated data with estimated parameters
    simData$Time=c(simData$Time,tmp$rt) # Populates the RT column in the simulated data
    simData$Resp=c(simData$Resp,tmp$response) # Populates the Resp column in the simulated data 
    simData$Cond=c(simData$Cond,rep(cond,length(tmp$rt)))} # Populates the Cond column in the simulated data
  
  sim = as.data.frame(simData) # Convert the simulated data from List format to data frame format
  
  save(sim, file = paste("Data/Model-Predictions/DS",dataset,"_P",useSub,"_zfacil.Rdata", sep = ""))
  
}
}

