## Dataset3 ##

rm(list=ls())

lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")

library(here, lib.loc = lib)
library(rtdists, lib.loc = lib)
library(msm, lib.loc = lib)
setwd(here())
source(file = "Modelling/dataset3-ef/02_megaBackground.R")

conds=c(1,2)

  
  nSub = 71

####################
#### Simple Model###
####################


for (useSub in 1:nSub) { #Run DDM for each subject in n Subjects
  
  load(paste("Data/dataset3-ef/clean/P",useSub,".Rdata",sep=""))
  newSeed=Sys.time()
  set.seed(as.numeric(newSeed))
  
  
  log.dens.like = function (x,data,par.names) {
    out=0
    names(x)=par.names
    
    for (cond in conds) {
      a=x["a"]
      t0=x["t0"]
      v=x["v"]
      z=0.5
      sv=0
      sz=0
      st0=0
      s=1
      tmp=ddiffusion(rt=data$Time[data$Cond==cond],response=data$Resp[data$Cond==cond],z=z*a,a=a,v=v,t0=t0-(st0/2),s=s,sv=sv,sz=sz,st0=st0)
      out=out+sum(log(pmax(tmp,1e-10)))
    }
    out
  }

  
  theta.names=c("a","t0",
                "v")
  
  savefile=paste("Modelling/dataset3-ef/07_Outputs/P",useSub,"Simple_Model.Rdata",sep="")
  
  source("Modelling/dataset3-ef/03_background.R")
  source("Modelling/dataset3-ef/04_runIterativeProcess.R")
  
  n.pars = length(theta.names)
  
  AIC = -2*max(weight)+ 2*n.pars 
  BIC = log(length(data$Time))*n.pars-2*max(weight)
  
  save(AIC, BIC, theta,weight,data,burnin,nmc,n.chains,theta.names,conds,
       file=savefile)
}



##### Simulate Data Using Parameters ####

for(useSub in 1:nSub) {
  
  
  load(paste("Modelling/dataset3-ef/07_Outputs/P",useSub,"Simple_Model.Rdata", sep = "")) #Loads through the datasets of each participant in nSub

  
  simData=list(Time=NULL,Cond=NULL,Resp=NULL) #Sets up a list with the correct headings in preparation for the simulation
  
  tmp1=apply(weight,2,max)
  tmp2=which.max(tmp1)
  tmp3=which.max(weight[,tmp2])
  
  blah=theta[tmp2,,tmp3]
  
  for (cond in conds) { # Loops through each cue condition (congruent and incongruent)
    currParams=c(blah["a"],0.5,blah["v"],blah["t0"]) # Sets the value of parameters. 
    names(currParams)=c("a","z","v","t0")  # Sets the names of the parameters
    
    
    tmp=rdiffusion(n=1000,a=currParams["a"],v=currParams["v"],t0=currParams["t0"],z=currParams["z"]*currParams["a"]) # Runs diffusion model to generated data with estimated parameters
    simData$Time=c(simData$Time,tmp$rt) # Populates the RT column in the simulated data
    simData$Resp=c(simData$Resp,tmp$response) # Populates the Resp column in the simulated data 
    simData$Cond=c(simData$Cond,rep(cond,length(tmp$rt)))} # Populates the Cond column in the simulated data
  
  sim = as.data.frame(simData) # Convert the simulated data from List format to data frame format
  
  save(sim, file = paste("Data/dataset3-ef/Model-Predictions/P",useSub,"_simple.RData", sep = ""))

}


