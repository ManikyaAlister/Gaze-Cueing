
rm(list=ls())
setwd("~/Dropbox/2021/Gaze-Cueing/Recovery")

library(msm)
source("02_simulate-DIFF.R")
library(lhs)



#When in the same working directory as the C code, this line will compile the code (first time usage, like installing a package)

# system("R CMD SHLIB DIFF.c")

# Set up hypercube

use.LHS=randomLHS(n=100, k=6) # = n params

colnames(use.LHS)=c("v.centre","v.change",
                    "b","t0.centre","t0.change","z")

use.range=array(NA,c(6,2)) #c(nparams, 2)

rownames(use.range)=c("v.centre","v.change",
                      "b","t0.centre", "t0.change","z")
colnames(use.range)=c("Min","Max")



# Define ranges for parameters for hypercube sampling

use.range["v.centre",]=c(0.1,4)
use.range["v.change",]=c(-2,2)
use.range["b",]=c(0.45,1.75)
use.range["t0.centre",]=c(0.1,0.6)
use.range["t0.change",]=c(-0.2, 0.2)
use.range["z",]=c(0.1,0.9)

for (useParam in colnames(use.LHS)) {
  use.LHS[,useParam]=use.range[useParam,"Min"]+
    use.LHS[,useParam]*(use.range[useParam,"Max"]-use.range[useParam,"Min"])
}


conds=c(-0.5,0.5)



# Simulate all data sets

for (i in 1:nrow(use.LHS)) {
  cat("\n",i,"/",nrow(use.LHS))
  
  # Set up simulated data storage
  data=list(Time=NULL,Resp=NULL,Cond=NULL)
  
  # Set up generating parameters storage
  genParams=array(NA,c(8,2),dimnames=list(c("z","a","ter","v","stoch.s","sz","sv","ster"),conds))
  
  # Loop over conditions
  for (cond in conds) {
    
    # Defining z based on condition
    tmp=NA
    if (cond==-0.5) {
      tmp=as.numeric(use.LHS[i,"z"])
    } else if (cond==0.5) {
      tmp=(1-as.numeric(use.LHS[i,"z"]))
    }
    
    # Define generating parameters for this condition
    genParams[,paste(cond)] = c(tmp,
                                as.numeric(use.LHS[i,"b"]),
                                as.numeric(use.LHS[i,"t0.centre"]) + (cond*as.numeric(use.LHS[i,"t0.change"])),
                                as.numeric(use.LHS[i,"v.centre"]) + (cond*as.numeric(use.LHS[i,"v.change"])),
                                1,0,0,0)
    
    # Actually simulate
    tmp=simulate.DIFF(N=100,params=genParams[,paste(cond)],maxCounter=10000,stepSize=0.001,use.table=use.table,n.table.options=n.table.options)
    
    # Store sim data
    data$Time=c(data$Time,tmp$rt)
    data$Resp=c(data$Resp,tmp$resp)
    data$Cond=c(data$Cond,rep(cond,length(tmp$rt)))
  }
  
  # Save sim data
  save(file=paste("Datasets/RECOVERY_DATA-DIFF_LHS-",i,".Rdata",sep=""),data,genParams,conds)
  
  
}