#Load Packages
setwd("~/cloudstor/2021/Gaze-Cueing")
rm(list=ls())
library(tidyverse)



#Load data
P = read_csv("Data/dataset1-raw/Study1RawData .csv")
P = filter(P, Block == "Main")

P = filter(P, CueType == "Arrow") 

original.length = length(P$Trial)

#Wrangle and rename variables into a format that Nathan's DDM script will recognise
P = cbind(ID = P$ParticipantID,
Validity = P$Validity,
Time = P$ReactionTime/1000,
Stim = P$CueDirection,
Resp = P$Accuracy + 1) 

P = as.data.frame(P) 
P$ID = as.numeric(P$ID) 
P = mutate(P, Cond = case_when(Validity == "Valid" ~ 1, 
                           Validity == "Invalid" ~ 2))
P = filter(P, Time < 5) #Filter out responses slower than 5 seconds (8 trials)
P = filter(P, Time > 0.1) #Filter out responses faster than 0.1 seconds (12 trials)

P$Time=as.numeric(P$Time)
P$Resp=as.numeric(P$Resp)
n = max(P$ID)

filtered.length = length(P$Time)
trials.rm = length(P$Time) - original.length
trials.rm.percent = trials.rm/filtered.length #percentage of trials excluded 9,819


save(P, file = "Data/dataset1b/clean/all-participants.RData")


for(i in 1:n) {
  PID = paste("P",i, sep ="")
  data =  filter(P, ID == i)
  assign(PID, data)
  save(data, file = paste("Data/dataset1b/clean/P",i,".Rdata",sep=""))
}

