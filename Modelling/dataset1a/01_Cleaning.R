#### Dataset1a ####

#Load Packages
setwd("~/Documents/2021/Gaze-Cueing")
rm(list=ls())
library(tidyverse)



#Load data
P = read_csv("Data/dataset1-raw/Study1RawData .csv")
P = filter(P, Block == "Main") 
P = filter(P, CueType == "Face") 

#Wrangle and rename variables into a format that DDM script will recognize
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

save(P, file = "Data/dataset1a/clean/all-participants.RData")


for(i in 1:n) {
  PID = paste("P",i, sep ="")
  data =  filter(P, ID == i)
  assign(PID, data)
  save(data, file = paste("Data/dataset1a/clean/P",i,".Rdata",sep=""))
}

