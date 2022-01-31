#### Dataset2 ####

#Load Packages
setwd("~/cloudstor/2021/Gaze-Cueing")
rm(list=ls())
library(tidyverse)



#Load data
data = read_csv("Data/Carlson-2016/Carlson 2016 Raw Data.csv")

original.length = length(data$Subject)

P = filter(data, TrialType != "Direct") #Remove direct (forward facing) trials

filtered.length = length(P$Subject) #n trials after filtering direct faces

#Wrangle and rename variables into a format that DDM script will recognize
P= cbind(ID = P$Subject,
Validity = P$TrialType,
Time = P$Target.RT/1000,
Stim = P$CellLabel,
Resp = P$Target.ACC + 1) 

P = as.data.frame(P)

#Convert "Stim" so that it is the direction of the gaze. 
#CellLabel collumn according to the author: 
#T CellLabel indicates the Expression (F or N; fearful or neutral), Gaze Direction (L, R, or 
#D; left, right, or direct), and target location (l or r; left or right) <-- I'm assuming 
#that "i" or "c" as the last letter means congruent or incongruent trial (it maps onto trialtype)

stim = P$Stim

stim[stim == "FRi"] = "Right"
stim[stim == "FRc"] = "Right"
stim[stim == "FRr"] = "Right"
stim[stim == "FRl"] = "Right"

stim[stim == "NRi"] = "Right"
stim[stim == "NRc"] = "Right"
stim[stim == "NRr"] = "Right"
stim[stim == "NRl"] = "Right"

stim[stim == "FLi"] = "Left"
stim[stim == "FLc"] = "Left"
stim[stim == "FLr"] = "Left"
stim[stim == "FLl"] = "Left"

stim[stim == "NLi"] = "Left"
stim[stim == "NLc"] = "Left"
stim[stim == "NLr"] = "Left"
stim[stim == "NLl"] = "Left"

P$Stim = stim


#Make  Binary condition "Cond": 1 = congruent, 2 = incongruent

P = mutate(P, Cond = case_when(Validity == "Congruent" ~ 2, 
                               Validity == "Inongruent" ~ 1))

#According to author, raw data starts at 4 because first three participants were
# pilots. Next code will just arrange so that ID starts at 1 instead of 4
P$ID = as.numeric(P$ID)
P$ID = P$ID-3
P$ID[P$ID>36]=P$ID[P$ID>36]-2 #No data for transformed 35, 36
P$ID[P$ID>43]=P$ID[P$ID>43]-1 #No data for double transformed 43 (3 participants' data missing)

#Data exclusion

P = filter(P, Time < 5) #Filter out responses slower than 5 seconds (8 trials)
P = filter(P, Time > 0.1) #Filter out responses faster than 0.1 seconds (12 trials)

P$Time=as.numeric(P$Time)
P$Resp=as.numeric(P$Resp)
n = max(P$ID)

trials_post = length(P$Time)
trials.rm = filtered.length-trials_post #no. of trials excluded
trials.rm.percent = trials.rm/filtered.length #percentage of trials excluded 


save(P, file = "Data/dataset2/clean/all-participants.RData")


for(i in 1:n) {
  PID = paste("P",i, sep ="")
  data =  filter(P, ID == i)
  assign(PID, data)
  accurate_trials = filter(data, Resp == 2)
  n.accurate = length(accurate_trials$Resp)
  acc = n.accurate/length(data$Resp)
  if (acc < .8) {
    save(data, file = paste("Data/dataset3/clean/rm-accuracy/P",i,".Rdata",sep=""))
  } else {
    data = filter(data, Time < 5) #Filter out responses slower than 5 seconds 
    data = filter(data, Time > 0.1)#Filter out responses faster than 0.1 seconds 
    save(data, file = paste("Data/dataset3/clean/P",i,".Rdata",sep=""))
  }
  save(data, file = paste("Data/dataset2/clean/P",i,".Rdata",sep=""))
}
