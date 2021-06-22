## What I need to do
# Sort out why the observed data is the same
# Re-run the z model to get the correct fits 

# person 29 bad data 

rm(list = ls())
setwd("~/Dropbox/2021/Gaze-Cueing")
library(tidyverse)
library(jtools)

load("data/participants_alt.Rdata") #This is a list of the participants who were less than 
# 50% likely for the null model to be their best model
n_datasets = 1 #number of data sets being run 
dataset_lengths = c(1) #number of participants in each data set
nSub = 41
#for (dataset in 1:n_datasets) {
  
#  nSub = dataset_lengths[dataset]

####### Observed Data ########
all.data=list() 
for (useSub in 1:nSub) {
  
  load(paste("Data/Gregory-and-Jackson-(2020)/P",useSub,".Rdata",sep=""))
  
  
  all.data[[useSub]]=data
  
}

all.data = all.data[participants_alt] #only interested in participants that didn't overwhelmingly favor the null model

qs=seq(0.1,0.9,0.1) #Define quantiles 

tmp=lapply(all.data,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs)) 

for (s in 1:length(participants_alt)) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ=array(unlist(tmp),c(length(qs),2,2,length(participants_alt)))


tmp=lapply(all.data,function(x) tapply(x$Resp==2,x$Cond,mean))

allP=array(unlist(tmp),c(2,length(participants_alt)))

#Means for congruent condition
q.mean.2.1=apply(allQ[,2,1,],1,mean)
p.mean.1=mean(allP[1,])

#Means for incongruent condition
q.mean.2.2=apply(allQ[,2,2,],1,mean) 
p.mean.2=mean(allP[2,])  

############################
###### Z-V Model ######
###########################
nsub = 41
all.data_z_v = list()
for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS",dataset,"_P",useSub,"_complex.RData",sep=""))
  
  all.data_z_v[[useSub]]=sim
  
}

simData_z_v=all.data_z_v[participants_alt]
rm(all.data_z_v)
#theta
tmp=lapply(simData_z_v,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:length(participants_alt)) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_z_v=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_z_v,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_z_v=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_z_v=apply(allQ_z_v[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_z_v[,2,COND,]...)
p.mean.1_z_v=mean(allP_z_v[1,])

#Means for incongruent cue condition
q.mean.2.2_z_v=apply(allQ_z_v[,2,2,],1,mean) 
p.mean.2_z_v=mean(allP_z_v[2,]) 

#------------------------------
#------ Plotting Complex-------
#------------------------------

#all in one 
quantiles_z_v = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_z_v, y = qs*p.mean.1_z_v),shape = 1)+
  geom_line(aes(x = q.mean.2.1_z_v, y = qs*p.mean.1_z_v))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_z_v, y = qs*p.mean.2_z_v),shape = 0)+
  geom_line(aes(x = q.mean.2.2_z_v, y = qs*p.mean.2_z_v))+
  theme_apa()
quantiles_z_v #View plot 
ggsave(paste("Modelling/07_Plots/DS",dataset,"_quantiles-z-v-alt.png", sep = ""), plot = quantiles_complex)

###########################
######### Z Model #########
###########################

# Load in only the predicted data generated from z DDM (simData)
nsub = 41
all.data_z = list()

for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS",dataset,"_P",useSub,"_z.RData",sep=""))
  
  all.data_z[[useSub]]=sim
  
}
simData_z=all.data_z[participants_alt]
rm(all.data_z)

tmp=lapply(simData_z,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in length(participants_alt)) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_z=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_z,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_z=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_z=apply(allQ_z[,2,1,],1,mean) 
p.mean.1_z=mean(allP_z[1,])

#Means for incongruent cue condition
q.mean.2.2_z=apply(allQ_z[,2,2,],1,mean) 
p.mean.2_z=mean(allP_z[2,]) 

#------------------------------
#----------- Plotting Z--------
#------------------------------
#all in one 
quantiles_z = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_z, y = qs*p.mean.1_z),shape = 1)+
  geom_line(aes(x = q.mean.2.1_z, y = qs*p.mean.1_z))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_z, y = qs*p.mean.2_z),shape = 0)+
  geom_line(aes(x = q.mean.2.2_z, y = qs*p.mean.2_z))+
  labs(title = "z") +
  theme_apa()
quantiles_z #view plot 
ggsave(paste("Modelling/07_Plots/DS",dataset,"_quantiles-z-alt.png", sep = ""), plot = quantiles_z)  


###########################
######### V Model #########
###########################

nsub = 41
all.data_v = list()
for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS", dataset, "_P",useSub,"_v.RData",sep=""))
  
  all.data_v[[useSub]]=sim
  
}
#Something wrong with how I generated the Z data 
simData_v=all.data_v[participants_alt]
rm(all.data_v)

tmp=lapply(simData_v,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in length[participants_alt]) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_v=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_v,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_v=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_v=apply(allQ_v[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_v[,2,COND,]...)
p.mean.1_v=mean(allP_v[1,])

#Means for incongruent cue condition
q.mean.2.2_v=apply(allQ_v[,2,2,],1,mean) 
p.mean.2_v=mean(allP_v[2,]) 

#------------------------------
#----------- Plotting V--------
#------------------------------

#all in one 
quantiles_v = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_v, y = qs*p.mean.1_v),shape = 1)+
  geom_line(aes(x = q.mean.2.1_v, y = qs*p.mean.1_v))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_v, y = qs*p.mean.2_v),shape = 0)+
  geom_line(aes(x = q.mean.2.2_v, y = qs*p.mean.2_v))+
  labs(title = "v")+
  theme_apa()
quantiles_v # view plot
ggsave(paste("Modelling/07_Plots/DS",dataset,"_quantiles-v-alt.png", sep = ""), plot = quantiles_v)


###########################
######### t0 Model #########
###########################


# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_t0 = list()
dataset = 1 
for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS", dataset, "_P",useSub,"_t0.RData",sep=""))
  
  all.data_t0[[useSub]]=sim
  
}
 
simData_t0=all.data_t0[participants_alt]
rm(all.data_t0)

tmp=lapply(simData_t0,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:length(participants_alt)) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_t0=array(unlist(tmp),c(length(qs),2,2,length(participants_alt)))

tmp=lapply(simData_t0,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_t0=array(unlist(tmp),c(2,length(participants_alt)))

#Means for congruent cue condition
q.mean.2.1_t0=apply(allQ_t0[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_t0[,2,COND,]...)
p.mean.1_t0=mean(allP_t0[1,])

#Means for incongruent cue condition
q.mean.2.2_t0=apply(allQ_t0[,2,2,],1,mean) 
p.mean.2_t0=mean(allP_t0[2,]) 

#------------------------------
#----------- Plotting t0--------
#------------------------------

#all in one 
quantiles_t0 = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_t0, y = qs*p.mean.1_t0),shape = 1)+
  geom_line(aes(x = q.mean.2.1_t0, y = qs*p.mean.1_t0))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_t0, y = qs*p.mean.2_t0),shape = 0)+
  geom_line(aes(x = q.mean.2.2_t0, y = qs*p.mean.2_t0))+
  labs(title = "t0")+
  theme_apa()
quantiles_t0 # view plot
ggsave(filename = paste("Modelling/08_Plots/DS",dataset,"_quantiles-t0-alt.png", sep = ""), plot = quantiles_t0)
ggsave("Modelling/07_Plots/DS1_quantiles-t0-alt.png")

###########################
######### v-t0 Model #########
###########################


# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_v_t0 = list()
dataset = 1 
for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS", dataset, "_P",useSub,"_v_t0.RData",sep=""))
  
  all.data_v_t0[[useSub]]=sim
  
}

simData_v_t0=all.data_v_t0[participants_alt]
rm(all.data_v_t0)

tmp=lapply(simData_v_t0,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:length[participants_alt]) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_v_t0=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_v_t0,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_v_t0=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_v_t0=apply(allQ_v_t0[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_v0_t0[,2,COND,]...)
p.mean.1_v_t0=mean(allP_v_t0[1,])

#Means for incongruent cue condition
q.mean.2.2_v_t0=apply(allQ_v_t0[,2,2,],1,mean) 
p.mean.2_v_t0=mean(allP_v_t0[2,]) 

#------------------------------
#----------- Plotting v-t0-----
#------------------------------

#all in one 
quantiles_v_t0 = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_v_t0, y = qs*p.mean.1_v_t0),shape = 1)+
  geom_line(aes(x = q.mean.2.1_v_t0, y = qs*p.mean.1_v_t0))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_v_t0, y = qs*p.mean.2_v_t0),shape = 0)+
  geom_line(aes(x = q.mean.2.2_v_t0, y = qs*p.mean.2_v_t0))+
  labs(title = "v-t0")+
  theme_apa()
quantiles_v_t0 # view plot
ggsave(filename = paste("Modelling/08_Plots/DS",dataset,"_quantiles_v-t0-alt.png", sep = ""), plot = quantiles_v_t0)
ggsave("Modelling/07_Plots/DS1_quantiles-t0-alt.png")


###########################
######### z-t0 Model #########
###########################


# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_z_t0 = list()
dataset = 1 
for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS", dataset, "_P",useSub,"_z-t0.RData",sep=""))
  
  all.data_z_t0[[useSub]]=sim
  
}

simData_z_t0=all.data_z_t0[participants_alt]
rm(all.data_z_t0)

tmp=lapply(simData_z_t0,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:length(participants_alt)) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_z_t0=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_z_t0,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_z_t0=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_z_t0=apply(allQ_z_t0[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_v0_t0[,2,COND,]...)
p.mean.1_z_t0=mean(allP_z_t0[1,])

#Means for incongruent cue condition
q.mean.2.2_z_t0=apply(allQ_z_t0[,2,2,],1,mean) 
p.mean.2_z_t0=mean(allP_z_t0[2,]) 

#------------------------------
#----------- Plotting z-t0-----
#------------------------------

#all in one 
quantiles_z_t0 = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_z_t0, y = qs*p.mean.1_z_t0),shape = 1)+
  geom_line(aes(x = q.mean.2.1_z_t0, y = qs*p.mean.1_z_t0))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_z_t0, y = qs*p.mean.2_z_t0),shape = 0)+
  geom_line(aes(x = q.mean.2.2_z_t0, y = qs*p.mean.2_z_t0))+
  labs(title = "z-t0")+
  theme_apa()
quantiles_z_t0 # view plot
ggsave(filename = paste("Modelling/08_Plots/DS",dataset,"_quantiles_z-t0.png", sep = ""), plot = quantiles_z_t0)


###################################
####### All Parameters Model ######
###################################


# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_ap= list()
for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS",dataset,"_P",useSub,"_all-params.RData",sep=""))
  
  all.data_ap[[useSub]]=sim
  
}
#Something wrong with how I generated the Z data 
simData_ap=all.data_ap
rm(all.data_ap)

tmp=lapply(simData_ap,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:41) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_ap=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_ap,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_ap=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_ap=apply(allQ_ap[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_ap[,2,COND,]...)
p.mean.1_ap=mean(allP_ap[1,])

#Means for incongruent cue condition
q.mean.2.2_ap=apply(allQ_ap[,2,2,],1,mean) 
p.mean.2_ap=mean(allP_ap[2,]) 

#--------------------------------------
#---- Plotting All Parameters Model----
#--------------------------------------

#plot(q.mean.2.1,qs*p.mean.2.1,pch=16,xlim=c(0.2,0.5)) 
#all in one 
quantiles_all_params = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_ap, y = qs*p.mean.1_ap),shape = 1)+
  geom_line(aes(x = q.mean.2.1_ap, y = qs*p.mean.1_ap))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_ap, y = qs*p.mean.2_ap),shape = 0)+
  geom_line(aes(x = q.mean.2.2_ap, y = qs*p.mean.2_ap))+
  labs(title = "All Paramaters Model")+
  theme_apa()
quantiles_all_params #view plot
ggsave(paste("Modelling/08_Plots/DS",dataset,"_quantiles-all-params.png", sep = ""), plot = quantiles_all_params)

###########################
####### Simple Model ######
###########################

# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_s = list()
for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/P",useSub,"_simple.RData",sep=""))
  
  all.data_s[[useSub]]=sim
  
}
#Something wrong with how I generated the Z data 
simData_s=all.data_s
rm(all.data_s)

tmp=lapply(simData_s,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:41) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_s=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_s,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_s=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_s=apply(allQ_s[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_s[,2,COND,]...)
p.mean.1_s=mean(allP_s[1,])

#Means for incongruent cue condition
q.mean.2.2_s=apply(allQ_s[,2,2,],1,mean) 
p.mean.2_s=mean(allP_s[2,]) 

#------------------------------
#---- Plotting Simple Model----
#------------------------------

#plot(q.mean.2.1,qs*p.mean.2.1,pch=16,xlim=c(0.2,0.5)) 
#all in one 
quantiles_simple = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_s, y = qs*p.mean.1_s),shape = 1)+
  geom_line(aes(x = q.mean.2.1_s, y = qs*p.mean.1_s))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_s, y = qs*p.mean.2_s),shape = 0)+
  geom_line(aes(x = q.mean.2.2_s, y = qs*p.mean.2_s))+
  labs(title = "Simple Model")+
  theme_apa()
quantiles_simple #view plot
ggsave(paste("Modelling/07_Plots/DS",dataset,"_quantiles-simple.png", sep = ""), plot = quantiles_simple)
#Separated across conditions

ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_s, y = qs*p.mean.1_s),shape = 1)+
  geom_line(aes(x = q.mean.2.1_s, y = qs*p.mean.1_s))+
  labs(title = "Simple", subtitle =  "Congruent")
  theme_apa()

ggplot()+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_s, y = qs*p.mean.2_s),shape = 0)+
  geom_line(aes(x = q.mean.2.2_s, y = qs*p.mean.2_s))+
  theme_apa()

###########################
## Z Interference Model ###
###########################


# Load in only the predicted predicted data generated from zinter DDM (simData_zinter)
nsub = 41
all.data_zinter = list()

for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS",dataset,"_P",useSub,"_zinter.RData",sep=""))
  
  all.data_zinter[[useSub]]=sim
  
}
simData_zinter=all.data_zinter
rm(all.data_zinter)

tmp=lapply(simData_zinter,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:41) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}

allQ_zinter=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_zinter,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_zinter=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_zinter=apply(allQ_zinter[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_zinter[,2,COND,]...)
p.mean.1_zinter=mean(allP_zinter[1,])

#Means for incongruent cue condition
q.mean.2.2_zinter=apply(allQ_zinter[,2,2,],1,mean) 
p.mean.2_zinter=mean(allP_zinter[2,]) 

#------------------------------
#--- Plotting Z interference---
#------------------------------

#all in one 
quantiles_zinter = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_zinter, y = qs*p.mean.1_zinter),shape = 1)+
  geom_line(aes(x = q.mean.2.1_zinter, y = qs*p.mean.1_zinter))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_zinter, y = qs*p.mean.2_zinter),shape = 0)+
  geom_line(aes(x = q.mean.2.2_zinter, y = qs*p.mean.2_zinter))+
  ggtitle("Z Interference")+
  theme_apa()
quantiles_zinter #view plot
ggsave(paste("Modelling/07_Plots/DS",dataset,"_quantiles-zinter.png", sep = ""), plot = quantiles_zinter)

#Separated across conditions

ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_zinter, y = qs*p.mean.1_zinter),shape = 1)+
  geom_line(aes(x = q.mean.2.1_zinter, y = qs*p.mean.1_zinter))+
  labs(title = "z Interference", subtitle =  "Congruent")+
  theme_apa()

ggplot()+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_zinter, y = qs*p.mean.2_zinter),shape = 0)+
  geom_line(aes(x = q.mean.2.2_zinter, y = qs*p.mean.2_zinter))+
  labs(title = "z Interference", subtitle =  "Incongruent")+
  theme_apa()
###########################
## Z Facilitation Model ##
###########################


# Load in only the predicted \ data generated from zfacil DDM (simData_zfacil)
nsub = 41
all.data_zfacil = list()

for (useSub in 1:nSub) {
  
  load(paste("Data/Model-Predictions/DS", dataset, "_P",useSub,"_zfacil.RData",sep=""))
  
  all.data_zfacil[[useSub]]=sim
  
}
simData_zfacil=all.data_zfacil
rm(all.data_zfacil)

tmp=lapply(simData_zfacil,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:41) {
  if (nrow(tmp[[s]])==1) {
    tmp[[s]]=rbind(list(rep(NA,length(qs)),rep(NA,length(qs))),tmp[[s]])
    rownames(tmp[[s]])=c(1,2)
  }
  for (i in 1:2) {
    for (j in 1:2) {
      if (is.null(tmp[[s]][paste(i),paste(j)][[1]])) tmp[[s]][paste(i),paste(j)][[1]]=rep(NA,length(qs))
    }
  }
}


allQ_zfacil=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_zfacil,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_zfacil=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_zfacil=apply(allQ_zfacil[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_zfacil[,2,COND,]...)
p.mean.1_zfacil=mean(allP_zfacil[1,])

#Means for incongruent cue condition
q.mean.2.2_zfacil=apply(allQ_zfacil[,2,2,],1,mean) 
p.mean.2_zfacil=mean(allP_zfacil[2,]) 

#------------------------------
#--- Plotting Z Facilitation---
#------------------------------

#all in one 
quantiles_zfacil = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_zfacil, y = qs*p.mean.1_zfacil),shape = 1)+
  geom_line(aes(x = q.mean.2.1_zfacil, y = qs*p.mean.1_zfacil))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_zfacil, y = qs*p.mean.2_zfacil),shape = 0)+
  geom_line(aes(x = q.mean.2.2_zfacil, y = qs*p.mean.2_zfacil))+
  labs(title = "z Facilitation")+
  theme_apa()
quantiles_zfacil #view plot
gggsave(paste("Modelling/07_Plots/DS",dataset,"_quantiles-zfacil.png", sep = ""), plot = quantiles_zfacil)

#Separated across conditions
ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_zfacil, y = qs*p.mean.1_zfacil),shape = 1)+
  geom_line(aes(x = q.mean.2.1_zfacil, y = qs*p.mean.1_zfacil))+
  labs(title = "z Facilitation", subtitle =  "Congruent")+
  theme_apa()

ggplot()+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_zfacil, y = qs*p.mean.2_zfacil),shape = 0)+
  geom_line(aes(x = q.mean.2.2_zfacil, y = qs*p.mean.2_zfacil))+
  labs(title = "z Facilitation", subtitle = "Incongruent")+
  theme_apa()

#}


# > tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,c(0.1,0.3,0.5,0.7,0.9))[2,1][[1]]
# 10%       30%       50%       70%       90% 
# 0.2485487 0.2835867 0.3152755 0.3599591 0.4410864 
# > tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,c(0.1,0.3,0.5,0.7,0.9))[2,2][[1]]
# 10%       30%       50%       70%       90% 
# 0.2461466 0.2960602 0.3376226 0.3861654 0.4776514 

#qs=c(0.1,0.3,0.5,0.7,0.9)

#tmp=tapply(P1$Time,list(P1$Resp,P1$Cond),quantile,qs)[2,1][[1]]

#tmp1=tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,qs)[2,1][[1]]
#tmp2=tapply(sim_matrix_simple$Time,list(sim_matrix_simple$Resp,sim_matrix_simple$Cond),quantile,qs)[2,1][[1]]

#plot(tmp,qs*mean(P1$Resp[P1$Cond==1]==2),pch=16,xlim=c(0.2,0.5))
#points(tmp1,qs*mean(sim_matrix_complex$Resp[sim_matrix_complex$Cond==1]==2),pch=5)
#points(tmp2,qs*mean(sim_matrix_simple$Resp[sim_matrix_simple$Cond==1]==2),pch=6)



#tmp=tapply(P1$Time,list(P1$Resp,P1$Cond),quantile,qs)[2,2][[1]]

#tmp1=tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,qs)[2,2][[1]]
#tmp2=tapply(sim_matrix_simple$Time,list(sim_matrix_simple$Resp,sim_matrix_simple$Cond),quantile,qs)[2,2][[1]]

#plot(tmp,qs*mean(P1$Resp[P1$Cond==2]==2),pch=16,xlim=c(0.2,0.5))
#points(tmp1,qs*mean(sim_matrix_complex$Resp[sim_matrix_complex$Cond==2]==2),pch=5)
#points(tmp2,qs*mean(sim_matrix_simple$Resp[sim_matrix_simple$Cond==2]==2),pch=6)






