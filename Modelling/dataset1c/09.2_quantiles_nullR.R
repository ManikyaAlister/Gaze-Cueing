## Dataset1c ##


rm(list = ls())
setwd("~/Documents/2021/Gaze-Cueing")
library(tidyverse)
library(jtools)

n_datasets = 1 #number of data sets being run 
dataset_lengths = c(1) #number of participants in each data set
nSub = 41

load("data/dataset1c/derived/participants_null.Rdata")

#for (dataset in 1:n_datasets) {
  
#  nSub = dataset_lengths[dataset]

####### Observed Data ########
all.data=list() 
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/clean/P",useSub,".Rdata",sep=""))
  
  
  all.data[[useSub]]=data
  
}
all.data = all.data[participants_null]


qs=seq(0.1,0.9,0.1) #Define quantiles 

tmp=lapply(all.data,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs)) 

for (s in 1:length(participants_null)) {
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

allQ=array(unlist(tmp),c(length(qs),2,2,length(participants_null)))


tmp=lapply(all.data,function(x) tapply(x$Resp==2,x$Cond,mean))

allP=array(unlist(tmp),c(2,length(participants_null)))

#Means for congruent condition
q.mean.2.1=apply(allQ[,2,1,],1,mean)
p.mean.1=mean(allP[1,])

#Means for incongruent condition
q.mean.2.2=apply(allQ[,2,2,],1,mean) 
p.mean.2=mean(allP[2,])  

############################
###### v_z Model ######
###########################
nsub = 41
all.data_c = list()
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_v-z.RData",sep=""))
  
  all.data_c[[useSub]]=sim
  
}

simData_c=all.data_c
rm(all.data_c)
#theta
tmp=lapply(simData_c,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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

allQ_c=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_c,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_c=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_c=apply(allQ_c[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_c[,2,COND,]...)
p.mean.1_c=mean(allP_c[1,])

#Means for incongruent cue condition
q.mean.2.2_c=apply(allQ_c[,2,2,],1,mean) 
p.mean.2_c=mean(allP_c[2,]) 

#------------------------------
#------ Plotting v_z-------
#------------------------------

#plot(q.mean.2.1,qs*p.mean.2.1,pch=16,xlim=c(0.2,0.5)) 

#all in one 
quantiles_v_z = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_c, y = qs*p.mean.1_c),shape = 1)+
  geom_line(aes(x = q.mean.2.1_c, y = qs*p.mean.1_c))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_c, y = qs*p.mean.2_c),shape = 0)+
  geom_line(aes(x = q.mean.2.2_c, y = qs*p.mean.2_c))+
  theme_apa()
quantiles_v_z #View plot 
ggsave("Modelling/dataset1c/08_Plots/quantiles-v-z-null.png", plot = quantiles_v_z)

#Separated across conditions
ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_c, y = qs*p.mean.1_c),shape = 1)+
  geom_line(aes(x = q.mean.2.1_c, y = qs*p.mean.1_c))+
  theme_apa()

ggplot()+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_c, y = qs*p.mean.2_c),shape = 0)+
  geom_line(aes(x = q.mean.2.2_c, y = qs*p.mean.2_c))+
  theme_apa()


###########################
######### Z Model #########
###########################


# Load in only the predicted predicted data generated from z DDM (simData)
nsub = 41
all.data_z = list()

for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_z.RData",sep=""))
  
  all.data_z[[useSub]]=sim
  
}
simData_z=all.data_z
rm(all.data_z)

tmp=lapply(simData_z,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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

allQ_z=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_z,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_z=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_z=apply(allQ_z[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_z[,2,COND,]...)
p.mean.1_z=mean(allP_z[1,])

#Means for incongruent cue condition
q.mean.2.2_z=apply(allQ_z[,2,2,],1,mean) 
p.mean.2_z=mean(allP_z[2,]) 

#------------------------------
#----------- Plotting Z--------
#------------------------------
quantiles_z = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_z, y = qs*p.mean.1_z),shape = 1)+
  geom_line(aes(x = q.mean.2.1_z, y = qs*p.mean.1_z))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_z, y = qs*p.mean.2_z),shape = 0)+
  geom_line(aes(x = q.mean.2.2_z, y = qs*p.mean.2_z))+
  labs(title = "Arrow z") +
  theme_apa()
quantiles_z #view plot 
ggsave("Modelling/dataset1c/08_Plots/quantiles-z-null.png", plot = quantiles_z)  

###########################
######### V Model #########
###########################

nsub = 41
all.data_v = list()
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_v.RData",sep=""))
  
  all.data_v[[useSub]]=sim
  
}
simData_v=all.data_v
rm(all.data_v)

tmp=lapply(simData_v,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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

allQ_v=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simData_v,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_v=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_v =apply(allQ_v[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_v[,2,COND,]...)
p.mean.1_v =mean(allP_v[1,])

#Means for incongruent cue condition
q.mean.2.2_v=apply(allQ_v[,2,2,],1,mean) 
p.mean.2_v=mean(allP_v[2,]) 

#------------------------------
#----------- Plotting V--------
#------------------------------
quantiles_v = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_v, y = qs*p.mean.1_v),shape = 1)+
  geom_line(aes(x = q.mean.2.1_v, y = qs*p.mean.1_v))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_v, y = qs*p.mean.2_v),shape = 0)+
  geom_line(aes(x = q.mean.2.2_v, y = qs*p.mean.2_v))+
  labs(title = "Arrow v")+
  theme_apa()
quantiles_v # view plot
ggsave("Modelling/dataset1c/08_Plots/quantiles-v-null.png", plot = quantiles_v)

###########################
######### t0 Model #########
###########################


# Load in predicted data generated from t0 DDM (simData)
nsub = 41
all.data_t0 = list()
dataset = 1 
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_t0.RData",sep=""))
  
  all.data_t0[[useSub]]=sim
  
}
 
simData_t0=all.data_t0

simData_t0_null = simData_t0[participants_null]
simData_t0 = simData_t0[participants_null]

rm(all.data_t0)

tmp=lapply(simData_t0,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

for (s in 1:length(participants_null)) {
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

allQ_t0=array(unlist(tmp),c(length(qs),2,2,length(participants_null)))

tmp=lapply(simData_t0,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_t0=array(unlist(tmp),c(2,length(participants_null)))

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
  labs(title = "Arrow t0 on null model data")+
  theme_apa()
quantiles_t0 # view plot
ggsave(filename = paste("Modelling/dataset1c/08_Plots/quantiles-t0-null.png", sep = ""), plot = quantiles_t0)
ggsave("Modelling/dataset1c/08_Plots/quantiles-t0.png")

###########################
######### v-t0 Model #########
###########################


# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_v_t0 = list()
dataset = 1 
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_v-t0.RData",sep=""))
  
  all.data_v_t0[[useSub]]=sim
  
}

simData_v_t0=all.data_v_t0
rm(all.data_v_t0)

tmp=lapply(simData_v_t0,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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
  labs(title = "Arrow v-t0")+
  theme_apa()
quantiles_v_t0 # view plot

ggsave("Modelling/dataset1c/08_Plots/quantiles-v-t0-null.png", plot = quantiles_v_t0)


###########################
######### z-t0 Model #########
###########################


# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_z_t0 = list()
dataset = 1 
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_z-t0.RData",sep=""))
  
  all.data_z_t0[[useSub]]=sim
  
}

simData_z_t0=all.data_z_t0
rm(all.data_z_t0)

tmp=lapply(simData_z_t0,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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
  labs(title = "Arrow z-t0")+
  theme_apa()
quantiles_z_t0 # view plot
ggsave(filename = paste("Modelling/dataset1c/08_Plots/quantiles_z-t0-null.png", sep = ""), plot = quantiles_z_t0)


###################################
####### Complex Model ######
###################################


# Load in predicted data generated from z DDM (simData)
nsub = 41
all.dataComplex= list()
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_complex.RData",sep=""))
  
  all.dataComplex[[useSub]]=sim
  
}
#Something wrong with how I generated the Z data 
simDataComplex=all.dataComplex
rm(all.dataComplex)

tmp=lapply(simDataComplex,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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

allQComplex=array(unlist(tmp),c(length(qs),2,2,41))

tmp=lapply(simDataComplex,function(x) tapply(x$Resp==2,x$Cond,mean))

allPComplex=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1Complex=apply(allQComplex[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQComplex[,2,COND,]...)
p.mean.1Complex=mean(allPComplex[1,])

#Means for incongruent cue condition
q.mean.2.2Complex=apply(allQComplex[,2,2,],1,mean) 
p.mean.2Complex=mean(allPComplex[2,]) 

#--------------------------------------
#---- Plotting Complex Model----
#--------------------------------------

#plot(q.mean.2.1,qs*p.mean.2.1,pch=16,xlim=c(0.2,0.5)) 
#all in one 
quantiles_complex = ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1Complex, y = qs*p.mean.1Complex),shape = 1)+
  geom_line(aes(x = q.mean.2.1Complex, y = qs*p.mean.1Complex))+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2Complex, y = qs*p.mean.2Complex),shape = 0)+
  geom_line(aes(x = q.mean.2.2Complex, y = qs*p.mean.2Complex))+
  labs(title = "Arrow All Paramaters Model")+
  theme_apa()
quantiles_complex #view plot
ggsave("Modelling/dataset1c/08_Plots/quantiles-complex-null.png", plot = quantiles_complex)

###########################
####### Simple Model ######
###########################

# Load in predicted data generated from z DDM (simData)
nsub = 41
all.data_s = list()
for (useSub in 1:nSub) {
  
  load(paste("Data/dataset1c/Model-Predictions/P",useSub,"_simple.RData",sep=""))
  
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
  labs(title = "Arrow Simple Model")+
  theme_apa()
quantiles_simple #view plot
ggsave("Modelling/dataset1c/08_Plots/quantiles-simple-null.png", plot = quantiles_simple)
#Separated across conditions

ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.1))+
  geom_point(aes(x = q.mean.2.1_s, y = qs*p.mean.1_s),shape = 1)+
  geom_line(aes(x = q.mean.2.1_s, y = qs*p.mean.1_s))+
  labs(title = "Arrow Simple", subtitle =  "Congruent")
  theme_apa()

ggplot()+
  geom_point(aes(x = q.mean.2.2, y = qs*p.mean.2), shape = 15)+
  geom_point(aes(x = q.mean.2.2_s, y = qs*p.mean.2_s),shape = 0)+
  geom_line(aes(x = q.mean.2.2_s, y = qs*p.mean.2_s))+
  theme_apa()


