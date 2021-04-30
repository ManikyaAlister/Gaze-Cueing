rm(list = ls())
setwd("~/Dropbox/2021/Gaze-Cueing")
library(tidyverse)
library(jtools)
nSub = 41
all.data=list()

####### Observed Data ########
for (useSub in 1:nSub) {
  
  load(paste("Data/Gregory-and-Jackson-(2020)/P",useSub,".Rdata",sep=""))

  
  all.data[[useSub]]=data
  
}
rm(data)


# > tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,c(0.1,0.3,0.5,0.7,0.9))[2,1][[1]]
# 10%       30%       50%       70%       90% 
# 0.2485487 0.2835867 0.3152755 0.3599591 0.4410864 
# > tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,c(0.1,0.3,0.5,0.7,0.9))[2,2][[1]]
# 10%       30%       50%       70%       90% 
# 0.2461466 0.2960602 0.3376226 0.3861654 0.4776514 

qs=seq(0.1,0.9,0.1) #Define quantiles

tmp=lapply(all.data,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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

#tmdata=tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,qs)[2,1][[1]]
#tmp2=tapply(sim_matrix_simple$Time,list(sim_matrix_simple$Resp,sim_matrix_simple$Cond),quantile,qs)[2,1][[1]]

allQ=array(unlist(tmp),c(length(qs),2,2,41))


tmp=lapply(all.data,function(x) tapply(x$Resp==2,x$Cond,mean))

allP=array(unlist(tmp),c(2,41))

#Means for congruent condition
q.mean.2.1=apply(allQ[,2,1,],1,mean)
p.mean.2.1=mean(allP[1,])

#Means for incongruent condition
q.mean.2.2=apply(allQ[,2,2,],1,mean) 
p.mean.1.1=mean(allP[2,]) #I'm not sure if this is indexed properly. See below. 

########### Redo everything for simData (simulated data) ###########

# Load in predicted data generated from complex DDM (simData)
load("Data/Hier_model_predictions/complex.RData")


tmp=lapply(simData,function(x) tapply(x$Time,list(x$Resp,x$Cond),quantile,qs))

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

#tmdata=tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,qs)[2,1][[1]]
#tmp2=tapply(sim_matrix_simple$Time,list(sim_matrix_simple$Resp,sim_matrix_simple$Cond),quantile,qs)[2,1][[1]]

allQ_sim=array(unlist(tmp),c(length(qs),2,2,41))


tmp=lapply(simData,function(x) tapply(x$Resp==2,x$Cond,mean))

allP_sim=array(unlist(tmp),c(2,41))

#Means for congruent cue condition
q.mean.2.1_Sim=apply(allQ_sim[,2,1,],1,mean) #Cond == 1 is congruent cues (from allQ_sim[,2,COND,]...)
p.mean.2.1_Sim=mean(allP_sim[1,])

#Means for incongruent cue condition
q.mean.2.2_Sim=apply(allQ_sim[,2,2,],1,mean) 
p.mean.1.1_Sim=mean(allP_sim[2,]) #I'm not entirely sure if this indexing is correct but I think indexing 
# AllP_sim with 1,1 gives us the mean of the incongruent cue condition instead of 2,2. 2,2 gives the same
# value as 2, 1 which makes me think it is for inaccurate trials (to which there are basically none)

#------------------------------#
#----------- Plotting ---------#
#------------------------------#

###############################
### Congruent Cue Condition ###
###############################
#This lets us see the quantiles of the congruent cues for the observed data:
plot(q.mean.2.1,qs*p.mean.2.1,pch=16,xlim=c(0.2,0.5)) 

#But I want to compare the observed data against the simulated data

ggplot()+
  geom_point(aes(x = q.mean.2.1, y = qs*p.mean.2.1))+
  geom_point(aes(x = q.mean.2.1_Sim, y = qs*p.mean.2.1_Sim),shape = 17)+
  geom_line(aes(x = q.mean.2.1_Sim, y = qs*p.mean.2.1_Sim))+
  theme_apa()

#The above works but in the data's current format, it's hard to create a legend. 

#Wrangle the data so that is is in a good format for plotting
#Overall goal is to combine q.mean.2.1 and q.mean.2.1_Sim into one data frame with a grouping variable

#Start with the observed data
q.mean = as.data.frame(q.mean.2.1) #Make q.mean.2.1 a data frame called q.mean
q.mean$p.mean = qs*p.mean.2.1 #Make a qs*p.means column for plotting 
q.mean$data = "Observed" #Add an extra column so that we can identify which data the adjacent column belongs to (grouping variable)
colnames(q.mean) = c("q.means", "p.means", "Data") #Add column names so that they're the same for both simulated and observed data. 

#Now do it for simulated data
q.mean.sim = as.data.frame(q.mean.2.1_Sim)
q.mean.sim$p.mean = qs*p.mean.2.1_Sim
q.mean.sim$data = "Complex Model" 
colnames(q.mean.sim) = c("q.means","p.means", "Data")

#Combine the simulated and observed data into one data frame
q.means = rbind(q.mean, q.mean.sim)

#Now let's plot again
ggplot(q.means)+
  geom_point(aes(x = q.means, y = p.means, shape = Data))+
  ylab("p.means*qs")+
  labs(title = "Congruent")+
  theme_apa()+
  legend(legend = c("Observed", "Predicted")) #<<<<<<?
#ggsave(filename = "Hierarchical-Modelling/08_Plots/Quantiles_Complex.png")


#################################
### Incongruent Cue Condition ###
#################################  

#Start with the observed data
q.mean_inc = as.data.frame(q.mean.2.2) #Make q.mean.2.1 a data frame called q.mean
q.mean_inc$p.mean = qs*p.mean.1.1 #Make a qs*p.means column for plotting 
q.mean_inc$data = "Observed" #Add an extra column so that we can identify which data the adjacent column belongs to (grouping variable)
colnames(q.mean_inc) = c("q.means", "p.means", "Data") #Add column names so that they're the same for both simulated and observed data. 

#Now do it for simulated data
q.mean.sim_inc = as.data.frame(q.mean.2.2_Sim)
q.mean.sim_inc$p.mean = qs*p.mean.1.1_Sim
q.mean.sim_inc$data = "Complex Model" 
colnames(q.mean.sim_inc) = c("q.means","p.means", "Data")

#Combine the simulated and observed data into one data frame
q.means_inc = rbind(q.mean_inc, q.mean.sim_inc)

#Now let's plot again
ggplot(q.means_inc)+
  geom_point(aes(x = q.means, y = p.means, shape = Data))+
  ylab("p.means*qs")+
  labs(title = "Incongruent")+
  theme_apa()
ggsave(filename = "Hierarchical-Modelling/08_Plots/Quantiles_Complex_incongruent.png")

#------ Extra stuff -----#

#points(tmp1,qs*mean(sim_matrix_complex$Resp[sim_matrix_complex$Cond==1]==2),pch=5)
#points(tmp2,qs*mean(sim_matrix_simple$Resp[sim_matrix_simple$Cond==1]==2),pch=6)





#tmp=tapply(P1$Time,list(P1$Resp,P1$Cond),quantile,qs)[2,2][[1]]

#tmp1=tapply(sim_matrix_complex$Time,list(sim_matrix_complex$Resp,sim_matrix_complex$Cond),quantile,qs)[2,2][[1]]
#tmp2=tapply(sim_matrix_simple$Time,list(sim_matrix_simple$Resp,sim_matrix_simple$Cond),quantile,qs)[2,2][[1]]

plot(tmp,qs*mean(P1$Resp[P1$Cond==2]==2),pch=16,xlim=c(0.2,0.5))
points(tmp1,qs*mean(sim_matrix_complex$Resp[sim_matrix_complex$Cond==2]==2),pch=5)
points(tmp2,qs*mean(sim_matrix_simple$Resp[sim_matrix_simple$Cond==2]==2),pch=6)



