##################
####PLOTTING######
##################

rm(list = ls())
library(tidyverse)
library(sm)
library(jtools)
setwd("~/Dropbox/2021/Gaze-Cueing")

##load in the model predictions and observed data ##

#Observed data

load("Data/Gregory-and-Jackson-(2020)/all-participants.RData")
P$Model = "Observed"
observed = select(P, Time, Cond, Model)


## Complex model predictions ##
load("Data/Hier_model_predictions/complex.RData")

#Convert into a data frame
simData = do.call(rbind.data.frame, simData)
simData$Model = "Complex Model"
simData = select(simData, Time, Cond, Model)

#Combine simulated data and observed data

dat = rbind(simData, observed)

ggplot(dat, aes(x = Time, colour = Model))+
  geom_density(alpha = .25)+
  xlim(0,1)+
  theme_apa()+
  facet_wrap(~Cond)

ggsave(filename = "Hierarchical-Modelling/08_Plots/density-plots.png", width = 12, height = 10)
