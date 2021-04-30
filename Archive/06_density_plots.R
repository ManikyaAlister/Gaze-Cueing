##################
####PLOTTING######
##################

rm(list = ls())
library(tidyverse)
library(sm)
setwd("~/Dropbox/2021/Gaze-Cueing")

#load in the model predictions and raw data


load("Data/Gregory-and-Jackson-(2020)/P1.RData")
P1 = select(P1, Time, Cond, Resp)
load("Data/model_predictions/P1_complex.RData")
load("Data/model_predictions/P1_simple.RData")
load("Data/model_predictions/P1_v.RData")
load("Data/model_predictions/P1_z.RData")

#Add a "model" column to every dataset so that when they're combined I can tell what model the data belongs to

P1$Model = "Observed"
sim_matrix_complex$Model = "Complex"
sim_matrix_simple$Model = "Simple"
sim_matrix_v$Model = "v"
sim_matrix_z$Model = "z"

# Make sure the "Time" reaction time variable is numeric
P1$Time = as.numeric(P1$Time)
dat2$Time = as.numeric(dat2$Time)
dat$Time = as.numeric(dat$Time)

#Check means
means = dat%>%
group_by(Model, Cond) %>%
  summarise(mean = mean(as.numeric(Time)))

#Combine all of the model predictions and observed data
dat = rbind(P1, sim_matrix_complex, sim_matrix_simple, sim_matrix_v, sim_matrix_z)

#Make a slightly smaller dataset with just the observed data and complex model
dat2 = rbind(P1, sim_matrix_complex)

#Practice with a density function
P1 %>% 
  ggplot(aes(x = Time))+
  geom_density()

#Density function with all of the data as a function of model
dat2 %>% 
  ggplot(aes(x = Time, fill = Model))+
  geom_density(alpha = .25)

#Density function with all of the data as a function of model and condition
dat %>% 
  ggplot(aes(x = Time, fill = Model))+
  geom_density(alpha = .25)

ggsave("Modelling/07_Plots/density_accrossConds.png", width = 10, height = 7)

dat %>% 
  ggplot(aes(x = Time, fill = Model))+
  geom_density(alpha = .25)+
  facet_wrap(~Cond)

ggsave("Modelling/07_Plots/density_1v2.png", width = 10, height = 7)

