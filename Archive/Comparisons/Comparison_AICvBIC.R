rm(list = ls())
setwd("~/Dropbox/2021/Gaze Cueing")
library(tidyverse)

#---------------------------------------------------------------------------------------------------
#-----------------------------------------Model Comparison -----------------------------------------
#---------------------------------------------------------------------------------------------------

# Define how many data sets to use
n = 41

#---------------------------------------------------------------------------------------------------
## Complex model ##

# Create blank vectors so the loop knows what to fill in

AIC_C = NULL
BIC_C = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { 
  load(paste("Fits_Complex/P",p,"Complex_Model.RData", sep = ""))
  AIC_C = rbind(AIC, AIC_C)
  BIC_C = rbind(BIC, BIC_C)
}
save(AIC_C, file = "Comparisons/AIC_C.RData")
save(BIC_C, file = "Comparisons/BIC_C.RData")
  
#---------------------------------------------------------------------------------------------------
## Simple Model ##

AIC_S = NULL
BIC_S = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Fits_Simple/P",p,"Simple_Model.RData", sep = ""))
  AIC_S = rbind(AIC, AIC_S)
  BIC_S = rbind(BIC, BIC_S)
}
save(AIC_S, file = "Comparisons/AIC_S.RData")
save(BIC_S, file = "Comparisons/BIC_S.RData")

#---------------------------------------------------------------------------------------------------
## v Model ##

AIC_v = NULL
BIC_v = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Fits_v/P",p,"V_Model.RData", sep = ""))
  AIC_v = rbind(AIC, AIC_v)
  BIC_v = rbind(BIC, BIC_v)
}
save(AIC_v, file = "Comparisons/AIC_v.RData")
save(BIC_v, file = "Comparisons/BIC_v.RData")

#---------------------------------------------------------------------------------------------------
## z Model ##

AIC_z = NULL
BIC_z = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Fits_z/P",p,"z_Model.RData", sep = ""))
  AIC_z = rbind(AIC, AIC_z)
  BIC_z = rbind(BIC, BIC_z)
}
save(AIC_z, file = "Comparisons/AIC_z.RData")
save(BIC_z, file = "Comparisons/BIC_z.RData")

#---------------------------------------------------------------------------------------------------
#Compare AIC of each model for each participant 

AIC_comp = cbind(AIC_C, AIC_S, AIC_v, AIC_z)
colnames(AIC_comp) = c("AIC_C", "AIC_S", "AIC_v", "AIC_z")

#Figure out which model was the best for each participant
best_AIC = max.col(-AIC_comp)

best_AIC = case_when(best_AIC == 1 ~ "Complex Model",
                     best_AIC == 2 ~ "Simple Model",
                     best_AIC == 3 ~ "v (drift rate) Model",
                     best_AIC == 4 ~ "z (starting point) Model")
best_AIC = as.factor(best_AIC)

count(as.dat.framebest_AIC ==  4)


count(best_AIC) %>% as.data.frame()

hist(best_AIC)
#---------------------------------------------------------------------------------------------------
#Compare BIC of each model for each participant 

BIC_comp = cbind(AIC_C, AIC_S, AIC_v, AIC_z)
colnames(BIC_comp) = c("BIC_C", "BIC_S", "BIC_v", "BIC_z")

best_BIC = max.col(-BIC_comp)
best_BIC = case_when(best_BIC == 1 ~ "Complex Model",
                     best_BIC == 2 ~ "Simple Model",
                     best_BIC == 3 ~ "v (drift rate) Model",
                     best_BIC == 4 ~ "z (starting point) Model")
best_BIC = as.character(best_BIC)
count(best_BIC == "Complex Model")

#---------------------------------------------------------------------------------------------------
## Plot ##

# AIC 

ggplot(data.frame(best_AIC), (aes(x = best_AIC)))+
  geom_histogram(stat = "count")+
  theme_classic(base_family = "Times")+
  ylab("No. of Participants Best Fit") +
  xlab("AIC Comparison")
ggsave(filename = "Modelling/07_Plots/AIC.png")

# BIC

 ggplot(data.frame(best_BIC), (aes(x = best_BIC)))+
  geom_histogram(stat = "count")+
  theme_classic(base_family = "Times")+
  ylab("No. of Participants Best Fit") +
   xlab("BIC Comparison")
 ggsave(filename = "Modelling/07_Plots/BIC.png")
 
