rm(list = ls())
library(tidyverse)
setwd("~/Documents/2021/Gaze-Cueing")

load("Data/dataset1a/derived/BICs.Rdata")

getWeights=function(x) {
  useX = x*(-0.5) # Transform BIC/AIC to a chi square distribution
  if (mean(is.na(useX)) == 1) {
    return(NA)
  }
  maxLogDens=max(useX)
  if (maxLogDens > 700) {
    densTransform=maxLogDens-700
    useX=useX-densTransform
  } else if (maxLogDens < -710) {
    densTransform=maxLogDens-700
    useX=useX-densTransform
  } else {
    densTransform=0
  }
  exp(useX)/sum(exp(useX))
}

S = 41

BICweights=array(NA,c(S,8))
for (s in 1:S) {
  BICweights[s,]= getWeights(BIC_comp[s,])
}
colnames(BICweights) = c("v_z", "none", "v", "z", "z_t0", "v_t0", "all_params", "t0")

#Get inclusion probabilities of all of the parameters
prob_z = NULL
prob_v = NULL
prob_t0 = NULL

for (i in 1:S) {
prob_z[i]=sum(BICweights[i,c("z","v_z", "all_params", "z_t0")])
prob_v[i]=sum(BICweights[i,c("v","v_z", "all_params", "v_t0")])
prob_t0[i]=sum(BICweights[i,c("t0","z_t0", "all_params", "v_t0")])
}
BIC_incl_prob = cbind(prob_z, prob_v, prob_t0)
save(BIC_incl_prob, file = "Data/dataset1a/derived/BIC_inclusion_probabilities.RData")

#Get gaze cueing magnitudes
library(tidyverse)
load("Data/dataset1a/clean/all-participants.RData")
data = P
data = data %>%
  select(ID, Time, Cond) %>%
  group_by(ID,Cond) %>%
  summarise(Time = mean(Time))

data.1 = data.2 = filter(data, Cond == 1)
data.2 = filter(data, Cond == 2)
time.2 = data.2$Time

data = cbind(data.1, time.2)
data = select(data, -Cond)
colnames(data) = c("ID", "Time.1", "Time.2")

magnitude = data$Time.2 - data$Time.1

#Get mean parameter differences for each participant
params =array(NA,c(S,6))
for (i in 1:S) {
  load(paste("Modelling/dataset1a/07_Outputs/P",i,"_complex_Model.RData", sep = "")) 
params[i,]  = apply(theta, 2, mean)
}
colnames(params) = theta.names
params = as.data.frame(params)
params$t0.diff = params$t0.2-params$t0.1
params$v.diff = params$v.2-params$v.1
params.diff = select(params, z, v.diff, t0.diff)

#Get mean parameter SD
params_SD =array(NA,c(S,6))
for (i in 1:S) {
  load(paste("Modelling/dataset1a/07_Outputs/P",i,"_complex_Model.Rdata", sep = "")) 
  params_SD[i,]  = apply(theta, 2, sd)
}
colnames(params_SD) = theta.names
params_SD = as.data.frame(params_SD)
table = cbind(magnitude, params.diff, prob_z, prob_v, prob_t0)
cor(table$magnitude, params_SD$t0.1)
plot(table$magnitude, params_SD$t0.1)

cor(table$magnitude, params_SD$t0.2)
plot(table$magnitude, params_SD$t0.2)
# Combine all together: 


table = cbind(magnitude, params.diff, prob_z, prob_v, prob_t0)
save(table, file = "Data/dataset1a/derived/magnitudes-params-incl.probs.RData")

table = as.data.frame(table)

t0.diffVt0prob = cor(table$t0.diff, table$prob_t0)
plot(table$t0.diff, table$prob_t0)

t0probVmag = cor(table$prob_t0, table$magnitude)
plot(table$prob_t0, table$magnitude)


