rm(ls == list())
setwd("~/Dropbox/2021/Gaze-Cueing")
library(bridgesampling)

#Load fits for each model
#Complex: 
load("Fits_Complex/P1Complex_Model.Rdata")
test = max(weight)

#simple: 
test_s = max(weight)

#z: 
test_z = max(weight)

output = bayes_factor(test, test_s)

bridge_sampler(posterior)

posterior = list(theta)
