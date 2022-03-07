rm(list = ls())
setwd("~/cloudstor/Gaze-Cueing")
load("Data/dataset1a/derived/magnitudes-params-incl.probs.RData")

mean_magnitude = round(mean(table$magnitude) * 1000, 2)  #cueing magnitude
sd_magnitude = round(sd(table$magnitude) * 1000, 2)      #SD cueing magnitude    

count_magnitudes = sum(table$magnitude > 0 )  #N participants with a positive cueing magnitude
count_magnitudes 

descriptives_1a = c(mean_magnitude, sd_magnitude, count_magnitudes)
names(descriptives_1a) = c("Mean Magnitude", "SD", "count")

save(descriptives_1a, file = "Data/dataset1a/derived/descriptives.Rdata")
