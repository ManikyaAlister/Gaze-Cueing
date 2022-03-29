setwd("~/Documents/2021/Gaze-Cueing")
rm(list = ls())
load("Data/dataset1b/derived/magnitudes-params-incl.probs.RData")

mean_magnitude = round(mean(table$magnitude) * 1000, 2)
sd_magnitude = round(sd(table$magnitude) * 1000, 2)

count_magnitudes = sum(table$magnitude > 0 ) 
count_magnitudes 

descriptives_1b = c(mean_magnitude, sd_magnitude, count_magnitudes)
names(descriptives_1b) = c("Mean Magnitude", "SD", "count")

save(descriptives_1b, file = "Data/dataset1b/derived/descriptives.Rdata")
