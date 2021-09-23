setwd("~/Documents/2021/Gaze-Cueing")
load("Data/dataset1c/derived/magnitudes-params-incl.probs.RData")

mean_magnitude = mean(table$magnitude)
sd_magnitude = sd(table$magnitude)

count_magnitudes = sum(table$magnitude > 0 )
count_magnitudes 

descriptives_1c = c(mean_magnitude, sd_magnitude, count_magnitudes)
names(descriptives_1c) = c("Mean Magnitude", "SD", "count")

save(descriptives_1c, file = "Data/dataset1c/derived/descriptives.Rdata")
