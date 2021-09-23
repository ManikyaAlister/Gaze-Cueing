rm(list = ls())
library(tidyverse)
load("~/Documents/2021/Gaze-Cueing/Data/dataset1a/clean/all-participants.RData")
faces.data = P
faces.data = faces.data %>%
  select(ID, Time, Cond) %>%
group_by(ID,Cond) %>%
  summarise(Time = mean(Time))

faces.data.1 = faces.data.2 = filter(faces.data, Cond == 1)
faces.data.2 = filter(faces.data, Cond == 2)
time.2 = faces.data.2$Time

faces.data = cbind(faces.data.1, time.2)
faces.data = select(faces.data, -Cond)
colnames(faces.data) = c("ID", "Time.1", "Time.2")

faces.data$magnitude.faces = faces.data$Time.2 - faces.data$Time.1

load("~/Documents/2021/Gaze-Cueing/Data/dataset1b/clean/all-participants.RData")
arrows.data = P 
arrows.data = arrows.data %>%
  select(ID, Time, Cond) %>%
  group_by(ID,Cond) %>%
  summarise(Time = mean(Time))

arrows.data.1 = arrows.data.2 = filter(arrows.data, Cond == 1)
arrows.data.2 = filter(arrows.data, Cond == 2)
time.2 = arrows.data.2$Time

arrows.data = cbind(arrows.data.1, time.2)
arrows.data = select(arrows.data, -Cond)
colnames(arrows.data) = c("ID", "Time.1", "Time.2")

arrows.data$magnitude.arrows = arrows.data$Time.2 - arrows.data$Time.1

cor_faceVarrow = cor(faces.data$magnitude.faces, arrows.data$magnitude.arrows)
# r = 0.09131829
plot(faces.data$magnitude.faces, arrows.data$magnitude.arrows)

#removing outlier at the bottom (p25): 
rm25.faces = faces.data[-25,]
rm25.arrows = arrows.data[-25,]
cor(rm25.faces$magnitude.faces, rm25.arrows$magnitude.arrows)
plot(rm25.faces$magnitude.faces, rm25.arrows$magnitude.arrows)
# r =0.2005099

