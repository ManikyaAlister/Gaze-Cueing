rm(list = ls())
library(tidyverse)
library(BayesFactor)
setwd("~/Dropbox/2021/Gaze Cueing/Data/Gregory and Jackson (2020)")
Study1RawData_ = read_csv("Study1RawData .csv")

#We're only really interested in faces, rather than arrows and lines, so I'm going to
#filter out the other cue type conditions. I'm also going to filter outliers. 

dat_face = filter(Study1RawData_, CueType == "Face", ReactionTime < 60000)

#Set variables to factors 
dat_face$SOA = as.factor(dat_face$SOA)
dat_face$Validity = as.factor(dat_face$Validity)

#Run an ANOVA for each level of SOA
anova = aov(ReactionTime ~ as.factor(SOA), data = dat_face)
summary(anova)

#Now do it Bayes
anova_b = anovaBF(ReactionTime ~ SOA, data = dat_face)
summary(anova_b) 

#Quite strong evidence that there is an effect. Let's graph it.

dat_face %>%
  ggplot(aes(x = SOA, y = ReactionTime, group = SOA))+
  geom_violin()+
  facet_wrap(~Validity)


#Let's run some t tests at each level of SOA to follow up the ANOVA
anova_followup = TukeyHSD(anova)
anova_followup

#Can't remember how to follow up the Bayes ANOVA

#Let's see if the Validity effect occurs

t_test = t.test(dat_face$ReactionTime, dat_face$Validity)

#Let's graph it


dat_face %>%
  ggplot(aes(x = Validity, y = ReactionTime))+
  geom_point()

means_SOA %>%
  ggplot(aes(x = SOA, y = mean_validity, group = SOA))+
  geom_bar()


means_validity = dat_face %>%
  group_by(Validity) %>%
  summarise(mean_validity = mean(ReactionTime))

means_SOA = dat_face %>%
  group_by(Validity, SOA) %>%
  summarise(mean_validity = mean(ReactionTime))
