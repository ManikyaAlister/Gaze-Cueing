### Dataset3 ###

setwd("~/Documents/2021/Gaze-Cueing")
rm(list = ls())
library('metafor')

#see info for effect size calculations:
?escalc

# measure = which effect size you want to calculate
# we want a standardized mean change score
# i.e., a standardized quantifification of differences in a
# repeated measures variablesc

# see in ?escalc
# section "measures for quantitative variables" c/pd below:

#When the response or dependent variable assessed in the individual studies is measured on some quantitative scale, 
# the raw mean change, standardized versions thereof, or the (log transformed) ratio of means (log response ratio) can be used 
# as outcome measures (Becker, 1988; Gibbons et al., 1993; Lajeunesse, 2011; Morris, 2000). Here, one needs to specify m1i and m2i, 
# the observed means at the two measurement occasions, sd1i and sd2i for the corresponding observed standard deviations, 
# ri for the correlation between the measurements at the two measurement occasions, and ni for the sample size. 

# The options for the measure argument are then:"MC" for the raw mean change,
# "SMCC" for the standardized mean change using change score standardization (Gibbons et al., 1993),
# "SMCR" for the standardized mean change using raw score standardization (Becker, 1988),
# "SMCRH" for the standardized mean change using raw score standardization with heteroscedastic population 
# variances at the two measurement occasions (Bonett, 2008),
# "ROMC" for the log transformed ratio of means (Lajeunesse, 2011).

# See also Morris and DeShon (2002) for a thorough discussion of the difference between the change score measures.
# A few notes about the change score measures. In practice, one often has a mix of information available from the 
# individual studies to compute these measures. In particular, if m1i and m2i are unknown, but the raw mean change 
# is directly reported in a particular study, then one can set m1i to that value and m2i to 0 (making sure that the 
# raw mean change was computed as m1i-m2i within that study and not the other way around). Also, for the raw mean 
# change ("MC") or the standardized mean change using change score standardization ("SMCC"), if sd1i, sd2i, and ri are 
# unknown, but the standard deviation of the change scores is directly reported, then one can set sd1i to that value and 
# both sd2i and ri to 0. Finally, for the standardized mean change using raw score standardization ("SMCR"), argument sd2i 
# is actually not needed, as the standardization is only based on sd1i (Becker, 1988; Morris, 2000), which is usually the 
# pre-test standard deviation (if the post-test standard deviation should be used, then set sd1i to that). Note that all of 
# these measures are also applicable for matched-pairs designs (subscripts 1 and 2 then simply denote the first and second 
# group that are formed by the matching).

## SMCC is a good choice just go with that!
#here's the code:

# escalc("SMCC", m1i = valid.mean, m2i = invalid.mean, sd1i = valid.sd, sd2i = invalid.sd, ri = valid.invalid.correlation, ni = sample.size)

# and that's it!

library(tidyverse)
load("Data/dataset3/clean/all-participants.RData")


# Calculate mean & SD for cued and miscued conditions 

data_all = P %>%
  select(PID, Time, Cond) %>%
  group_by(Cond) %>%
  summarise(Mean = mean(Time), SD = sd(Time))



# Calculate correlation between cued and miscued trials

cued = filter(P, Cond == 1) %>%
  group_by(PID) %>%
  summarise(Mean = mean(Time))

miscued = filter(P, Cond == 2) %>%
  group_by(PID) %>%
  summarise(Mean = mean(Time))

cor_data =  cbind(cued$Mean,miscued$Mean)           
correlation = cor(cor_data) 

# Calculate inputs for escalc
data_all$Mean = as.numeric(data_all$Mean)
data_all$SD = as.numeric(data_all$SD)

ri = correlation[1,2]
ri = ri[[1]]

m1i = data_all[2,2]
m1i = m1i[[1]]

m2i = data_all[1,2]
m2i = m2i[[1]]

sd1i = data_all[2,3]
sd1i = sd1i[[1]]

sd2i = data_all[1,3]
sd2i = sd2i[[1]]

ni = as.numeric(max(P$PID))

# Calculate standardised mean change score

smcc3 = escalc("SMCC", m1i = m1i, m2i = m2i, sd1i = sd1i, sd2i = sd2i, ri = ri, ni = ni)

