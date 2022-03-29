##############################
## Exploratory Correlations ##
##############################
library(BayesFactor)

# Correlation between data set 1a cueing magnitudes and likelihood of being described by the simple model
library(BayesFactor)
load("Data/dataset1a/derived/magnitudes-params-incl.probs.RData")
load("Data/dataset1a/derived/BIC-Weights.RData")
output = correlationBF(table$magnitude, BICweights[,"none"], posterior = FALSE)


# Correlation between data set 1a t0 probability and dataset 1b
load("Data/dataset1a/derived/magnitudes-params-incl.probs.RData")
table1 = table
load("Data/dataset1b/derived/magnitudes-params-incl.probs.RData")
table2 = table
output2 = correlationBF(table1$prob_t0, table2$prob_t0)
output2

# Correlation between cueing magnitudes in data sets 1a and 1b
output3 = correlationBF(table1$magnitude, table2$magnitude)
cor(table1$magnitude, table2$magnitude)

# Bayes t-test between DS1 and DS2
p.1 = filter(P, Validity == "Valid")
p.2 = filter(P, Validity == "Invalid")
tmp.1 = p.1 %>%
  group_by(ID) %>%
  summarise(mean = mean(Time))

tmp.2 = p.2 %>%
  group_by(ID) %>%
  summarise(mean = mean(Time))

magnitude = tmp.2$mean-tmp.1$mean
mean(magnitude)


tmp1 = 