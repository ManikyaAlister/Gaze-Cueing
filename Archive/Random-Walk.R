#https://www.r-bloggers.com/2020/09/simulating-paths-from-a-random-walk/

rm(list = ls())
library(tidyverse)
library(gganimate)
# returns the random walk path values as a vector 
# (random walk always starts at 0)
# p: probability of increasing by 1
# stop if path value hits either `lower` or `upper`
run <- function(p, lower, upper) {
  values <- c(0)
  current <- 0
  while (current > lower & current < upper) {
    current <- current + ifelse(runif(1) < p, 1, -1)
    values <- c(values, current)
  }
  values
}

# returns the random walk path values as a vector 
# (random walk always starts at 0)
# p: probability of increasing by 1
# stop if path value hits either `lower` or `upper`
run <- function(p, lower, upper) {
  values <- c(0)
  current <- 0
  while (current > lower & current < upper) {
    current <- current + ifelse(runif(1) < p, 0.1, -0.1)
    values <- c(values, current)
  }
  values
}

N <- 1  # no. of paths to simulate
p <- 0.52
lower <- -1
upper <- 1

# simulate paths
set.seed(1055)
vlist <- replicate(N, run(p, lower, upper), simplify = TRUE)

# get length of longest path
#max_length <- max(sapply(vlist, length))

# Get length of path 
length = length(vlist)
plot_data = expand.grid(time = 1:length) 
  plot_data$walk = vlist
                        

P = ggplot(plot_data)+
  geom_line(aes(x = time, y = walk))+
  ylim(-1, 1)+
  theme_minimal()
  
P + transition_reveal(time)
  
  

# make plot
#par(mar = rep(0, 4))  # no margins
#plot(c(1, max_length), c(lower, upper), type = "n")
#for (i in 1:N) {
#  lines(1:length(vlist[[i]]), vlist[[i]])
#}
#abline(h = 0, lty = "dashed")
#abline(h = lower, lwd = 2)
#abline(h = upper, lwd = 2)

