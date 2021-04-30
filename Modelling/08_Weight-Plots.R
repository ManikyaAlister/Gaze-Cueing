rm(list = ls())
library(tidyverse)
n = 41
S = n
setwd("~/Dropbox/2021/Gaze-Cueing")
 
######## Load AIC and BIC for Each Model #######
#Or Skip to lines 83/84 if already saved
#---------------------------------------------------------------------------------------------------
## Complex model ##

# Create blank vectors so the loop knows what to fill in

AIC_C = NULL
BIC_C = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { 
  load(paste("Fits_Complex/P",p,"Complex_Model.RData", sep = ""))
  AIC_C = rbind(AIC, AIC_C)
  BIC_C = rbind(BIC, BIC_C)
}
#save(AIC_C, file = "Comparisons/AIC_C.RData")
#save(BIC_C, file = "Comparisons/BIC_C.RData")
#Repeat for other models
#---------------------------------------------------------------------------------------------------
## Simple Model ##

AIC_S = NULL
BIC_S = NULL 

for (p in 1:n) { 
  load(paste("Fits_Simple/P",p,"Simple_Model.RData", sep = ""))
  AIC_S = rbind(AIC, AIC_S)
  BIC_S = rbind(BIC, BIC_S)
}


#---------------------------------------------------------------------------------------------------
## v Model ##

AIC_v = NULL
BIC_v = NULL 

for (p in 1:n) { 
  load(paste("Fits_v/P",p,"V_Model.RData", sep = ""))
  AIC_v = rbind(AIC, AIC_v)
  BIC_v = rbind(BIC, BIC_v)
}
#save(AIC_v, file = "Comparisons/AIC_v.RData")
#save(BIC_v, file = "Comparisons/BIC_v.RData")

#---------------------------------------------------------------------------------------------------
## z Model ##

AIC_z = NULL
BIC_z = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Fits_z/P",p,"z_Model.RData", sep = ""))
  AIC_z = rbind(AIC, AIC_z)
  BIC_z = rbind(BIC, BIC_z)
}
#save(AIC_z, file = "Comparisons/AIC_z.RData")
#save(BIC_z, file = "Comparisons/BIC_z.RData")

#---------------------------------------------------------------------------------------------------
## z Interference Model ##

AIC_zinter = NULL
BIC_zinter = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Fits_zinter/P",p,"zinter_Model.RData", sep = ""))
  AIC_zinter = rbind(AIC, AIC_zinter)
  BIC_zinter = rbind(BIC, BIC_zinter)
}

#---------------------------------------------------------------------------------------------------
## z Facilitation Model ##

AIC_zfacil = NULL
BIC_zfacil = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Fits_zfacil/P",p,"zfacil_Model.RData", sep = ""))
  AIC_zfacil = rbind(AIC, AIC_zfacil)
  BIC_zfacil = rbind(BIC, BIC_zfacil)
}

#---------------------------------------------------------------------------------------------------
#Compare AIC of each model for each participant 

AIC_comp = cbind(AIC_C, AIC_S, AIC_v, AIC_z, AIC_zinter, AIC_zfacil)
colnames(AIC_comp) = c("complex", "none", "v", "z", "zinter", "zfacil")

BIC_comp = cbind(BIC_C, BIC_S, BIC_v, BIC_z, BIC_zinter, BIC_zfacil)
colnames(BIC_comp) = c("complex", "none", "v", "z", "zinter", "zfacil")

save(AIC_comp, file = "Data/AICs.Rdata")
save(BIC_comp, file = "Data/BICs.Rdata")

###########################
### Model Probabilities ##
###########################

load("Data/BICs.Rdata")
load("Data/AICs.Rdata")

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

########## BIC ###########

BFweights=array(NA,c(,4))
for (s in 1:S) {
  BFweights[s,]= getWeights(BIC_comp[s,])
}
colnames(BFweights) = c("complex", "none", "v", "z", "zinter", "zfacil")


#save(BFweights, file = "Data/BF-Weights.RData")
#save(BIC_comp, file = "Data/BICs.RData")

#If I want to order by V model 
# BFweights = BFweights[order(BFweights[,4]),]

# Plot

S = 41 #n participants
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="BIC",xaxt="n",yaxt="n")

# Plotting

for (i in 1:S) {
  use.i=i
  sumThing=0
  
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,BFweights[use.i,"none"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"none"]
  
  col=hcl(h=130,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BFweights[use.i,"z"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"z"] #green 
  
  col=hcl(h=220,c=100,l=40)
  rect(i-0.5,sumThing,i+0.5,sumThing+BFweights[use.i,"v"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"v"] #Yellow
  
  col=hcl(h=420,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BFweights[use.i,"complex"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"complex"] #Blue 
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", 
       legend = c("Complex", "Simple", "v", "z"), 
       col = c(hcl(h=420,c=100,l=70),hcl(h=330,c=100,l=40), hcl(h=220,c=100,l=40), hcl(h=130,c=100,l=70)),
       pch = 15,
       horiz = T,
       cex = .73,
       inset=c(0,.0))
       

########## AIC ###########

AICweights=array(NA,c(S,4))
for (s in 1:S) {
  AICweights[s,]= getWeights(AIC_comp[s,])
}
AIC_weights = getWeights(AICweights)

colnames(AICweights) = c("complex", "none", "v", "z")
#save(AICweights, file = "Data/BF-Weights.RData")
#save(AIC_comp, file = "Data/BICs.RData")

#If I want to order by V model 
# AICweights = AICweights[order(AICweights[,4]),]

# Plot

S = 41
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="AIC",xaxt="n",yaxt="n")

for (i in 1:S) {
  use.i=i
  sumThing=0
  
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,AICweights[use.i,"none"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"none"]
  
  col=hcl(h=130,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"z"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"z"] #green 
  
  col=hcl(h=220,c=100,l=40)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"v"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"v"] #Yellow
  
  col=hcl(h=420,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"complex"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"complex"] #Blue 
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom",legend = c("Complex", "Simple", "v", "z"), 
       col = c(hcl(h=420,c=100,l=70),hcl(h=330,c=100,l=40), hcl(h=220,c=100,l=40), hcl(h=130,c=100,l=70)),
       pch = 15, 
       inset=c(0,0),
       horiz = T,
       cex = .73)

####################################
#####Inclusion Probability #########
####################################

### BIC ####

#To get a inclusion probability for z, we just add the models that have z and don’t have z:
  
  plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="z Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BFweights[use.i,c("none","v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=40)
  currWeight=sum(BFweights[use.i,c("z","complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", legend = c("z varies", "z does not vary"), 
       col = c(hcl(h=110,c=100,l=40),hcl(h=330,c=100,l=0)),
       pch = 15, 
       inset=c(0,0),
       horiz = T,
       cex = .73)


#And same principle for v:
  
  plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="v Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=1)
  currWeight=sum(BFweights[use.i,c("none","z")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BFweights[use.i,c("v","complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", legend = c("v varies", "v does not vary"), 
       col = c(hcl(h=110,c=100,l=40),hcl(h=330,c=100,l=0)),
       pch = 15, 
       inset=c(0,0),
       horiz = T,
       cex = .73)

### AIC ####

#To get a inclusion probability for z, we just add the models that have z and don’t have z:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="z Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(AICweights[use.i,c("none","v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=40)
  currWeight=sum(AICweights[use.i,c("z","complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", legend = c("z varies", "z does not vary"), 
       col = c(hcl(h=110,c=100,l=40),hcl(h=330,c=100,l=0)),
       pch = 15, 
       inset=c(0,0),
       horiz = T,
       cex = .73)


#And same principle for v:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="v Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=1)
  currWeight=sum(AICweights[use.i,c("none","z")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(AICweights[use.i,c("v","complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", legend = c("v varies", "v does not vary"), 
       col = c(hcl(h=110,c=100,l=40),hcl(h=330,c=100,l=0)),
       pch = 15, 
       inset=c(0,0),
       horiz = T,
       cex = .73)
