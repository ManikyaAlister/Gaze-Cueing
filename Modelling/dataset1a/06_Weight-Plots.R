rm(list = ls())
library(tidyverse)
S = 41 #subjects
setwd("~/Documents/2021/Gaze-Cueing")
## Dataset  1b ##
######## Load AIC and BIC for Each Model #######
#Or Skip to lines 83/84 if already saved
#---------------------------------------------------------------------------------------------------
## v_z model ##

# Create blank vectors so the loop knows what to fill in

AIC_v_z = NULL
BIC_v_z = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { 
  load(paste("Modelling/dataset1a/07_Outputs/P",p,"_v-z_Model.RData", sep = ""))
  AIC_v_z = rbind(AIC, AIC_v_z)
  BIC_v_z = rbind(BIC, BIC_v_z)
  
}

#save(AIC_C, file = "Data/dataset1a/derived/AIC_C.RData") #To save for each individual model if wanted (copy for each model)
#save(BIC_C, file = "Data/dataset1a/derived/BIC_C.RData")
#Repeat for other models
#---------------------------------------------------------------------------------------------------
## Simple Model ##
AIC_S = NULL
BIC_S = NULL 

for (p in 1:S) { 
  load(paste("Modelling/dataset1a/07_Outputs/P",p,"Simple_Model.RData", sep = ""))
  AIC_S = rbind(AIC, AIC_S)
  BIC_S = rbind(BIC, BIC_S)

}

#---------------------------------------------------------------------------------------------------
## v Model ##

AIC_v = NULL
BIC_v = NULL 

for (p in 1:S) { 
  load(paste("Modelling/dataset1a/07_Outputs/P",p,"_v_Model.RData", sep = ""))
  AIC_v = rbind(AIC, AIC_v)
  BIC_v = rbind(BIC, BIC_v)
}

#---------------------------------------------------------------------------------------------------
## z Model ##

AIC_z = NULL
BIC_z = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(paste("Modelling/dataset1a/07_Outputs/P",p,"_z_Model.RData", sep = ""))
  AIC_z = rbind(AIC, AIC_z)
  BIC_z = rbind(BIC, BIC_z)
}
#---------------------------------------------------------------------------------------------------
## t0 Model ##

AIC_t0 = NULL
BIC_t0 = NULL 

for (p in 1:S) { 
  load(paste("Modelling/dataset1a/07_Outputs/P",p,"_t0_Model.RData", sep = ""))
  AIC_t0 = rbind(AIC, AIC_t0)
  BIC_t0 = rbind(BIC, BIC_t0)
}
#---------------------------------------------------------------------------------------------------
## z_t0 model##

AIC_z_t0 = NULL
BIC_z_t0 = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(paste("Modelling/dataset1a/07_outputs/P",p,"_z-t0_Model.RData", sep = ""))
  AIC_z_t0 = rbind(AIC, AIC_z_t0)
  BIC_z_t0 = rbind(BIC, BIC_z_t0)
}

#---------------------------------------------------------------------------------------------------
## v-t0 Model ##

AIC_v_t0 = NULL
BIC_v_t0 = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(paste("Modelling/dataset1a/07_Outputs/P",p,"_v-t0_Model.RData", sep = ""))
  AIC_v_t0 = rbind(AIC, AIC_v_t0)
  BIC_v_t0 = rbind(BIC, BIC_v_t0)
}

#---------------------------------------------------------------------------------------------------
## all-parameters model##

AIC_C = NULL
BIC_C = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(paste("Modelling/dataset1a/07_Outputs/P",p,"_complex_Model.RData", sep = ""))
  AIC_C = rbind(AIC, AIC_C)
  BIC_C = rbind(BIC, BIC_C)
}

#---------------------------------------------------------------------------------------------------
#Compare AIC of each model for each participant 

AIC_comp = cbind(AIC_v_z, AIC_S, AIC_v, AIC_z, AIC_z_t0, AIC_v_t0, AIC_C, AIC_t0)
colnames(AIC_comp) = c("v_z", "none", "v", "z", "z_t0", "v_t0", "complex", "t0")

BIC_comp = cbind(BIC_v_z, BIC_S, BIC_v, BIC_z, BIC_z_t0, BIC_v_t0, BIC_C, BIC_t0)
colnames(BIC_comp) = c("v_z", "none", "v", "z", "z_t0", "v_t0", "complex", "t0")

save(AIC_comp, file = "Data/dataset1a/derived/AICs.Rdata")
save(BIC_comp, file = "Data/dataset1a/derived/BICs.Rdata")

###########################
### Model Probabilities ##
###########################

load("Data/dataset1a/derived/BICs.Rdata")
load("Data/dataset1a/derived/AICs.Rdata")

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

BICweights=array(NA,c(S,8))
for (s in 1:S) {
  BICweights[s,]= getWeights(BIC_comp[s,])
}
colnames(BICweights) = c("v_z", "none", "v", "z", "z_t0", "v_t0", "complex", "t0")

### Sort participants into those who were best fitted by the null model v not the null model for model fitting: ###
### Only doing for BIC

BICweights_tmp = BICweights
BICweights_tmp=as.data.frame(BICweights) 

BICweights_alt = filter(BICweights_tmp, none < .5) #participants for which the null model is less than 50% likely
BICweights_null = filter(BICweights_tmp, none >= .5) #participants for which the null model is more than 50% likely

participants_alt = BICweights_alt$ID #Create a vector of all the participants who best fitted alternative models 
participants_null = BICweights_null$ID #And then for null 

save(participants_alt, file = "Data/dataset1a/derived/participants_alt.Rdata")
save(participants_null, file = "Data/dataset1a/derived/participants_null.Rdata")

save(BICweights, file = "Data/dataset1a/derived/BIC-Weights.RData")
save(BIC_comp, file = "Data/dataset1a/derived/BICs.RData")

#If I want to order by V model 
# BICweights = BICweights[order(BICweights[,4]),]

# Plot

S = 41 #n participants

png("Modelling/dataset1a/08_Plots/weighted-prob-BIC.png")
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face BIC",xaxt="n",yaxt="n")

# Plotting
for (i in 1:S) {
  use.i=i
  sumThing=0
  
  col=hcl(h=330,c=100,l=40) #Purple
  rect(i-0.5,0,i+0.5,BICweights[use.i,"none"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"none"]
  
  col=hcl(h=130,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BICweights[use.i,"z"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"z"] #green 
  
  col=hcl(h=220,c=100,l=40)
  rect(i-0.5,sumThing,i+0.5,sumThing+BICweights[use.i,"v"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"v"] #Yellow
  
  col=hcl(h=420,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BICweights[use.i,"v_z"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"v_z"] #Blue 
  
  col=hcl(h=180,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BICweights[use.i,"t0"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"t0"] 
  
  col=hcl(h=280,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BICweights[use.i,"z_t0"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"z_t0"] 
  
  col=hcl(h=380,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BICweights[use.i,"v_t0"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"v_t0"] 
  
  col=hcl(h=80,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BICweights[use.i,"complex"], border = col, col = col)
  sumThing=sumThing+BICweights[use.i,"complex"] 
  
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", 
       legend = c("v_z", "Simple", "v", "z", "t0", "z_t0", "v_t0", "complex"), 
       col = c(hcl(h=420,c=100,l=70),hcl(h=330,c=100,l=40), hcl(h=220,c=100,l=40), hcl(h=130,c=100,l=70), hcl(h=180,c=100,l=70), hcl(h=280,c=100,l=70), hcl(h=380,c=100,l=70), hcl(h=80,c=100,l=70)),
       pch = 15,
       horiz = T,
       cex = .55,
       inset=c(0,.0))
dev.off()   
########## AIC ###########

AICweights=array(NA,c(S,8))
for (s in 1:S) {
  AICweights[s,]= getWeights(AIC_comp[s,])
}
colnames(AICweights) = c("v_z", "none", "v", "z", "z_t0", "v_t0", "complex", "t0")


save(AICweights, file = "Data/dataset1a/derived/AIC-Weights.RData")
save(BIC_comp, file = "Data/dataset1a/derived/BICs.RData")

#If I want to order by V model 
# AICweights = AICweights[order(AICweights[,4]),]

# Plot

S = 41 #n participants
png("Modelling/dataset1a/08_Plots/weighted-prob-AIC.png")
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face AIC",xaxt="n",yaxt="n")

# Plotting

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
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"v_z"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"v_z"] #Blue 
  
  col=hcl(h=180,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"t0"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"t0"] 
  
  col=hcl(h=280,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"z_t0"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"z_t0"] 
  
  col=hcl(h=380,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"v_t0"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"v_t0"] 
  
  col=hcl(h=80,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"complex"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"complex"] 
  
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", 
       legend = c("v_z", "Simple", "v", "z", "t0", "z_t0", "v_t0", "complex"), 
       col = c(hcl(h=420,c=100,l=70),hcl(h=330,c=100,l=40), hcl(h=220,c=100,l=40), hcl(h=130,c=100,l=70), hcl(h=180,c=100,l=70), hcl(h=280,c=100,l=70), hcl(h=380,c=100,l=70), hcl(h=80,c=100,l=70)),
       pch = 15,
       horiz = T,
       cex = .55,
       inset=c(0.3,.0))
dev.off()
####################################
#####Inclusion Probability #########
####################################

### BIC ####

#### z #####

#To get a inclusion probability for z, we just add the models that have z and don’t have z:
png("Modelling/dataset1a/08_Plots/inclusion-prob-BIC-z.png")  
  plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face z Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","v","t0", "v_t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=40)
  currWeight=sum(BICweights[use.i,c("z","v_z", "complex", "z_t0")])
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
dev.off()
##### v ######
#And same principle for v:
png("Modelling/dataset1a/08_Plots/inclusion-prob-BIC-v.png")    
  plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face v Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=1)
  currWeight=sum(BICweights[use.i,c("none","z", "z_t0", "t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("v","v_z", "v_t0", "complex")])
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
dev.off()

#### t0 #####
#And same principle for t0:
png("Modelling/dataset1a/08_Plots/inclusion-prob-BIC-t0.png")  
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face t0 Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BICweights[use.i,c("none","z", "v_z", "v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BICweights[use.i,c("t0","z_t0", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", legend = c("t0 varies", "t0 does not vary"), 
       col = c(hcl(h=110,c=100,l=40),hcl(h=330,c=100,l=0)),
       pch = 15, 
       inset=c(0,0),
       horiz = T,
       cex = .73)
dev.off()
### AIC ####

#### z #####

#To get a inclusion probability for z, we just add the models that have z and don’t have z:
png("Modelling/dataset1a/08_Plots/inclusion-prob-AIC-z.png")  
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face z Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(AICweights[use.i,c("none","v","t0", "v_t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=40)
  currWeight=sum(AICweights[use.i,c("z","v_z", "complex", "z_t0")])
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
dev.off()
##### v ######
#And same principle for v:
png("Modelling/dataset1a/08_Plots/inclusion-prob-AIC-v.png")  
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face v Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=1)
  currWeight=sum(AICweights[use.i,c("none","z", "z_t0", "t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(AICweights[use.i,c("v","v_z", "v_t0", "complex")])
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

dev.off()
#### t0 #####
#And same principle for t0:
png("Modelling/dataset1a/08_Plots/inclusion-prob-AIC-t0.png")  
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="Face t0 Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(AICweights[use.i,c("none","z", "v_z", "v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(AICweights[use.i,c("t0","z_t0", "v_t0", "complex")])
  rect(i-0.5,sumThing,i+0.5,sumThing+currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", legend = c("t0 varies", "t0 does not vary"), 
       col = c(hcl(h=110,c=100,l=40),hcl(h=330,c=100,l=0)),
       pch = 15, 
       inset=c(0,0),
       horiz = T,
       cex = .73)
dev.off()

#### Quantitative Evaluations #####

## AIC ##

mean_AICweights = apply(AICweights,2,mean) ##Check mean BIC weight of each model
save(mean_AICweights, file = "Data/dataset1a/derived/mean_AICweights.Rdata")
#v_z           none          v          z       z_t0       v_t0     complex    t0 
#0.08022601 0.15017886 0.09736894 0.14654650 0.11403436 0.10362599 0.03862628 0.26939305 

best_AIC = max.col(AICweights)

best_AIC = case_when(best_AIC == 1 ~ "v-z",
                     best_AIC == 2 ~ "Simple Model",
                     best_AIC == 3 ~ "v (drift rate) Model",
                     best_AIC == 4 ~ "z (starting point) Model",
                     best_AIC == 5 ~ "z-t0",
                     best_AIC == 6 ~ "v-t0",
                     best_AIC == 7 ~ "complex",
                     best_AIC == 8 ~ "t0")
best_AIC = as.factor(best_AIC)
best_AIC = summary(best_AIC)
save(best_AIC, file = "Data/dataset1a/derived/best_AIC.Rdata")

## BIC ##

mean_BICweights = apply(BICweights,2,mean) ##Check mean BIC weight of each model averaged across participants
save(mean_BICweights, file = "Data/dataset1a/derived/mean_BICweights.Rdata")
#v_z            none           v             z          z_t0    v_t0      complex      t0 
#0.031704996 0.424263748 0.063179785 0.110303738 0.022516675 0.021236614 0.001318609 0.325475833 

best_BIC = max.col(BICweights)

best_BIC = case_when(best_BIC == 1 ~ "v-z",
                     best_BIC == 2 ~ "Simple Model",
                     best_BIC == 3 ~ "v (drift rate) Model",
                     best_BIC == 4 ~ "z (starting point) Model",
                     best_BIC == 5 ~ "z-t0",
                     best_BIC == 6 ~ "v-t0",
                     best_BIC == 7 ~ "complex",
                     best_BIC == 8 ~ "t0")
best_BIC = as.factor(best_BIC)
best_BIC = summary(best_BIC) #No. of participants for which "x" model was best according to BIC  
save(best_BIC, file = "Data/dataset1a/derived/best_BIC.Rdata")
#Simple Model                       t0              v (drift rate) Model               v-z 
# 22                                14                        1                        1 
# z (starting point) Model 
#         3 


