rm(list = ls())
library(tidyverse)
n = 41
S = n
setwd("~/Dropbox/2021/Gaze-Cueing")

#n_datasets = 1
#models = c(`_all-params`) 
#for (dataset in 1:n_datasets) {
######## Load AIC and BIC for Each Model #######
#Or Skip to lines 83/84 if already saved
#---------------------------------------------------------------------------------------------------
## Complex model ##

# Create blank vectors so the loop knows what to fill in

AIC_C = NULL
BIC_C = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { 
  load(paste("Modelling/07_Outputs/P",p,"Complex_Model.RData", sep = ""))
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
  load(paste("Modelling/07_Outputs/P",p,"Simple_Model.RData", sep = ""))
  AIC_S = rbind(AIC, AIC_S)
  BIC_S = rbind(BIC, BIC_S)
}


#---------------------------------------------------------------------------------------------------
## v Model ##

AIC_v = NULL
BIC_v = NULL 

for (p in 1:n) { 
  load(paste("Modelling/07_Outputs/P",p,"V_Model.RData", sep = ""))
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
  load(paste("Modelling/07_Outputs/P",p,"z_Model.RData", sep = ""))
  AIC_z = rbind(AIC, AIC_z)
  BIC_z = rbind(BIC, BIC_z)
}
#save(AIC_z, file = "Comparisons/AIC_z.RData")
#save(BIC_z, file = "Comparisons/BIC_z.RData")
#---------------------------------------------------------------------------------------------------
## t0 Model ##

AIC_t0 = NULL
BIC_t0 = NULL 

for (p in 1:n) { 
  load(paste("Modelling/07_Outputs/DS1_P",p,"_t0Model.RData", sep = ""))
  AIC_t0 = rbind(AIC, AIC_t0)
  BIC_t0 = rbind(BIC, BIC_t0)
}
#---------------------------------------------------------------------------------------------------
## z-t0 model##

AIC_z_t0 = NULL
BIC_z_t0 = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Modelling/07_outputs/DS1_P",p,"_z-t0_Model.RData", sep = ""))
  AIC_z_t0 = rbind(AIC, AIC_z_t0)
  BIC_z_t0 = rbind(BIC, BIC_z_t0)
}

#---------------------------------------------------------------------------------------------------
## v-t0 Model ##

AIC_v_t0 = NULL
BIC_v_t0 = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Modelling/07_Outputs/DS1_P",p,"_v-t0_Model.RData", sep = ""))
  AIC_v_t0 = rbind(AIC, AIC_v_t0)
  BIC_v_t0 = rbind(BIC, BIC_v_t0)
}

#---------------------------------------------------------------------------------------------------
## all-parameters model##

AIC_ap = NULL
BIC_ap = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:n) { #Loop in each data set
  load(paste("Modelling/07_Outputs/DS1_P",p,"_all-params_Model.RData", sep = ""))
  AIC_ap = rbind(AIC, AIC_ap)
  BIC_ap = rbind(BIC, BIC_ap)
}

#---------------------------------------------------------------------------------------------------
#Compare AIC of each model for each participant 

AIC_comp = cbind(AIC_C, AIC_S, AIC_v, AIC_z, AIC_z_t0, AIC_v_t0, AIC_ap, AIC_t0)
colnames(AIC_comp) = c("complex", "none", "v", "z", "_z_t0", "v_t0", "all-params", "t0")

BIC_comp = cbind(BIC_C, BIC_S, BIC_v, BIC_z, BIC_z_t0, BIC_v_t0, BIC_ap, BIC_t0)
colnames(BIC_comp) = c("complex", "none", "v", "z", "_z_t0", "v_t0", "all-params", "t0")

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

BFweights=array(NA,c(S,8))
for (s in 1:S) {
  BFweights[s,]= getWeights(BIC_comp[s,])
}
colnames(BFweights) = c("complex", "none", "v", "z", "_z_t0", "v_t0", "all_params", "t0")


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
  
  col=hcl(h=180,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BFweights[use.i,"t0"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"t0"] 
  
  col=hcl(h=280,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BFweights[use.i,"_z_t0"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"_z_t0"] 
  
  col=hcl(h=380,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BFweights[use.i,"v_t0"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"v_t0"] 
  
  col=hcl(h=80,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+BFweights[use.i,"all_params"], border = col, col = col)
  sumThing=sumThing+BFweights[use.i,"all_params"] 
  
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", 
       legend = c("z-v", "Simple", "v", "z", "t0", "_z_t0", "v_t0", "all-params"), 
       col = c(hcl(h=420,c=100,l=70),hcl(h=330,c=100,l=40), hcl(h=220,c=100,l=40), hcl(h=130,c=100,l=70), hcl(h=180,c=100,l=70), hcl(h=280,c=100,l=70), hcl(h=380,c=100,l=70), hcl(h=80,c=100,l=70)),
       pch = 15,
       horiz = T,
       cex = .55,
       inset=c(0,.0))
       
########## AIC ###########

AICweights=array(NA,c(S,8))
for (s in 1:S) {
  AICweights[s,]= getWeights(AIC_comp[s,])
}
colnames(AICweights) = c("complex", "none", "v", "z", "_z_t0", "v_t0", "all_params", "t0")


#save(AICweights, file = "Data/BF-Weights.RData")
#save(BIC_comp, file = "Data/BICs.RData")

#If I want to order by V model 
# AICweights = AICweights[order(AICweights[,4]),]

# Plot

S = 41 #n participants
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="AIC",xaxt="n",yaxt="n")

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
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"complex"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"complex"] #Blue 
  
  col=hcl(h=180,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"t0"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"t0"] 
  
  col=hcl(h=280,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"_z_t0"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"_z_t0"] 
  
  col=hcl(h=380,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"v_t0"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"v_t0"] 
  
  col=hcl(h=80,c=100,l=70)
  rect(i-0.5,sumThing,i+0.5,sumThing+AICweights[use.i,"all_params"], border = col, col = col)
  sumThing=sumThing+AICweights[use.i,"all_params"] 
  
}
axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)
legend("bottom", 
       legend = c("z-v", "Simple", "v", "z", "t0", "_z_t0", "v_t0", "all-params"), 
       col = c(hcl(h=420,c=100,l=70),hcl(h=330,c=100,l=40), hcl(h=220,c=100,l=40), hcl(h=130,c=100,l=70), hcl(h=180,c=100,l=70), hcl(h=280,c=100,l=70), hcl(h=380,c=100,l=70), hcl(h=80,c=100,l=70)),
       pch = 15,
       horiz = T,
       cex = .55,
       inset=c(0.3,.0))

####################################
#####Inclusion Probability #########
####################################

### BIC ####

#### z #####

#To get a inclusion probability for z, we just add the models that have z and don’t have z:
  
  plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="z Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BFweights[use.i,c("none","v","t0", "v_t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=40)
  currWeight=sum(BFweights[use.i,c("z","complex", "all_params", "_z_t0")])
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

##### v ######
#And same principle for v:
  
  plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="v Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=1)
  currWeight=sum(BFweights[use.i,c("none","z", "_z_t0", "t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BFweights[use.i,c("v","complex", "v_t0", "all_params")])
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


#### t0 #####
#And same principle for t0:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="t0 Inclusion Probability BIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(BFweights[use.i,c("none","z", "complex", "v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(BFweights[use.i,c("t0","_z_t0", "v_t0", "all_params")])
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

### AIC ####

#### z #####

#To get a inclusion probability for z, we just add the models that have z and don’t have z:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="z Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(AICweights[use.i,c("none","v","t0", "v_t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=40)
  currWeight=sum(AICweights[use.i,c("z","complex", "all_params", "_z_t0")])
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

##### v ######
#And same principle for v:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="v Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=1)
  currWeight=sum(AICweights[use.i,c("none","z", "_z_t0", "t0")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(AICweights[use.i,c("v","complex", "v_t0", "all_params")])
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


#### t0 #####
#And same principle for t0:

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="",main="t0 Inclusion Probability AIC",xaxt="n",yaxt="n")
for (i in 1:S) {
  use.i=i
  sumThing=0
  col=hcl(h=330,c=100,l=0)
  currWeight=sum(AICweights[use.i,c("none","z", "complex", "v")])
  rect(i-0.5,0,i+0.5,currWeight, border = col, col = col)
  sumThing=sumThing+currWeight
  col=hcl(h=110,c=100,l=60)
  currWeight=sum(AICweights[use.i,c("t0","_z_t0", "v_t0", "all_params")])
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
