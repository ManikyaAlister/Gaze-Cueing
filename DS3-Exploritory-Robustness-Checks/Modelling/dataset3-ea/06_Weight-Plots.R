rm(list = ls())

lib = .libPaths("~/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
library(here, lib.loc = lib)
library(dplyr, lib.loc = lib)
## Dataset  3 ##
######## Load AIC and BIC for Each Model #######
#Or Skip to lines 83/84 if already saved
#---------------------------------------------------------------------------------------------------
## v_z model ##

dataset = "dataset3-ea"
S = 71 #subjects

# Create blank vectors so the loop knows what to fill in
AIC_v_z = NULL
BIC_v_z = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { 
  load(here(paste("Modelling/",dataset,"/07_Outputs/P",p,"_v-z_Model.Rdata", sep = "")))
  AIC_v_z = rbind(AIC, AIC_v_z)
  BIC_v_z = rbind(BIC, BIC_v_z)
  
}

#save(AIC_C, file = "Data/dataset1a/derived/AIC_C.Rdata") #To save for each individual model if wanted (copy for each model)
#save(BIC_C, file = "Data/dataset1a/derived/BIC_C.Rdata")
#Repeat for other models
#---------------------------------------------------------------------------------------------------
## Simple Model ##
AIC_S = NULL
BIC_S = NULL 

for (p in 1:S) { 
  load(here(paste("Modelling/",dataset,"/07_Outputs/P",p,"Simple_Model.Rdata", sep = "")))
  AIC_S = rbind(AIC, AIC_S)
  BIC_S = rbind(BIC, BIC_S)

}

#---------------------------------------------------------------------------------------------------
## v Model ##

AIC_v = NULL
BIC_v = NULL 

for (p in 1:S) { 
  load(here(paste("Modelling/",dataset,"/07_Outputs/P",p,"_v_Model.Rdata", sep = "")))
  AIC_v = rbind(AIC, AIC_v)
  BIC_v = rbind(BIC, BIC_v)
}

#---------------------------------------------------------------------------------------------------
## z Model ##

AIC_z = NULL
BIC_z = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(here(paste("Modelling/",dataset,"/07_Outputs/P",p,"_z_Model.Rdata", sep = "")))
  AIC_z = rbind(AIC, AIC_z)
  BIC_z = rbind(BIC, BIC_z)
}
#---------------------------------------------------------------------------------------------------
## t0 Model ##

AIC_t0 = NULL
BIC_t0 = NULL 

for (p in 1:S) { 
  load(here(paste("Modelling/",dataset,"/07_Outputs/P",p,"_t0_Model.Rdata", sep = "")))
  AIC_t0 = rbind(AIC, AIC_t0)
  BIC_t0 = rbind(BIC, BIC_t0)
}
#---------------------------------------------------------------------------------------------------
## z_t0 model##

AIC_z_t0 = NULL
BIC_z_t0 = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(here(paste("Modelling/",dataset,"/07_outputs/P",p,"_z-t0_Model.Rdata", sep = "")))
  AIC_z_t0 = rbind(AIC, AIC_z_t0)
  BIC_z_t0 = rbind(BIC, BIC_z_t0)
}

#---------------------------------------------------------------------------------------------------
## v-t0 Model ##

AIC_v_t0 = NULL
BIC_v_t0 = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(here(paste("Modelling/",dataset,"/07_Outputs/P",p,"_v-t0_Model.Rdata", sep = "")))
  AIC_v_t0 = rbind(AIC, AIC_v_t0)
  BIC_v_t0 = rbind(BIC, BIC_v_t0)
}

#---------------------------------------------------------------------------------------------------
## all-parameters model##

AIC_C = NULL
BIC_C = NULL 

#Loop in each data set and combine to create a vector of AIC and BIC respectively for each participant
for (p in 1:S) { #Loop in each data set
  load(here(paste("Modelling/",dataset,"/07_Outputs/P",p,"_complex_Model.Rdata", sep = "")))
  AIC_C = rbind(AIC, AIC_C)
  BIC_C = rbind(BIC, BIC_C)
}

#---------------------------------------------------------------------------------------------------
#Compare AIC of each model for each participant 

AIC_comp = cbind(AIC_v_z, AIC_S, AIC_v, AIC_z, AIC_z_t0, AIC_v_t0, AIC_C, AIC_t0)
colnames(AIC_comp) = c("v_z", "none", "v", "z", "z_t0", "v_t0", "complex", "t0")

BIC_comp = cbind(BIC_v_z, BIC_S, BIC_v, BIC_z, BIC_z_t0, BIC_v_t0, BIC_C, BIC_t0)
colnames(BIC_comp) = c("v_z", "none", "v", "z", "z_t0", "v_t0", "complex", "t0")

# This method loads everything with the rows in reverse order so the following code amends this so it's in the correct order 

AIC_comp = AIC_comp[nrow(AIC_comp):1,]
BIC_comp = BIC_comp[nrow(AIC_comp):1,]

# Save

save(AIC_comp, file = here(paste0("Data/",dataset,"/derived/AICs.Rdata")))
save(BIC_comp, file = here(paste0("Data/",dataset,"/derived/BICs.Rdata")))

###########################
### Model Probabilities ##
###########################

load(here(paste0("Data/",dataset,"/derived/BICs.Rdata")))
load(here(paste0("Data/",dataset,"/derived/AICs.Rdata")))

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

save(participants_alt, file = here(paste0("Data/",dataset,"/derived/participants_alt.Rdata")))
save(participants_null, file = here(paste0("Data/",dataset,"/derived/participants_null.Rdata")))

save(BICweights, file = here(paste0("Data/",dataset,"/derived/BIC-Weights.Rdata")))
save(BIC_comp, file = here(paste0("Data/",dataset,"/derived/BICs.Rdata")))

#If I want to order by V model 
# BICweights = BICweights[order(BICweights[,4]),]

# Plot

S = 71 #n participants

png(here(paste0("Modelling/",dataset,"/08_Plots/weighted-prob-BIC.png")))
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


save(AICweights, file = here(paste0("Data/",dataset,"/derived/AIC-Weights.Rdata")))
save(BIC_comp, file = here(paste0("Data/",dataset,"/derived/BICs.Rdata")))

#If I want to order by V model 
# AICweights = AICweights[order(AICweights[,4]),]

# Plot

S = 71 #n participants
png(here(paste0("Modelling/",dataset,"/08_Plots/weighted-prob-AIC.png")))
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
png(here(paste0("Modelling/",dataset,"/08_Plots/inclusion-prob-BIC-z.png")))  
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
png(here(paste0("Modelling/",dataset,"/08_Plots/inclusion-prob-BIC-v.png")))
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
png(here(paste0("Modelling/",dataset,"/08_Plots/inclusion-prob-BIC-t0.png")))
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
png(here(paste0("Modelling/",dataset,"/08_Plots/inclusion-prob-AIC-z.png")))  
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
png(here(paste0("Modelling/",dataset,"/08_Plots/inclusion-prob-AIC-v.png")))  
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
png(here(paste0("Modelling/",dataset,"/08_Plots/inclusion-prob-AIC-t0.png"))) 
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

# Calculate mean AIC weight of each model averaged across participants 

mean_AICweights = round(sort(apply(AICweights,2,mean), decreasing = TRUE),2) 
save(mean_AICweights, file = here(paste0("Data/",dataset,"/derived/mean_AICweights.Rdata")))

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
save(best_AIC, file = here(paste0("Data/",dataset,"/derived/best_AIC.Rdata")))

## Calculate mean raw AICs

raw_AIC = round(apply(AIC_comp, 2, mean),2)

## BIC ##

#Calculate mean BIC weight of each model averaged across participants
mean_BICweights = round(sort(apply(BICweights,2,mean), decreasing = TRUE),2)
save(mean_BICweights, file = here(paste0("Data/",dataset,"/derived/mean_BICweights.Rdata")))

best_BIC = max.col(BICweights)

#Calculate no. of participants for which "x" model was best according to BIC  
best_BIC = case_when(best_BIC == 1 ~ "v-z",
                     best_BIC == 2 ~ "Simple Model",
                     best_BIC == 3 ~ "v (drift rate) Model",
                     best_BIC == 4 ~ "z (starting point) Model",
                     best_BIC == 5 ~ "z-t0",
                     best_BIC == 6 ~ "v-t0",
                     best_BIC == 7 ~ "complex",
                     best_BIC == 8 ~ "t0")
best_BIC = as.factor(best_BIC)
best_BIC = summary(best_BIC) 
save(best_BIC, file = here(paste0("Data/",dataset,"/derived/best_BIC.Rdata")))

## Calculate mean raw BIC scores

raw_BIC = round(apply(BIC_comp, 2, mean),2)


## Make a table that I can paste straight into the latex document

library(xtable)

# convert AIC and BIC weights to percentages for manuscript table

mean_BICweights_perc = mean_BICweights*100
mean_AICweights_perc = mean_AICweights*100

latex_table = cbind(
  mean_BICweights_perc[order(factor(names(mean_BICweights_perc), levels = c('none', 't0', 'z', 'v', 'v_z', 'v_t0','z_t0','complex')))],
  raw_BIC[order(factor(names(raw_BIC), levels = c('none', 't0', 'z', 'v', 'v_z', 'v_t0','z_t0','complex')))],
  mean_AICweights_perc[order(factor(names(mean_AICweights_perc), levels = c('none', 't0', 'z', 'v', 'v_z', 'v_t0','z_t0','complex')))],
  raw_AIC[order(factor(names(raw_AIC), levels = c('none', 't0', 'z', 'v', 'v_z', 'v_t0','z_t0','complex')))]
)
rownames(latex_table) = c('Simple', 't0', 'z', 'v', 'v-z', 't0-v','t0-z','Complex')
colnames(latex_table) = c("BIC Probability (%)","BIC Raw","AIC Probability (%)","AIC Raw")

class(latex_table)

xtable::xtable(latex_table, digits = c(0,0,2,0,2)) #Paste output (from "Simple" to "Complex" straight into Latex)

### Parameter Inclusion Probabilities ###

# Inclusion probability AIC
AIC_prob_z = NULL
AIC_prob_v = NULL
AIC_prob_t0 = NULL

#Combine the probability of each model that assumes each respective parameter varies 
for (i in 1:S) {
  AIC_prob_z[i]=sum(AICweights[i,c("z","v_z", "complex", "z_t0")])
  AIC_prob_v[i]=sum(AICweights[i,c("v","v_z", "complex", "v_t0")])
  AIC_prob_t0[i]=sum(AICweights[i,c("t0","z_t0", "complex", "v_t0")])
}
AIC_incl_prob = cbind(AIC_prob_z, AIC_prob_v, AIC_prob_t0)
AIC_incl_prob = apply(AIC_incl_prob, 2, mean) #Average across all participants 

# Inclusion probability BIC
BIC_prob_z = NULL
BIC_prob_v = NULL
BIC_prob_t0 = NULL

#Combine the probability of each model that assumes each respective parameter varies 
for (i in 1:S) {
  BIC_prob_z[i]=sum(BICweights[i,c("z","v_z", "complex", "z_t0")])
  BIC_prob_v[i]=sum(BICweights[i,c("v","v_z", "complex", "v_t0")])
  BIC_prob_t0[i]=sum(BICweights[i,c("t0","z_t0", "complex", "v_t0")])
}
BIC_incl_prob = cbind(BIC_prob_z, BIC_prob_v, BIC_prob_t0)
BIC_incl_prob_mean = round(apply(BIC_incl_prob, 2, mean),2) #Average across all participants 

best_BIC_incl = max.col(BIC_incl_prob)
best_BIC_incl = case_when(
  best_BIC_incl == 1 ~ "z",
  best_BIC_incl == 2 ~ "v",
  best_BIC_incl == 3 ~ "t0")
best_BIC_incl = as.factor(best_BIC_incl)
best_BIC_incl = summary(best_BIC_incl)

# Calculate how many participants had more than 50% prob. of being best described by a model allowing x parameter to vary
t0.5 = sum((BICweights[,"t0"]>.5)==T)
z.5 = sum((BICweights[,"z"]>.5)==T)
v.5 = sum((BICweights[,"v"]>.5)==T)



