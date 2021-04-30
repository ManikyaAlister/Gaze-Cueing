
rm(list=ls())

maxLikes=array(NA,c(41,4))

for (usesub in 1:41) {
  load(paste("Fits_Simple/P",usesub,"Simple_Model.Rdata",sep=""))
  maxLikes[usesub,1]=max(weight)
  
  load(paste("Fits_z/P",usesub,"z_Model.Rdata",sep=""))
  maxLikes[usesub,2]=max(weight)
  
  load(paste("Fits_v/P",usesub,"v_Model.Rdata",sep=""))
  maxLikes[usesub,3]=max(weight)
  
  load(paste("Fits_Complex/P",usesub,"Complex_Model.Rdata",sep=""))
  maxLikes[usesub,4]=max(weight)
}




tmp1=apply(weight,2,max)
tmp2=which.max(tmp1)
tmp3=which.max(weight[,tmp2])

blah=theta[tmp2,,tmp3]

rt = P %>%
  group_by(ID, Cond) %>%
  summarise(mean = mean(Time))
