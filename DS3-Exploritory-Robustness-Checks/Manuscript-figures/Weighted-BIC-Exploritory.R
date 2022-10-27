library(here)
setwd(here())

pdf("Manuscript-figures/Weighted-BIC-Exploritory.pdf",width=3*3+1,height=(4*3)+3)
#create matrix "m" which is the size of how many plots I'm going to have. 0 is where there's no plots,
m=matrix(1:12,nrow=6,byrow=T)
m=rbind(0,cbind(0,m,0),0)
m=cbind(m[,1:2],0,m[,3:4])
m=rbind(m[1:2,],0,m[3,],0,m[4:5,])#,0,m[6,],0,m[7:8,])

layout(mat=m,
       widths=c(0,1,0,1,0),
       heights=c(0,1,0,1,0,1,0,1,0))
#par(mar=rep(0,4))
par(mar=c(4,6,4,8))

S = 71 #n participants

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability", xaxt="n",yaxt="n")

load("Data/dataset3-SOA2/derived/BIC-Weights.RData")

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

#datasets = c("Dataset 1", "Dataset 2", "Dataset 3")

mtext(side=3,line=0.8,"SOA 200",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)



plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",main=,xaxt="n",yaxt="n")
load("Data/dataset3-SOA5/derived/BIC-Weights.RData")
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
par(xpd=TRUE)
legend("topleft", 
       legend = c( "Simple", "t0", "z", "v","z-v", "t0-z", "t0-v", "Complex"), 
       col = c(hcl(h=330,c=100,l=40), hcl(h=180,c=100,l=70),  hcl(h=130,c=100,l=70),hcl(h=220,c=100,l=40),hcl(h=420,c=100,l=70), hcl(h=280,c=100,l=70), hcl(h=380,c=100,l=70), hcl(h=80,c=100,l=70)),
       pch = 15,
       horiz = F, # Whether the legend is horizontal or not 
       cex = 1.5, # legend size
       inset=c(1, 0))# How far below the plot the legend appears
#text.width = 7)  

axis(side=2, at=seq(0,1,0.5), labels=seq(0,1,0.5),cex.axis=1.5)
axis(side=1, at=seq(0,S,S), labels=seq(0,S,S), cex.axis=1.5)

mtext(side=3,line=0.8,"SOA 500",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")

load("Data/dataset3-ea/derived/BIC-Weights.RData")
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


mtext(side=3,line=0.8,"Angry",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)


plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",main=,xaxt="n",yaxt="n")
load("Data/dataset3-eh/derived/BIC-Weights.RData")
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

mtext(side=3,line=0.8,"Happy",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

#par(mar=c(4.6,4.6,3,6))
plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",xaxt="n",yaxt="n")

load("Data/dataset3-en/derived/BIC-Weights.RData")

S = 70

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

S = 71
mtext(side=3,line=0.8,"Neutral",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

plot(x=100,y=100,xlim=c(0,S),ylim=c(0,1),xlab="",ylab="Probability",main=,xaxt="n",yaxt="n")
load("Data/dataset3-ef/derived/BIC-Weights.RData")
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

mtext(side=3,line=0.8,"Fearful",cex=1, font = 2)
mtext(side=1,line=0.8,"Participant",cex=.7, font = 1)

dev.off()
