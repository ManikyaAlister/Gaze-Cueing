
for(i in 1:n.chains){
  for (s in 1:S) {
    while (weight[1,i,s]==-Inf) {
      theta[i,,s,1]=rtnorm(n=n.pars,mean=start.points,sd=start.points.sd,lower.bounds,upper.bounds)
      weight[1,i,s]=log.dens.like(theta[i,,s,1],data=data[[s]],par.names=theta.names)
    }
  }
  
  ##Make initial phis
  tmp=grep("a",phi.names)
  phi[i,tmp,1]=rtnorm(n=length(tmp),mean=2,sd=2,0,Inf)
  tmp=grep("t0",phi.names)
  phi[i,tmp,1]=rtnorm(n=length(tmp),mean=0.3,sd=0.3,0,Inf)
  tmp=grep("v",phi.names)
  phi[i,tmp,1]=rtnorm(n=length(tmp),mean=3,sd=3,-Inf,Inf)
  tmp=grep("z",phi.names)
  phi[i,tmp,1]=rtnorm(n=length(tmp),mean=0.5,sd=0.3,0,1)
  
  tmp=grep("sigma",phi.names)
  phi[i,tmp,1]=rtnorm(n=length(tmp),mean=0,sd=1,0,Inf)
}




save.image(savefile)


for(i in 2:nmc){
  if (i %% 60 == 0) cat(" ",round((i/nmc)*100,0),"%")
  
  ## Update phis
  
  phi[,,i]=phi[,,i-1]
  rand.samp=sample(1:n.chains,n.chains)
  for (p in theta.names) {
    which.theta=match(x=p,table=theta.names)
    which.phi=match(x=paste(p,c("mu","sigma"),sep="."),table=phi.names)
    if (i %% migration.freq == 0 & i > migration.start & i < migration.end) {
      phi[,,i]=migration.crossover_hyper(pars=which.phi,use.theta=theta[rand.samp,which.theta,,i-1],use.phi=phi[,,i],prior=prior[[p]],p=p)
    } else {
      phi[,,i]=t(sapply(1:n.chains,crossover_hyper,pars=which.phi,use.theta=theta[rand.samp,which.theta,,i-1],use.phi=phi[,,i],prior=prior[[p]],p=p))
    }
  }
  
  
  rand.samp=sample(1:n.chains,n.chains)
  for (s in 1:S) {
    if (i %% migration.freq == 0 & i > migration.start & i < migration.end) {
      temp=migration.crossover(pars=1:n.pars,use.theta=theta[,,s,i-1],use.like=weight[i-1,,s],data=data[[s]],hyper=phi[rand.samp,,i],par.names=theta.names)
    } else {
      temp=t(sapply(1:n.chains,crossover,pars=1:n.pars,use.theta=theta[,,s,i-1],use.like=weight[i-1,,s],data=data[[s]],hyper=phi[rand.samp,,i],par.names=theta.names))
    }
    weight[i,,s]=temp[,1]
    theta[,,s,i]=temp[,2:(n.pars+1)]
  }
}

save.image(savefile)



theta=theta[,,,burnin:nmc]
phi=phi[,,burnin:nmc]
weight=weight[burnin:nmc,,]





