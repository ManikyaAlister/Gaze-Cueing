View(all.data_t0)
 blah1=all.data[[29]]
 blah2=all.data_s[[29]]
 blah1.1=quantile(blah1$Time[blah1$Resp==2 & blah1$Cond==1])
 blah1.2=quantile(blah1$Time[blah1$Resp==2 & blah1$Cond==2])
 blah2.1=quantile(blah2$Time[blah2$Resp==2 & blah2$Cond==1])
 blah2.2=quantile(blah2$Time[blah2$Resp==2 & blah2$Cond==2])
 plot(c(0,1),c(0,1))
 points(blah1.1,blah2.1)
 
plot(c(100),c(100),xlim=c(0,2),ylim=c(0,2))
 points(blah1.1,blah2.1)
 blah1.1=quantile(blah1$Time[blah1$Resp==2 & blah1$Cond==1],c(0.1,0.3,0.5,0.7,0.9))
 blah1.2=quantile(blah1$Time[blah1$Resp==2 & blah1$Cond==2],c(0.1,0.3,0.5,0.7,0.9))
 blah2.1=quantile(blah2$Time[blah2$Resp==2 & blah2$Cond==1],c(0.1,0.3,0.5,0.7,0.9))
 blah2.2=quantile(blah2$Time[blah2$Resp==2 & blah2$Cond==2],c(0.1,0.3,0.5,0.7,0.9))
 
 plot(c(100),c(100),xlim=c(0,2),ylim=c(0,2), main = "null")
 points(blah1.1,blah2.1)
 points(blah1.2,blah2.2,pch=4)
 lines(c(0,2),c(0,2),col="red")
 
 
 blah1=all.data[[29]]
 blah2=all.data_s[[29]]
 blah1.1=quantile(blah1$Time[blah1$Resp==1 & blah1$Cond==1],c(0.1,0.3,0.5,0.7,0.9))
 blah1.2=quantile(blah1$Time[blah1$Resp==1 & blah1$Cond==2],c(0.1,0.3,0.5,0.7,0.9))
 blah2.1=quantile(blah2$Time[blah2$Resp==1 & blah2$Cond==1],c(0.1,0.3,0.5,0.7,0.9))
 blah2.2=quantile(blah2$Time[blah2$Resp==1 & blah2$Cond==2],c(0.1,0.3,0.5,0.7,0.9))
 plot(c(100),c(100),xlim=c(0,2),ylim=c(0,2), main = "null")
 points(blah1.1,blah2.1)
 points(blah1.2,blah2.2,pch=4)
 lines(c(0,2),c(0,2),col="red")
 
 blah1=all.data[[29]]
 blah2=all.data_t0[[29]]
 blah1.1=quantile(blah1$Time[blah1$Resp==1 & blah1$Cond==1],c(0.1,0.3,0.5,0.7,0.9))
 blah1.2=quantile(blah1$Time[blah1$Resp==1 & blah1$Cond==2],c(0.1,0.3,0.5,0.7,0.9))
 blah2.1=quantile(blah2$Time[blah2$Resp==1 & blah2$Cond==1],c(0.1,0.3,0.5,0.7,0.9))
 blah2.2=quantile(blah2$Time[blah2$Resp==1 & blah2$Cond==2],c(0.1,0.3,0.5,0.7,0.9))
 plot(c(100),c(100),xlim=c(0,2),ylim=c(0,2), main = "t0")
 points(blah1.1,blah2.1)
 points(blah1.2,blah2.2,pch=4)
 lines(c(0,2),c(0,2),col="red")


