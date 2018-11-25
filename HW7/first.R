Sim_CI<-function(n,mu,sd,m,conf.level){
  
  prob<-(1-conf.level)/2
  t.value=qt(prob,df=n-1,lower.tail=FALSE)
  
  mean.x=c()
  se.x=c()
  
  CI.half=c()
  CI.Up=c()
  CI.Lower=c()
  Check=c()
  
  #Mumeanltiple sampling
  for (i in 1:m){
    
    #one sampling
    x<-rnorm(n,mean=mu,sd=sd)
    mean.x[i]<-mean(x)
    se.x[i]<-sd(x)/sqrt(n)
    
    #calculate one confidence interval
    CI.half[i]<-t.value*se.x[i]
    CI.Up[i]<-mean.x[i]+CI.half[i]
    CI.Lower[i]<-mean.x[i]-CI.half[i]
    if(CI.Up[i]>mu & CI.Lower<mu) Check[i]<-1 else Check[i]<-0
  }
  
  plot(c(CI.Up,CI.Lower),type="n",pch=19,xlim=c(1,m),xlab="Trial",ylab=expression(mu))  #n=甚麼都不畫
  abline(h=mu,col="blue")
  
  #check weather each confidence interval captures the population mean (mu)
  for (i in 1:m){
    if (Check[i]==1){
      points(i, mean.x[i], col="green", pch=10)
      points(i, CI.Up[i], col="green", pch=20)
      points(i, CI.Lower[i], col="green", pch=20)
      lines(c(i,i),c(CI.Lower[i],CI.Up[i]), col="green", pch=19)
      
    }else{
      points(i, mean.x[i], col="red", pch=10)
      points(i, CI.Up[i], col="red", pch=20)
      points(i, CI.Lower[i], col="red", pch=20)
      lines(c(i,i),c(CI.Lower[i],CI.Up[i]), col="red", pch=19)
    }
  }
  

  title(expression(paste("Simulating confidence interval for",mu)))
  
  legend("bottomright",cex=0.6,bty = "n",ncol = 2,
         c(expression(paste(mu,"Captured")),expression(paste(mu,"Not Captured"))),
         fill = c("green","red"))
  
  No.Captured = m-sum(Check)
  RESULT = list(Trial=m, Sample.Size=n,
                Population.mean=mu, Population.sd=sd,
                Confidence=conf.level,
                No.Captured=No.Captured)
  
  return(RESULT)
}

Sim_CI(n=30, m=100, mu=50, sd=5, conf.level=0.95)
  



