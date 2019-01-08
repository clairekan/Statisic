#data cleaning
student=read.csv("Student.csv",sep=",",header=T)

student=student[,c("PartyDays","StudyHrs")]
student=na.omit(student)

PartyDays=student$PartyDays
StudyHrs=student$StudyHrs

#simple linear regression
RESULTS=lm(PartyDays~StudyHrs, data=student)
summary(RESULTS)

#correlation coefficient
cor.test(PartyDays,StudyHrs)

#ANOVA
anova(RESULTS)

#手動計算
x.mean=mean(student$StudyHrs)
y.mean=mean(student$PartyDays)

student$xx=student$StudyHrs- x.mean
student$yy=student$PartyDays - y.mean
student$xxyy=student$xx*student$yy
student$xx2= student$xx^2

##b1
b1 = sum(student$xxyy)/sum(student$xx2);b1

##SSE
RESULTS=lm(PartyDays~StudyHrs, data=student)

yhat=RESULTS$fitted.values
SSE=sum((student$PartyDays-yhat)^2);SSE

##standard error of residual
s=sqrt(SSE/(nrow(student)-2));s

#PI
PI=predict(RESULTS,data.frame(StudyHrs=c(10,20,30)),interval="prediction",level=0.95)

#CI
CI=predict(RESULTS,data.frame(StudyHrs=c(10,20,30)),interval="confidence",level=0.95)

#plotting
fit=PI[,1]
PI.low=PI[,2]
PI.high=PI[,3]

CI.low=CI[,2]
CI.high=CI[,3]

xx.test=c(10,20,30)

par(mfrow=c(1,2))#把畫布分開(列，欄)

#plot PI
plot(PartyDays~StudyHrs,data=student,pch=20,col="gray50",main="Prediction interval",xlab="study_hrs",ylab="party_days",ylim=c(-5,32),cex.main=2,cex.lab=1.2)

abline(RESULTS,col="navy")
for(i in 1:length(xx.test)){
  lines(c(xx.test[i],xx.test[i]),c(PI.low[i],PI.high[i]),col="red",lwd=3)
  points(xx.test[i],PI.low[i],col="red",pch=15)
  points(xx.test[i],PI.high[i],col="red",pch=15)
  points(xx.test[i],fit[i],col="red",pch=8)
}

#plot CI
plot(PartyDays~StudyHrs,data=student,pch=20,col="gray50",main="Confidence interval",xlab="study_hrs",ylab="party_days",ylim=c(-5,32),cex.main=2,cex.lab=1.2)

abline(RESULTS,col="navy")
for(i in 1:length(xx.test)){
  lines(c(xx.test[i],xx.test[i]),c(CI.low[i],CI.high[i]),col="red",lwd=3)
  points(xx.test[i],CI.low[i],col="red",pch=15)
  points(xx.test[i],CI.high[i],col="red",pch=15)
  points(xx.test[i],fit[i],col="red",pch=8)
}

#draw the scatter plot
dev.off()
##y vs x
plot(PartyDays~StudyHrs,pch=16,cex=1,col="navy",
     main="PartyDays vs StudyHrs",xlab="Study_Hrs",ylab="Party_Days",cex.main=2,cex.lab=1.2)

#residuals vs x
res=RESULTS$res
plot(res~StudyHrs,pch=16,cex=1,col="gold3",
     main="residual vs StudyHrs",xlab="Study_Hrs",ylab="residuals",ylim=c(-24,24),cex.main=2,cex.lab=1.2)

abline(h=0,col="red")

#histogram of residuals
par(mfrow=c(1,2))

hist(res,breaks=20,border="white",col="olivedrab3",
     main="histogram of residual", xlab="residuals",
     cex.main=2,cex.lab=1.2)

#qqplot
qqnorm(res,cex.main=2,cex.lab=1.2)
qqline(res,col="red")

