setwd("")
#gerwd()
stu=read.csv("Student.csv",sep=",",header=T)

#---------------------------------------------------

head(stu)

PartyDays=stu$PartyDays
StudyHrs=stu$StudyHrs

#scatterplot
plot(PartyDays~StudyHrs,pch=16,cex=1,col="navy",main="PartyDays vs StudyHrs",xlab="Study_Hrs",ylab="Party_Days")

#or
plot(PartyDays,StudyHrs,pch=16,cex=1,col="navy",main="PartyDays vs StudyHrs",xlab="Study_Hrs",ylab="Party_Days")

#correlation coefficient
cor.test(PartyDays,StudyHrs)

#Is there any NA value in the data?
length(PartyDays[is.na(PartyDays)])
length(StudyHrs[is.na(StudyHrs)])

#simple linear regression
RESULTS=lm(PartyDays~StudyHrs)
summary(RESULTS)     #same as above

coeff=coefficients(RESULTS)
RESULTS$coefficients

res=residuals(RESULTS)
RESULTS$residuals

yhat=fitted.values(RESULTS) #estimated y (yhat)
RESULTS$fitted.values


dev.off()   #清理畫圖區

plot(PartyDays~StudyHrs,pch=16,col="blue",main="PartyDays vs StudyHrs",xlab="Study_Hrs",ylab="Party_Days")

abline(RESULTS,col="red")

#anova
anova(RESULTS)
