
# Lab13: Correlation analysis - Categorical variables

rm(list = ls())

# Read data
setwd("D:/OneDrive/文件/統計學")

data = "heartatk.csv"
Heart_Table = read.table(data, header=TRUE, sep=",")
head(Heart_Table)


# Generate a two-way table
MyTable = xtabs(~ SEX + DIED, data = Heart_Table) # generate a 2x2 table(列連表)
MyTable

# total number of observations:
sum(MyTable)



# 1. Risk and Odds --------------------------------------------------------

MyTable


# 1(1) By manual calculation: 

## risk
risk.F = MyTable[1,2] / sum(MyTable[1,]); risk.F
risk.M = MyTable[2,2] / sum(MyTable[2,]); risk.M

## relative risk
relarisk.M_F = risk.M / risk.F; relarisk.M_F  #base: Female

## percent increase in risk
incrisk.M_F = (rela.risk.M_F-1); incrisk.M_F
incrisk.M_F = (risk.M-risk.F) / risk.F; incrisk.M_F

## odds
odds.F = MyTable[1,2] / MyTable[1,1]; odds.F
odds.M = MyTable[2,2] / MyTable[2,1]; odds.M

## odds ratio
oddsratio.M_F = odds.M / odds.F; oddsratio.M_F  #base: Female
oddsratio.F_M = odds.F / odds.M; oddsratio.F_M  #base: Male


# 1(2) By functions: 
#install.packages("epitools")
library(epitools)

epitab(MyTable, method = "riskratio")  #relative risk
relarisk.M_F = risk.M / risk.F; relarisk.M_F  #base: Female

epitab(MyTable, method = "oddsratio")  #odds ratio
oddsratio.M_F = odds.M / odds.F; oddsratio.M_F  #base: Female

epitab(MyTable, method = "oddsratio", rev = "row")  #change the base to M
oddsratio.F_M = odds.F / odds.M; oddsratio.F_M  #base: Male



# 2. Chi-square test for two-way table ------------------------------------
#    (homogeneity test, independence test)

# 2(1) By manual calculation: 

# row total, column total, total
total.0 = sum(MyTable[,1]); total.0
total.1 = sum(MyTable[,2]); total.1
total.F = sum(MyTable[1,]); total.F
total.M = sum(MyTable[2,]); total.M

total = sum(MyTable)

# generate an expected table
ExpTable = MyTable
ExpTable[1,1] = total.0*total.F / total
ExpTable[1,2] = total.1*total.F / total
ExpTable[2,1] = total.0*total.M / total
ExpTable[2,2] = total.1*total.M / total

ExpTable


# calculate chi-square statistic
chi.11 = ((MyTable[1,1] - ExpTable[1,1])^2) / ExpTable[1,1]
chi.12 = ((MyTable[1,2] - ExpTable[1,2])^2) / ExpTable[1,2]
chi.21 = ((MyTable[2,1] - ExpTable[2,1])^2) / ExpTable[2,1]
chi.22 = ((MyTable[2,2] - ExpTable[2,2])^2) / ExpTable[2,2]

chi.square = sum(chi.11, chi.12, chi.21, chi.22); chi.square


# p-value
df = (nrow(MyTable)-1) * (ncol(MyTable)-1); df

leftp = pchisq(chi.square, df = df)
p.value = 1-leftp
p.value



# 2(2) By functions: 
RESULTS = chisq.test(MyTable, correct = F) #F:不需要調種整
RESULTS

# expected table
exp.table = RESULTS$expected
exp.table


##z test
prop.test(x = c(767,643), n = c(5065,7779), alternative = "two.sided", correct = F)



# 3. Goodness-of-fit test: Testing uniform distribution ---------------------------

## step1:

## step2:

#observed values
obs = c(47,50,55,46,53,39,55,55,44,56)
n = 500#上面加起來
p = 0.1#平均分配

#expected values
exp = rep(n*p, length = length(obs))#算期望值

tbl = data.frame(obs,exp)

#check conditions

#calculate the chi-square statistic
tbl$chii = (tbl$obs-tbl$exp)^2/ tbl$exp
chi.square = sum(tbl$chii); chi.square


## step3:
df = length(obs) - 1
p.value =  1 - pchisq(chi.square, df = df); p.value

## step4: 
alpha = 0.05
if (p.value <= alpha) {
  print("Reject H0.")
} else {
  print("Do not reject H0.")
}

## step5:



# By chisq.test()
chisq.test(tbl$obs, p = rep(0.1,length = 10), correct = F)





# HW1 hint: Write a function for Chi-square test (2*2) ---------------------------

MyChiSq <- function(table) {
  
  
  
  # generate an expected table
  
  
  
  # calculate each (observed-expected)^2/expected
  # hint: You can use 2 for-loops to specify a single cell in the table
  
  
  
  # calculate Chi-square statistics
  
  
  
  # calculate p-value
  # hint: pchisq()
  
  
  
  # results of the function
  # hint: results = list(X.squared = Chi, df = df, p.value = p.value)
  # hint: return(results)
  
  
}


MyChiSq(MyTable)


# HW2 hint: testing binomial distribution ----------------------------------


###**step1**

#####H0:一周內的降雨日數符合二項分配

#####Ha:一周內的降雨日數不符合二項分配

##**step2**
x = c(0,1,2,3,4,5,6,7)

#observed values
obs = c(5,13,26,19,20,7,0,0)
n = 90*7
pp =sum(x*obs)/n


#expected values
exp = c()
for (i in 1:length(obs)) {
  exp[i] = dbinom(x[i],size=7,pp) * 90
}#(一周間下雨的期望天數)*90周

tbl = data.frame(x, obs, exp)

#check conditions

#####發現並沒有所有的row都符合期望值>1，所以做一些調整
#combining 6,7,8 rows
tbl.678 = tbl[6,] + tbl[7,] + tbl[8,] #add up 6,7,8 rows
tbl = tbl[-c(6,7,8),] #remove 6,7,8 rows
tbl = rbind(tbl, tbl.678) #add the combined row

#####這樣就有符合所有的row都符合期望值>1而且80%以上都>5的條件

###**step3**

#calculate the chi-square statistic
chi=0
for (i in 1:6){
 
    chi= chi+(tbl[i,2]-tbl[i,3])^2/tbl[i,3]
  
}
paste("chi.square:",chi)

df = nrow(tbl)-1-1 #k-1-r
p_value = pchisq(chi, df = df, lower.tail = F)
paste("p-value:",p_value)

###**step4**

#####alpha = 0.05<p-value，無法拒絕H0


###**step5**

#####無法拒絕一周內的降雨日數符合二項分配，也就是當我們將一周下6天跟下7天(從未發生過)的資料做過處理後，一周內的降雨日數可能符合二項分配




# HW3 hint: Testing normal distribution ---------------------------------------------


###**step1**

#####H0:每月平均PM2.5的濃度值呈現常態分佈
#####Ha:每月平均PM2.5的濃度值不呈現常態分佈


## step2:
pm2.5 = c(18.8, 14.6, 14.0, 15.8, 12.4, 13.2, 16.1, 13.8, 16.2, 
          16.1, 17.8, 18.7, 15.8, 13.3, 13.6, 16.4, 13.8, 16.6, 
          15.3, 19.0, 18.4, 15.0, 18.8, 18.1, 17.3, 16.3, 17.5, 
          18.1, 14.2, 18.0, 13.0, 13.3, 12.4, 16.6, 14.1, 20.6, 
          16.8, 13.3, 18.2, 16.9)


#defining thresholds to categorize continuous data
mean = mean(pm2.5)
sd = sd(pm2.5)

n = length(pm2.5)

thres = c(mean-3*sd, mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd, mean+3*sd)


#observed values (continuous --> discrete)
obs = rep(0, length = 8)

for (i in 1:length(pm2.5)) {
  if (pm2.5[i]<thres[1]) {
    obs[1]= obs[1]+1
  } else if (thres[1]<=pm2.5[i]&pm2.5[i]<thres[2]) {
    obs[2]= obs[2]+1
  } else if (thres[2]<=pm2.5[i]&pm2.5[i]<thres[3]) {
    obs[3]= obs[3]+1
  } else if (thres[3]<=pm2.5[i]&pm2.5[i]<thres[4]) {
    obs[4]= obs[4]+1
  } else if (thres[4]<=pm2.5[i]&pm2.5[i]<thres[5]) {
    obs[5]= obs[5]+1
  } else if (thres[5]<=pm2.5[i]&pm2.5[i]<thres[6]) {
    obs[6]= obs[6]+1
  } else if (thres[6]<=pm2.5[i]&pm2.5[i]<thres[7]) {
    obs[7]= obs[7]+1
  } else {
    obs[8]= obs[8]+1
  }
}


#expected values
cumu.p = c(pnorm(thres[1],mean=mean,sd=sd), 
           pnorm(thres[2],mean=mean,sd=sd) - pnorm(thres[1],mean=mean,sd=sd), 
           pnorm(thres[3],mean=mean,sd=sd) - pnorm(thres[2],mean=mean,sd=sd), 
           pnorm(thres[4],mean=mean,sd=sd) - pnorm(thres[3],mean=mean,sd=sd), 
           pnorm(thres[5],mean=mean,sd=sd) - pnorm(thres[4],mean=mean,sd=sd), 
           pnorm(thres[6],mean=mean,sd=sd) - pnorm(thres[5],mean=mean,sd=sd),
           pnorm(thres[7],mean=mean,sd=sd) - pnorm(thres[6],mean=mean,sd=sd),
           1-pnorm(thres[7],mean=mean,sd=sd)
           )

exp = cumu.p * n


cate = c("< m-3d", "m-3d ~ m-2d", "m-2d ~ m-d", "m-d ~ m", "m ~ m+d", "m+d ~ m+2d", "m+2d ~ m+3d", "> m+3d")

tbl = data.frame(obs, exp)
rownames(tbl) = cate

#####check conditions

#####發現並沒有所有的row都符合期望值>1，所以做一些調整

tbl.123= tbl[1,] + tbl[2,] + tbl[3,] #add up 6,7,8 rows
tbl = tbl[-c(1,2,3),] #remove 6,7,8 rows
tbl = rbind(tbl, tbl.123) #add the combined row

tbl.345 = tbl[3,] + tbl[4,] + tbl[5,] #add up 6,7,8 rows
tbl = tbl[-c(3,4,5),] #remove 6,7,8 rows
tbl = rbind(tbl, tbl.345) #add the combined row

#####這樣就有符合所有的row都符合期望值>1而且80%以上都>5的條件

###**step3**

chi=0
for (i in 1:4){
  
  chi= chi+(tbl[i,1]-tbl[i,2])^2/tbl[i,2]
  
}
paste("chi.square:",chi)

df = nrow(tbl)-1-2  # k-1-r
p_value = pchisq(chi, df = df, lower.tail = F)
paste("p-value:",p_value)



###**step4**
alpha = 0.05>p-value=0.031

#####足夠拒絕H0

###**step5**

#####有顯著的證據可以推翻平均值呈現常態分佈的假設，代表很可能PM2.5的分布不是常態分配













