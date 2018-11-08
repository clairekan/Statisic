
# 107-1 Statistics Lab6: Estimating proportions with confidence
# confd=信賴區間
Prop_CI = function(n, p, confd) {
 
  all.phat = c()
  for (i in 1:10000) {
    phat = sum(sample(c(0,1), n, replace = T, prob = c(1-p,p)))/n
    all.phat[i] = phat
  }
  
  all.phat = sort(all.phat)#從小排到大
  
  low.per = ceiling(((1-confd)/2)*10000)#下界；eiling:四捨五入
  low.vlue = all.phat[low.per]
  high.per = ceiling((1-((1-confd)/2))*10000)#上界
  high.vlue = all.phat[high.per]
  
  results = c(paste((1-confd)/2*100,"%"), paste((1-((1-confd)/2))*100,"%"), low.vlue, high.vlue)
  
  mean.p = mean(all.phat)
  sd.p = sd(all.phat)
  
  CI = list(CI = matrix(results, nrow = 2, byrow = T), mean = mean.p, sd = sd.p )
  
  return(CI)
}

Prop_CI(883,0.68,0.95)


# By calculation
n = 883
p = 0.68

z = qnorm((1-0.95)/2, lower.tail = T)
se = sqrt(p*(1-p)/n)

high.c = p - z*se; high.c
low.c = p +z*se; low.c

prop.test(x=a,n=1006,alternative="two sides",conf.level=0.95)
#difference
prop.test(x=c(86,24),n=c(1105,1379),alternative="two.sided",conf.level=0.95)
