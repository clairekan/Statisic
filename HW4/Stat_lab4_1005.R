
# 107-1 Statistics Lab4: Random variables and probability distribution

# 1. The probability of binomial distribution by simulation
# 2. The existed binomial and normal distribution functions in R
# 3. Plotting normal distributions
# ----------------------------------------------------------------------------


# 1. The probability of binomial distribution by simulation ------------------

# Step 1. Generate one binomial random variable
Bino.event = function(n, p) {
  simp.event = c()
  pp = 10*p
  
  for (i in 1:n) {
    samp = sample(1:10, 1, replace = T)
    
    # an event with probability p
    if (samp <= pp) {
      event = 1
    } else {
      event = 0
    }
    
    simp.event[i] = event
  }
  
  x = sum(simp.event)
  #x = length(simp.event[simp.event==1]) #another way to write
  return(x)
}

Bino.event(n = 100, p = 0.3)


# Step 2. Generate a set of binomial random variable
bino.set = c()

for (i in 1:1000) {
  bino.set[i] = Bino.event(n = 100, p = 0.3)
}

hist(bino.set, prob = T, breaks = 20)


# Step 3. PDF and CDF
# PDF
bino.set==35
bino.set[bino.set==35]
length(bino.set[bino.set==35])

PDF = length(bino.set[bino.set==35]) / length(bino.set) 
PDF

# CDF
bino.set<=35
bino.set[bino.set<=35]
length(bino.set[bino.set<=35])

CDF = length(bino.set[bino.set<=35]) / length(bino.set)
CDF


# 2. The existed binomial and normal distribution functions in R ----------

# Binomial function
pbinom(35, size = 100, prob = 0.3) #given x, size, prob --> cumulative probability
dbinom(35, size = 100, prob = 0.3) #given x, size, prob --> probability

qbinom(0.8839214, size = 100, prob = 0.3) #given cumulative probability --> x
rbinom(10, size = 100, prob = 0.3) #random samples from a binomial distribution of given size, prob


# Normal distribution function
pnorm(35, mean = 30, sd = 5) #given mean, sd --> cumulative probability
dnorm(35, mean = 30, sd = 5) #given mean, sd --> density

qnorm(0.8413447, mean = 30, sd = 5) #given cumulative probability --> x
rnorm(10, mean = 30, sd = 5) #random samples from a normal distribution of given mean, sd



# 3. Plotting normal distributions -------------------------------------------

# The Xs and Ys of the plot
xx = seq(0, 10, length.out = 100)
yy = dnorm(xx, mean = 5, sd = 1.5)

# Plotting
plot(x = xx, y = yy, type = "l", main = "Normal distribution")
axis(side = 1, at = seq(1,10,by=1))

## Add a line on the plot
abline(v = 5, col = "blue")
#abline(h = 0.2, col = "red")


## Highlight an area on the plot
region.x = xx[xx>=6 & xx<=8]
region.y = yy[xx>=6 & xx<=8]

polygon(region.x, region.y, col = "yellow")

region.x2 = c(head(region.x,1), region.x, tail(region.x,1))
region.y2 = c(0, region.y, 0)
#head()
#tail()

polygon(region.x2, region.y2, col = "red")







