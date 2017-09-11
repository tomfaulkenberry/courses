# Week 3
# Psyc 5316

########################################################################
# sampling distribution for the sample mean

# let's generate a sampling distribution for n=25
# basic loop
# 1. take sample of size 25
# 2. compute the mean of the sample
# 3. store the mean in a vector
# 4. do this a bunch of times

N = 5000
Xbar=rep(0,N)
for (i in 1:N){
  Xsamp=rnorm(n=25, mean=50, sd=25)
  Xbar[i]=mean(Xsamp)
}

# let's plot the sampling distibution underneath the population
par(mfrow=c(2,1))
x=seq(-50,150,0.1)
plot(x,dnorm(x, mean=50, sd=25), type="l")
plot(density(Xbar),xlim=c(-50,150))

# compute mean and standard deviation of sampling distribution
mean(Xbar)
sd(Xbar)


#######################################################################
# computing probabilities of samples

# using raw scores
pnorm(11, mean=14, sd=1.279)

# using z-scores
pnorm(-2.345)


######################################################################
# confidence intervals

pnorm(1.645)-pnorm(-1.645)
pnorm(1.96)-pnorm(-1.96)
pnorm(2.58)-pnorm(-2.58)

# computing quantiles

qnorm(0.05)
qnorm(0.95)


#######################################################################
# T distributions
dev.off()
x=seq(-3,3,0.01)
plot(x,dnorm(x),type="l")
lines(x,dt(x,df=5),lty=2)
lines(x,dt(x,df=20),lty=3)
legend(0,0.1,c("normal","df=5","df=20"),lty=1:3)


# T quantiles for 90% CI
qt(0.05, df=21)
qt(0.95, df=21)

# 95% CI for reading scores
read=c(12,20,34,45,34,36,37,50,11,32,29)
length(read) # easy way to compute sample size
Xbar=mean(read)
s=sd(read)

c=qt(0.975,df=10) # compute 0.975 quantile

Xbar-c*s/sqrt(10) # lower limit
Xbar+c*s/sqrt(10) # upper limit

#######################################################################
# sampling from non-normal distributions

# Uniform distribution

N=5000
Xbar=rep(0,N)
for (i in 1:N){
  Xsamp=runif(n=20, min=0, max=1)
  Xbar[i]=mean(Xsamp)
}

par(mfrow=c(2,1))
x=seq(0,1,0.01)
plot(x, dunif(x, min=0, max=1), type="l")
plot(density(Xbar))


# exponential distribution

N=5000
Xbar=rep(0,N)
for (i in 1:N){
  Xsamp=rexp(n=20, rate=1)
  Xbar[i]=mean(Xsamp)
}

x=seq(0,1,0.01)
plot(x, dexp(x, rate=1), type="l")
plot(density(Xbar))


# t-scores from contaminated normal

# first, we define a function to generate a contaminated normal distribution

rcnorm <- function(n,mean,sd1,sd2,prob){
  x0 = rnorm(n, mean, sd=sd1)
  x1 = rnorm(n, mean, sd=sd2)
  flag = rbinom(n, size=1, prob)
  return(x0*(1-flag) + x1*flag)
}

# draw 5000 samples of size 20 and compute t-scores
dev.off()
N=5000
T=rep(0,N)
for (i in 1:N){
  samp=rcnorm(n=20, mean=50, sd1=3, sd2=10, prob=0.5)
  T[i]=(mean(samp)-50)/(sd(samp)/sqrt(20))
}

x=seq(-4,4,0.01)
plot(density(T))
lines(x,dt(x,df=19),lty=2) # looks pretty similar, right?

# 95% confidence intervals for Student's t:
qt(c(0.025,0.975),df=19)

# what does this equate to in OUR sampling distribution?
sum(T>-2.09 & T<2.09)/5000     


# t-scores from lognormal distribution

# some stuff that you need to make it work!
m <- 20
s <- 10
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))

# small sample: n=20
N=5000
T=rep(0,N)
for (i in 1:N){
  samp=rlnorm(n=20, meanlog=location, sdlog=shape)
  T[i]=(mean(samp)-m)/(sd(samp)/sqrt(20))
}

par(mfrow=c(2,1))
x=seq(0,50,0.01)
plot(x,dlnorm(x,meanlog=location,sdlog=shape),type="l")
plot(density(T))

quantile(T,probs=c(0.025, 0.975))
sum(T>-2.09 & T<2.09)/5000     


# large sample: n=200
N=5000
T=rep(0,N)
for (i in 1:N){
  samp=rlnorm(n=200, meanlog=location, sdlog=shape)
  T[i]=(mean(samp)-m)/(sd(samp)/sqrt(200))
}

x=seq(0,50,0.01)
plot(x,dlnorm(x,meanlog=location,sdlog=shape),type="l")
plot(density(T))


quantile(T, probs=c(0.025, 0.975))
sum(T>-2.09 & T<2.09)/5000     
