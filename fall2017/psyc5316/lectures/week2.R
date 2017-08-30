# Week 2 -- maximum likelihood estimation


# probability versus likelihood

x = 1:20
y = dbinom(x, size = 20, prob=0.7)
barplot(y,names.arg = x, xlab="x", ylab="p(x)")

# check that y is a probability distribution
sum(y)



theta = seq(from=0, to=1, by=0.01)
y = dbinom(12, size=20, prob=theta)
plot(theta,y,type="l")

# check that y is a probability distribution
sum(y) # NO!!
# this plot is a "likelihood" function


# Goal: find the maximum likelihood for given data.  This is called 
# "Maximum Likelihood Estimate" (MLE)


######################################################
# example: find MLE for binomial data
#
# suppose on 20 coin flips, we see 12 successes.  What is theta?

# Step 1: define "negative log likelihood" function (NLL)

nll.binom <- function(data,par){
  return(-log(dbinom(data, size = 20, prob = par)))
}

# Step 2: find the parameter (theta) which *minimizes* the NLL

optim(par=0.5, fn=nll.binom, data=12) # look at "par" output..this is the MLE



#####################################################
# example 2: find MLE for normal data
# 
# same idea as above.  Given a data set, what is the 
# mean and standard deviation

data <- read.csv("~/github/courses/fall2017/psyc5316/lectures/data1.csv")
hist(data$x)
plot(density(x)) # smoothed version of histogram

# Step 1: define negative log likelihood function

nll.normal <- function(data,par){
  return(-sum(log(dnorm(data, mean=par[1], sd=par[2]))))
}

# Step 2: find parameters (mean and sd) that minimize NLL

optim(par=c(1,0.1), fn=nll.normal, data=data$x) # look at par output; first number is mean, second is sd

x=seq(-3,4,0.01)
plot(density(data$x))
lines(x,dnorm(x,mean=2.85, sd=0.12),lty=2)



#########################################
# example 3: find MLE for ex-Gaussian data
#
# the ex-Gaussian distribution is a combination of a normal distribution with
# a longer right tail.  It has 3 parameters: mean (mu), sd (sigma), and shape (tau)

# we first need to define the ex-Gaussian density function
dexg <- function(x, mu, sigma, tau){
  return((1/tau)*exp((sigma^2/(2*tau^2))-(x-mu)/tau)*pnorm((x-mu)/sigma-(sigma/tau)))
}

# let's plot a few ex-Gaussian curves
x=seq(0,4,0.1)
plot(x,dexg(x,0.5,0.2,0.3),type="l") # change parameters to see effects on shape of distribution


# now, let's look at some data.  These are 1000 observations of reaction time
data <- read.csv("~/github/courses/fall2017/psyc5316/lectures/data2.csv")
head(data)
plot(density(data$rt))

# Step 1: define NLL
nll.exg <- function(data,par){
  return(-sum(log(dexg(data, mu=par[1], sigma=par[2], tau=par[3]))))
}

# Step 2: find parameters (mu, sigma, tau) that minimize NLL
optim(par=c(0,0.1,0.1), fn=nll.exg, data = data$rt)  # look at par output -- first number = mu, second = sigma, third = tau

x=seq(0,4,0.1)
plot(density(data$rt))
lines(y,dexg(y,mu=0.715, sigma=0.336, tau=0.465),lty=2)
