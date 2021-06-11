# Lecture 5 -- parametric bootstrapping

# Murdock (1961) data
times = c(1,3,6,9,12,18) # retention intervals (in seconds)
numRecall = c(94, 77, 40, 26, 24, 16) # number of words correctly recalled
X = data.frame(times, numRecall)

# define NLLs for each model
nll.power = function(data, pars){ 
  a = pars[1]
  b = pars[2]
  t = data$times
  x = data$numRecall
  tmp1 = log(choose(100,x))
  tmp2 = x * log(a*t^b)
  tmp3 = (100-x) * log(1-a*t^b)
  return(-1*sum(tmp1 + tmp2 + tmp3))
}

nll.exp = function(data, pars){ 
  a = pars[1]
  b = pars[2]
  t = data$times
  x = data$numRecall
  tmp1 = log(choose(100,x))
  tmp2 = x * log(a*b^t)
  tmp3 = (100-x) * log(1-a*b^t)
  return(-1*sum(tmp1 + tmp2 + tmp3))
}

# perform MLE with optim
a_init = runif(1) 
b_init = -1*runif(1)
initPar = c(a_init, b_init) # collect a,b into one parameter vector
model = optim(par = initPar, 
               fn = nll.power,
               data = X)

# new stuff!
# use estimates of a and b to perform bootstrapping

# number of bootstrap samples
numSims = 1000 

# extract initial parameter estimates
aHat = model$par[1]
bHat = model$par[2]

# set up empty vectors to store our bootstrapped estimates
a_bootstrap = numeric(numSims)
b_bootstrap = numeric(numSims)

# do a loop
for (i in 1:numSims){
  # generate simulated data from binomial model from initial parameter estimates
  numRecall = rbinom(n=6, size=100, prob = aHat*times^bHat)
  X = data.frame(times, numRecall)
  
  # perform MLE in simulated dataset
  initPar = c(aHat, bHat) # best guess would be our MLE estimates!
  model = optim(par = initPar,
                fn = nll.power,
                data = X)
  
  # extract and store bootstrap parameter estimates
  a_bootstrap[i] = model$par[1]
  b_bootstrap[i] = model$par[2]
 }

# look at bootstrapped parameter estimates
hist(a_bootstrap)
hist(b_bootstrap)

# 95% confidence intervals
quantile(a_bootstrap, probs = c(0.025, 0.975))
quantile(b_bootstrap, probs = c(0.025, 0.975))
