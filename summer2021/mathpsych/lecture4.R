# Lecture 4 -- model fit diagnostics

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

# find minimum of NLL
# first, need a guess for initial values of a,b
# let's just try random numbers

a_init = runif(1) 
b_init = runif(1)
initPar = c(a_init, b_init) # collect a,b into one parameter vector

model1 = optim(par = initPar, 
      fn = nll.power,
      data = X)

model2 = optim(par = initPar, 
               fn = nll.exp,
               data = X)


# let's plot on top of original data
plot(numRecall/100 ~ times)

# first, extract parameters from power model
a = model1$par[1]
b = model1$par[2]
curve(a*x^b,
      from=0, to=18,
      add=T)

# next, extract parameters from exponential model
a = model2$par[1]
b = model2$par[2]
curve(a*b^x,
      from=0, to=18,
      add=T,
      lty=2) # this makes a dashed line

# compute BICs
bic1 = 2*log(6)+2*model1$value
bic2 = 2*log(6)+2*model2$value

# BF
exp((bic1-bic2)/2)
