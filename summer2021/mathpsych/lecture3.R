# Lecture 3 -- application of MLE to forgetting

# Murdock (1961) data

times = c(1,3,6,9,12,18) # retention intervals (in seconds)
numRecall = c(94, 77, 40, 26, 24, 16) # number of words correctly recalled

# put these into ONE data "frame"
X = data.frame(times, numRecall)

# first, we'll fit the power model

# define negative log likelihood (the thing we're minimizing)
# note: pars is actually a vector with two numbers (a,b)
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

# find minimum of NLL
# first, need a guess for initial values of a,b
# let's just try random numbers

a_init = runif(1) 
b_init = runif(1)

initPar = c(a_init, b_init) # collect a,b into one parameter vector

optim(par = initPar, 
      fn = nll.power,
      data = X)

# we'll want to extract the MLEs, so assign the optim into an object

model1 = optim(par = initPar, 
               fn = nll.power,
               data = X)

# let's plot on top of original data
# first, extract parameters, then plot
a = model1$par[1]
b = model1$par[2]

plot(numRecall/100 ~ times)
curve(a*x^b,
      from=0, to=18,
      add=T)

