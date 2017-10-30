# PSYC 5316
# Week 10 -- posterior prediction and MCMC 

###############################
# posterior prediction

# first, sample from posterior

# grid approximation of posterior using 1000 grid points
p_grid = seq(from=0, to=1, length.out=1000)
prior = rep(1, 1000)
likelihood = dbinom(x=6, size=9, prob=p_grid)
posterior = likelihood * prior
posterior = posterior/sum(posterior)

# 10,000 samples from posterior
samples = sample(p_grid, prob=posterior, size=10000, replace=TRUE)
plot(samples, col="blue")
plot(density(samples), lwd=2)

# simulate 9 tosses with p=0.7

predictions = rbinom(1e5, size=9, prob=0.7)
plot(table(predictions), xlim=c(0,9))

# generate range of predictive distributions
par(mfrow=c(1,5))
Ps = c(0.1,0.3,0.5,0.7,0.9)
for (i in 1:5){
  predictions = rbinom(1e5, size=9, prob=Ps[i])
  plot(table(predictions), xlim=c(0,9), ylab="", main=paste("p=",Ps[i]))
}
par(mfrow=c(1,1))

# posterior predictive distribution
# incorporates the uncertainty in p
# from the sampled posterior distribution

predictions = rbinom(1e5, size=9, prob=samples)
plot(table(predictions), xlim=c(0,9))


##############################################
# MCMC - Metropolis algorithm


# define and plot island chain and population densities
islandNumber = 1:10
populations = c(2,3,3,5,6,10,10,1,2,3)
plot(islandNumber, populations, type="h")

# begin recording locations
N=10000
locations = numeric(N)
locations[1] = sample(1:10,1) # random starting place

# loop that defines Metropolis algorithm
for (i in 2:N){
  # take random walk either left or right
  proposal = locations[i-1] + sample(c(-1,1), 1)
  
  # make chain of islands circular
  if (proposal==11){proposal=1}
  if (proposal==0){proposal=10}
  
  # compute acceptance ratio (ratio of pop densities)
  current = populations[locations[i-1]]
  proposed = populations[proposal]
  acceptRatio = min(1, proposed/current)
  
  # move or stay according to acceptance ratio
  makeStep = rbinom(1, size=1, prob=acceptRatio)
  if (makeStep==0){
    locations[i]=locations[i-1]
  }
  else{
    locations[i]=proposal
  }
}

plot(locations[1:100])
plot(table(locations))


##########################################################
# estimating posterior for globe-tossing model

# define likelihood 
likelihood = function(data, par){
  return(dbinom(x=data, prob=par, size=9))
}
  
# define prior
prior = function(data, par){
  return(dunif(x=par, min=0, max=1))
}

# define posterior
posterior = function(data, par){
  return(likelihood(data, par)*prior(data, par))
}

# Metropolis algorithm  
N=10000
samples = numeric(N)

# random starting place
samples[1] = runif(1, min=0, max=1)

for (i in 2:N){
  #proposal = runif(1, min=0, max=1)
  x = rnorm(1, mean=samples[i-1], sd=0.01)
  if (x>0 & x<1){
    proposal=x
  }else if (x>1){
    proposal=1
  }else if (x<0){
    proposal=0
  }
  
  # acceptance ratio
  current = posterior(data=6, par=samples[i-1])
  proposed = posterior(data=6, par=proposal)
  acceptRatio = min(1, proposed/current)
  
  # move or stay
  makeStep = rbinom(1, size=1, prob=acceptRatio)
  if (makeStep==0){
    samples[i]=samples[i-1]
  }
  else {
    samples[i]=proposal
  }
}

# plot sample chain
plot(samples, type="l")

# plot posterior
plot(density(samples), lwd=2, xlim=c(0,1))


##############################
# changing prior

prior = function(data, par){
  return(dunif(x=par, min=0.5, max=1))
}

##############################
# changing proposal function
# comment out line 111 above 
# and replace with following code chunk

x = rnorm(1, mean=samples[i-1], sd=0.1)
if (x>0 & x<1){
  proposal=x
}else if (x>1){
  proposal=1
}else if (x<0){
  proposal=0
}
