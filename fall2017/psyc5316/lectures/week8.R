## Week 8
## PSYC 5316
## intro to Bayesian modeling

# grid approximation
p_grid = seq(from=0, to=1, length.out=20)
prior = rep(1, 20)
likelihood = dbinom(x=6, size=9, prob=p_grid)
posterior = likelihood * prior
posterior = posterior/sum(posterior)

# plot the posterior
plot(p_grid, posterior, type="b")
 
# different priors to try
prior = ifelse(p_grid < 0.5, 0, 1)
prior = exp(-5*abs(p_grid - 0.5))

# sampling from posterior

# grid approximation of posterior using 1000 grid points
p_grid = seq(from=0, to=1, length.out=1000)
prior = rep(1, 1000)
likelihood = dbinom(x=6, size=9, prob=p_grid)
posterior = likelihood * prior
posterior = posterior/sum(posterior)

# 10,000 samples from posterior
samples = sample(p_grid, prob=posterior, size=10000, replace=TRUE)
plot(samples)
plot(density(samples))


# computations with samples

# intervals of defined boundary
sum(samples<0.5)/10000
sum(samples>0.5 & samples<0.75)/10000

# intervals of defined probability mass
quantile(samples,0.8)
quantile(samples,c(0.1,0.9))

# HPDI
library(coda)
sampMCMC = as.mcmc(samples)
HPDinterval(sampMCMC, prob=0.80)

# point estimates
# posterior mean and median
mean(samples)
median(samples)

# posterior mode - a little harder
dens = density(samples)
dens$x[which.max(dens$y)]
