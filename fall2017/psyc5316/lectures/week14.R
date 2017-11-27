######################################
# PSYC 5316
# Week 14 
######################################

library(BayesFactor)
library(tidyverse)

# call "sleep" data set from BayesFactor package
data(sleep)

# perform traditional paired samples t-test
# first, compute difference scores

drug1 = sleep %>%
  filter(group==1)
drug2 = sleep %>%
  filter(group==2)

change = drug2$extra - drug1$extra

mean(change)
sd(change)

# effect size (Cohen's d)
mean(change)/sd(change)

# perform t-test
t.test(change, mu=0)

############################
# Bayesian t-test
############################

bf = ttestBF(x=change)
bf

# reciprocal
1/bf

# sampling from posterior
chains = posterior(bf, iterations=10000)
summary(chains)

# trace plots and posterior density
plot(chains[,1:3])

# prior?

x=seq(-3,3,0.01)
plot(x, dcauchy(x, scale=0.707), type="l")
lines(x, dnorm(x, mean=0, sd=1), lty=2)

# plot posterior and prior together
plot(density(chains[,3]), xlim=c(-3,3))
lines(x,dcauchy(x, scale=.707), lty=2)

# plot Bayes factor
library(polspline)
fit.posterior = logspline(chains[,3])
posterior = dlogspline(0, fit.posterior)
prior = dcauchy(0, scale=0.707)

points(0, posterior, pch=19)
points(0, prior, pch=19)
