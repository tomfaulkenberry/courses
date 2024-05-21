# notes for lecture 3

# recall our setup: 20 trials of therapeutic touch experiment
# model: binomial, with parameter w = prob of success
# on each trial

# Bayes factor = P(data|H0)/P(data|H1)

# from Lecture 2, we plotted "predictive distributions"
# to compute Bayes factors for H0 over H1 when observing 9 successes

# for uniform prior, BF_01 = 3.36
# for smooth peaked prior, BF_01 = 2.36

# lets look at the posteriors again
# first, assume uniform prior on w

# technical note: both priors we used are beta distributions
# uniform = beta(1,1)
# smooth peaked = beta(2,2)
# mathematically, it can be shown that with a beta(a,b) prior
# and k successes out of n trials, the posterior distribution is
# also a beta distribution:
# p(w | data) = beta(a + k, b + n - k)
# we'll use this below


# prior 1: uniform prior = beta(1,1)
# so posterior = beta(1 + 9, 1 + 20 - 9) = beta(10,12)
# lets plot both the posterior and the prior together

curve(dbeta(x, 10, 12), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

curve(dbeta(x,1,1), from=0, to=1,
      lwd=2, lty=2,
      add=T)

# now let's look at P(w=0.5) on each plot:

points(0.5, dbeta(0.5,10,12), pch=19)
points(0.5, dbeta(0.5,1,1), pch=19)

# notice that after observing data, our belief that w=0.5
# i.e, H0 / guessing
# has INCREASED

# by how much?
priorBelief = dbeta(0.5,1,1)
posteriorBelief = dbeta(0.5,10,12)
posteriorBelief/priorBelief

# answer = by a factor of 3.36
# this is the same as the Bayes factor we got earlier!

# Let's try with the smooth peaked prior
# prior 2: smooth peaked prior = beta(2,2)
# so posterior = beta(2 + 9, 2 + 20 - 9) = beta(11,13)
# lets plot both the posterior and the prior together

curve(dbeta(x, 11, 13), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

curve(dbeta(x,2,2), from=0, to=1,
      lwd=2, lty=2,
      add=T)

# now let's look at P(w=0.5) on each plot:

points(0.5, dbeta(0.5,11,13), pch=19)
points(0.5, dbeta(0.5,2,2), pch=19)

# notice that after observing data, our belief that w=0.5
# i.e, H0 / guessing
# has again INCREASED

# by how much?
priorBelief = dbeta(0.5,2,2)
posteriorBelief = dbeta(0.5,11,13)
posteriorBelief/priorBelief

# answer = by a factor of 2.36

# not an accident -- this is an example of the Savage-Dickey Density Ratio
