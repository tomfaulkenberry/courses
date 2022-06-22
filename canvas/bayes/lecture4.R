# Lecture 4 - Bayesian inference for correlations

# Goal: given observed (sample) correlation (Pearson's r), infer things about
# population correlation parameter, denoted "rho"

# with any Bayesian inference, one must choose prior
# objective Bayes -- use "default" priors when possible


# Jasp default -- uniform prior

x=seq(-1,1,length.out=200)
plot(x, dunif(x,-1,1), type="l",
     lwd=2, bty="n",
     xlab = expression(paste("Pearson's ",rho)),
     ylab = "Density")


# Jeffreys (1961) - recommended prior of the form 1-r^2

curve(1-x^2, from=-1, to=1,
      bty="n", lwd=2,
      xlab=expression(paste("Pearson's ",rho)),
      ylab="Density")

# this works well if our prior belief has more mass at a
# correlation of 0.  But we might have two other types
# of prior belief:
# 
# 1. flat (uniform) prior (just like binomial example)
# 2. higher prior belief at the "ends" +/- 1
# for example, if we were doing psychometrics and 
# testing the test/retest reliability of a scale,
# we might place a larger degree of belief that the correlation
# is closer to +/- 1 than to 0.

# Solution - use the "stretched Beta prior"
# formula for this prior can be found in 
# Ly, Marsman, & Wagenmakers (2018), equation 13


stretchedBeta = function(x,width){
  (1-x^2)^(1/width-1)/beta(0.5,1/width)
}

# note - Ly et al. parameterized this prior in terms of a tuning
# parameter alpha. JASP uses "width"...they are inversely 
# related: width = 1/alpha

curve(stretchedBeta(x, width=2), from=-1, to=1,
      bty="n", lwd=2,
      xlab=expression(paste("Pearson's ",rho)),
      ylab="Density",
      ylim=c(0,1))

curve(stretchedBeta(x, width=1),
      lwd=2, lty=2, add=T)

curve(stretchedBeta(x, width=0.5),
      lwd=2, lty=3, add=T)

legend(x=-0.3, y=0.3, legend=c("width=2","width=1","width=0.5"), 
       lwd=2, lty=1:3, bty="n")


# roughly, the "width" parameter controls
# how wide the peak is around 0.
# as width decreases, the prior becomes more
# peaked.  Width=1 --> flat, width=2 --> peaked at ends
