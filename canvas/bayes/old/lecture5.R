# Lecture 5 - Bayesian t-tests

# Goal: given observed (sample) mean, infer things about
# population mean 

# first, let's transform to effect size: delta = (x-mu)/sigma

# what prior to use for delta?
# let's try normal prior with standard deviation =1
# aka, "unit information prior"

curve(dnorm(x), from=-4, to=4,
      bty="n", lwd=2,
      xlab=expression(paste("Effect size ",delta)),
      ylab="Density")
lines(x=c(0,0), y=c(0,dnorm(0)), lwd=1.5, lty=2, col="gray")
lines(x=c(0,1), y=c(dnorm(1), dnorm(1)), lwd=1.5)
lines(x=c(0,0), y=c(dnorm(1)+0.01, dnorm(1)-0.01), lwd=1.5)
lines(x=c(1,1), y=c(dnorm(1)+0.01, dnorm(1)-0.01), lwd=1.5)
text(x=0.5, y=dnorm(1)-0.02, expression(paste(sigma," =1")), cex=1.5)

# even less information would be provided if sigma was not specified
# Zellner & Siow (1980) proposed a hierarchical model
# where delta was distributed as Normal(0, sigma^2_b),
# and then the "hyperparameter" sigma^2_b was then distributed
# as an Inverse chi-square distribution

curve(dnorm(x), from=-4, to=4,
      bty="n", lwd=2,
      xlab=expression(paste("Effect size ",delta)),
      ylab="Density")
lines(x=c(0,0), y=c(0,dnorm(0)), lwd=1.5, lty=2, col="gray")
lines(x=c(0,1), y=c(dnorm(1), dnorm(1)), lwd=1.5)
lines(x=c(0,0), y=c(dnorm(1)+0.01, dnorm(1)-0.01), lwd=1.5)
lines(x=c(1,1), y=c(dnorm(1)+0.01, dnorm(1)-0.01), lwd=1.5)
text(x=0.5, y=dnorm(1)-0.03, expression(sigma[b]^2), cex=1.5)

curve(extraDistr::dinvchisq(x,1), from=0, to=8,
      bty="n", lwd=2,
      xlab=expression(sigma[b]^2),
      ylab="Density")


# Jasp default -- integrate out the hyperparameter
# to get a Cauchy distribution (Liang et al., 2008)

curve(dnorm(x), from=-3, to=3,
      bty="n", lwd=2, lty=3,
      xlab=expression(paste("Effect size ",delta)),
      ylab="Density")
curve(dcauchy(x, scale=1), lwd=2, lty=1, add=T)
legend(-1, 0.15, c("Cauchy", "Normal"), lwd=rep(2,2), lty=c(1,3), bty="n")
