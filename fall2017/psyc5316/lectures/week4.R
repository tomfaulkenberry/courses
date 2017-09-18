# R script for Week 4
# PSYC 5316

scores = c(25,60,43,56,32,43,47,59,39,41)
mean(scores)

xbar = mean(scores)
s = sd(scores)
n=length(scores)
c = qt(0.975,df=n-1)

xbar - c*s/sqrt(n) # lower limit
xbar + c*s/sqrt(n) # upper limit

#############################################
# hypothesis testing
# example 1

(44.5-50)/(12/sqrt(10))
qnorm(0.05)

# visualizing "rejection region"

# do this first!
shadedTails <- function(from, to, density, ..., col="red"){
  y_seq = seq(from, to, length.out=500)
  d = c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=50)
}
  
x=seq(-4,4,0.01)
plot(x,dnorm(x),type="l",xlab="z-score")
shadedTails(-4, -1.645, dnorm)
points(x=-1.45, y=0.01, lwd=2)


# example 2
(610-580)/(50/sqrt(20))
qnorm(0.99)

x=seq(-4,4,0.01)
plot(x,dnorm(x),type="l",xlab="z-score")
shadedTails(2.33, 4, dnorm)
points(x=2.68, y=0.01, lwd=2)

# example 3
(54-50)/(12/sqrt(16))
qnorm(0.975)

x=seq(-4,4,0.01)
plot(x,dnorm(x),type="l",xlab="z-score")
shadedTails(-4, -1.96, dnorm)
shadedTails(1.96, 4, dnorm)
points(x=1.33, y=0.01, lwd=2)


# p-values
x=seq(-4,4,0.01)
plot(x,dnorm(x),type="l",xlab="z-score")
shadedTails(-4, -0.53, dnorm)

pnorm(-0.53)

# two-tailed example
x=seq(-4,4,0.01)
plot(x,dnorm(x),type="l",xlab="z-score")
shadedTails(-4, -2.67, dnorm)
shadedTails(2.67, 4, dnorm)

pnorm(-2.67)+(1-pnorm(2.67))


# illustrating power
x=seq(-4,6,0.01)
plot(x,dnorm(x),type="l",xlab="z-score")
text(-2,0.2,"H0 true")
shadedTails(1.645,6,dnorm)

lines(x,dnorm(x,mean=2.74,sd=1),lty=2)
text(5,0.2,"H0 false")

# power with two-tailed test
x=seq(-4,6,0.01)
plot(x,dnorm(x),type="l",xlab="z-score")
text(-2,0.2,"H0 true")
shadedTails(-4,-1.96,dnorm)
shadedTails(1.96,6,dnorm)
lines(x,dnorm(x,mean=2.74,sd=1),lty=2)
text(5,0.2,"H0 false")
