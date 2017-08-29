# R commands -- Week 1

x = c(0,1,2,3)
p = c(0.125, 0.375, 0.375, 0.125)
barplot(p, names.arg=x, xlab="outcomes", ylab="p(x)")

mu = sum(x*p)
sum((x-mu)^2*p)

# dnorm
x = seq(from=0, to=400, by=0.1)
y = dnorm(x, mean=230, sd=20)
plot(x,y,type="l")
lines(x,dnorm(x,mean=100,sd=30),lty=2)

# pnorm(q, mean=??, sd=??) returns the probability 
# that a normal random variable has value less than
# or equal to q.

# Suppose that cholesterol levels in adults are 
# normally distributed with mean mu=230 and standard
# deviation sigma=20.

# what is the probability that a randomly selected
# adult will have a cholesterol level

# less than or equal to 200?
pnorm(200, mean=230, sd=20)

# greater than or equal to 240?
1-pnorm(240, mean=230, sd=20)

# between 210 and 250?
pnorm(250,mean=230, sd=20)-pnorm(210, mean=230, sd=20)


# qnorm(p, mean=??, sd=??) returns the "quantile"
# (percentile) associated with probability p in a 
# normal distribution with given mean and sd

# in the cholesterol example above, what cholesterol
# level would be greater than or equal to 95% of the
# population?

qnorm(0.95, mean=230, sd=20)
qnorm(c(0.025,.975), mean=230, sd=20)


























