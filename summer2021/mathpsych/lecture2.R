# lecture 2
# intro to maximum likelihood estimation

# probability versus likelihood

# Example: w = 0.2, N = 10 trials, probability of 3 successes? 4 successes?
choose(10,3) * (0.2)^3 * (0.8)^7
choose(10,4) * (0.2)^4 * (0.8)^6

# R has a built-in function for this!
dbinom(x=3, size=10, prob=0.2)

# plot "probability density function"
x = seq(from=0, to=10, by=1)
barplot(dbinom(x, size=10, prob=0.2), # change to 0.7 to see effect
        names.arg = 0:10,
        xlab="Data x",
        ylab="P(x)")
        

# what if we're given the data y, but NOT the parameter w?
# plot "likelihood function"

# example: suppose we observed 7 successes in 10 trials. What is w?

w = seq(from=0, to=1, by=0.01)
plot(w, dbinom(x=7, size=10, prob=w),
     type="l",
     ylab="Likelihood")

# maximum likelihood estimation
# given observed data (y=7), find parameter value (w) that 
# maximizes the likelihood function
# "What value of w is most likely, given the data?"

# we will use the "optim" function to maximize the likelihood function
# actually - we will *minimize* the "negative likelihood" (NL)

plot(w, -dbinom(x=7, size=10, prob=w),
     type="l",
     ylab="Negative likelihood")

# define NL (function with two variables - "data" and "parameter values")
nl.binom = function(data, par){
  -dbinom(data, size=10, prob=par)
}

# use optim to minimize NLL
optim(par=0.5, fn=nl.binom, data=7)




