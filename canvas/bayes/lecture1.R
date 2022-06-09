# lecture 1
# intro to modeling / classical inference

# probability versus likelihood

# Example: w = 0.25, N = 10 trials, probability of 8 successes? 
choose(10,8) * (0.25)^8 * (0.75)^2

# R has a built-in function for this!
dbinom(x=8, size=10, prob=0.25)

# plot potential data from model parameters
x = seq(from=0, to=10, by=1)
barplot(dbinom(x, size=10, prob=0.75), 
        names.arg = 0:10,
        xlab="Data x",
        ylab="P(x)")
        

# what if we're given the data x, but NOT the parameter w?
# plot "likelihood function"

# example: suppose we observed 8 successes in 10 trials. What is w?

w = seq(from=0, to=1, by=0.01)
plot(w, dbinom(x=8, size=10, prob=w),
     type="l",
     ylab="Likelihood")

# you'll notice that it peaks at 8/10 = 0.80
# this is the "maximum likelihood estimate" :)

abline(v=0.80)

# but there's also uncertainty around this value
# in this course, we're going to learn how to quantify 
# this uncertainty

# classical method for
# hypothesis testing / model comparison
# classical inference based on assuming null model: w=0.5
# and calculating probability of observing data (or more extreme)
# under this model:

x = seq(from=0, to=10, by=1)
barplot(dbinom(x, size=10, prob=0.5), # change to 0.5 to reflect null
        names.arg = 0:10,
        xlab="Data x",
        ylab="P(x)")

# we observed x = 8, so compute P(x >= 8 | null):

1 - pbinom(7, size=10, prob=0.5)

# this probability is small (less than 0.05), so we REJECT the null
# model, leaving support for the alternative (i.e., x > 0.5)

# alternatively, use the binom.test function:

binom.test(x=8, n=10, p=0.5, alternative="greater")


# back to Emily Rosa
# recall that she observed success on only 44% of trials
# out of 280 trials, x=123 successes.

binom.test(x=123, n=280, p=0.5, alternative="greater")

# this p-value is greater than 0.05, which tells us that observing
# these data is plausible under the null model.
# thus, we fail to reject the null.
