## Week 8
## PSYC 5316
## Bootstrap methods

# bootstrap estimate for mu

x=c(1,4,2,19,4,12,29,4,9,16)
xBar = mean(x)
s=sd(x)
n=length(x)

B=2000

# efficient
bootSamples = matrix(sample(x, size=length(x)*B, replace=TRUE), nrow=B)
means = apply(bootSamples, 1, mean)
means = sort(means)

lowerIndex = round(0.05/2*B)
upperIndex = B-lower

# confidence interval
means[lowerIndex] # lower bound
means[upperIndex] # upper bound








# generate random linear data
N = 100
x = seq(1,10,length=100)
y = 3 + 5*x + rnorm(N, mean=0, sd=5)
plot(x, y, ylim=c(0,60))

# using "lm" to fit linear model
model1 = lm(y~x)
summary(model1)

# plot best fitting line
intercept = model1$coefficients[1]
slope = model1$coefficients[2]
abline(a=intercept, b=slope)

# plot residuals
plot(model1$residuals)


# generate random heteroskedastic data
N = 100
x = seq(1,10,length=100)
y = 3 + 5*x + rnorm(N, mean=0, sd=ceiling(x))
plot(x, y, ylim=c(0,60))

# using "lm" to fit linear model
model2 = lm(y~x)
summary(model2)

# plot best fitting line
intercept = model2$coefficients[1]
slope = model2$coefficients[2]
abline(a=intercept, b=slope)

# plot residuals
plot(model2$residuals)


# Bootstrap methods for estimating CIs




