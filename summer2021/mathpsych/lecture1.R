# Lecture 1 - extensions of linear models

# Murdock (1961) data

x = c(1,3,6,9,12,18) # retention intervals (in seconds)
y = c(0.94, 0.77, 0.40, 0.26, 0.24, 0.16) # proportion recalled

plot(y~x)

# linear model
model1 = lm(y~x)
summary(model1)

# extract parameters (a = intercept, b=slope)
a = model1$coefficients[1]
b = model1$coefficients[2]

curve(a+b*x, 
      from=0, to=20,
      add=T)

# exponential model
plot(log(y)~x)


model2 = lm(log(y)~x)
summary(model2)

int = model2$coefficients[1]
slope = model2$coefficients[2]

# plot exponential curve on data
plot(y~x)
a = exp(int)
b = exp(slope)
curve(a*b^x, 0, 20, add=T)


# power function model
plot(log(y) ~ log(x))

model3 = lm(log(y)~log(x))
summary(model3)

int = model3$coefficients[1]
slope = model3$coefficients[2]

# plot power curve on data
plot(y~x)
a = exp(int)
b = slope
curve(a*x^b, 0, 20, add=T)


# computing model fit
summary(model1)
summary(model2)
summary(model3)
