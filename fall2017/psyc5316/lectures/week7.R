## Week 7
## PSYC 5316

# generate random linear data
N = 100
x = runif(N)
y = 3 + 5*x + rnorm(N)
plot(x, y, ylim=c(0,10))

# using "lm" to fit linear model
model1 = lm(y~x)
summary(model1)

# plot best fitting line
intercept = model1$coefficients[1]
slope = model1$coefficients[2]
abline(a=intercept, b=slope)

# plot residuals
plot(density(model1$residuals))

# example with real data
data = read.csv("https://git.io/vd2w3")
plot(data$education, data$income)

# fit linear model
model2 = lm(data$income ~ data$education)
summary(model2)
intercept = model2$coefficients[1]
slope = model2$coefficients[2]
abline(a=intercept, b=slope)

# plot residuals
plot(density(model2$residuals))

###################################################
# fitting via minimizing squared errors

SS = function(data,par){
  with(data, sum((y-(par[1]+par[2]*x))^2))
}

dat=data.frame(x,y)
inits=c(1,1)
optim(inits, SS, data=dat)


# education versus salary example
dat=data.frame(data$education, data$income)
names(dat) = c("x", "y")
inits=c(1,1)
optim(inits, SS, data=dat)

####################################################
# fitting via Maximum Likelihood Estimation

reg.nll = function(data, par){
  residual = with(data, y-(par[1]+par[2]*x))
  return(sum(-log(dnorm(residual, mean=0, sd=par[3]))))
}

dat=data.frame(x,y)
inits=c(1,1,1)
optim(inits, reg.nll, data=dat)

# confidence intervals via Hessian matrix
fit = optim(inits, reg.nll, data=dat, hessian = TRUE)
hessian = fit$hessian
inverse = solve(hessian)
se = sqrt(diag(inverse))
