# lecture 7 - the ex-Gaussian distribution

# need to install the "moments" package
install.packages("moments")

# load Schwarz (2001) data
X = read.csv("https://raw.githubusercontent.com/tomfaulkenberry/courses/master/summer2021/mathpsych/schwarz-A.csv")

# plot histogram of RTs
hist(X$RT, breaks=30)

# goal: fit both a normal and an ex-Gaussian distribution to these data

# Step 1: define an objective function (NLL) 
nll.normal = function(data, pars){
  mu = pars[1]
  sigma = pars[2]
  return(-sum(log(dnorm(data,
                        mean = mu,
                        sd = sigma))))
}

# Step 2: use sample mean/sd as initial parameters for mu/sigma
initPar = c(mean(X$RT), sd(X$RT))

# Step 3: perform optimization
model1 = optim(par = initPar,
               fn = nll.normal,
               data = X$RT)

# extract parameters
mu = model1$par[1]
sigma = model1$par[2]

# plot model against raw data
hist(X$RT, breaks = 30, probability = T)
x = seq(from = 0, to = 600, length.out=200)
lines(x, dnorm(x, mean=mu, sd=sigma))

# compute BIC
k = 2 # two parameters
N = length(X$RT)
BIC1 = k*log(N) + 2*model1$value

# now, let's fit with an ex-Gaussian
# note: ex-Gaussian density is not included in R, so we have to hand-code it
dexg = function(x, mu, sigma, tau){
  return((1/tau)*exp((sigma^2/(2*tau^2))-(x-mu)/tau)*pnorm((x-mu)/sigma-(sigma/tau)))
  }

# Step 1: define objective function (NLL)
nll.exg = function(data,pars){
  mu = pars[1]
  sigma = pars[2]
  tau = pars[3]
  return(-sum(log(dexg(data, mu, sigma, tau))))}

# Step 2: function to give initial guess for parameters
# note: this is from Heathcote (2004)
init.exg = function(data){
  require("moments")
  tau = 0.8*sd(data)
  mu = mean(data) - skewness(data)
  sigma = sqrt(var(data)-tau^2)
  return(c(mu, sigma, tau))
}

initPar = init.exg(X$RT)

# Step 3: perform optimization
model2 = optim(par=initPar,
               fn = nll.exg,
               data = X$RT)

# extract parameters
mu = model2$par[1]
sigma = model2$par[2]
tau = model2$par[3]

# add to plot to compare with normal fit
lines(x, dexg(x, mu, sigma, tau), lty=2)

# compute BIC
k = 3 # ex-Gaussian has three parameters
N = length(X$RT)
BIC2 = k*log(N) + 2*model2$value

# note that BIC for model 2 is less than BIC for model 1
# implying that ex-Gaussian is better fit
BIC2 < BIC1

# test subsets
X_1 = subset(X, d==1)
X_4 = subset(X, d==4)






