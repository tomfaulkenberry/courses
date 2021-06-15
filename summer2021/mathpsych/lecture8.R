# lecture 7 - the Wald distribution

# load Schwarz (2001) data
X = read.csv("https://raw.githubusercontent.com/tomfaulkenberry/courses/master/summer2021/mathpsych/schwarz-A.csv")

# plot histogram of RTs
hist(X$RT, breaks=30)

# goal: fit Wald model

# density for shifted Wald
dwald = function(x, alpha, gamma){
  return((alpha/(sqrt(2*pi*x^3)))*exp(-(alpha-gamma*x)^2/(2*x)))
}

# Step 1: define NLL for shifted wald
nll.wald = function(data, pars){
  alpha = pars[1] # response threshold
  gamma = pars[2] # drift rate
  return(-sum(log(dwald(data,alpha,gamma))))
}

# Step 2: create function to give initial parameter estimate
# from Heathcote (2004)
waldInit = function(x, p = 0.9) {
  theta = p*min(x) 
  gamma = sqrt(mean(x)/var(x))
  alpha = gamma*mean(x) 
  return(c(alpha,gamma))	
}

initPar = waldInit(X$RT)

# Step 3: perform optimization
model = optim(par = initPar,
               fn = nll.wald,
               data = X$RT)

# extract parameters
alpha = model$par[1]
gamma = model$par[2]

# plot model against raw data
hist(X$RT, breaks = 30, probability = T)
x = seq(from = 0, to = 600, length.out=200)
lines(x, dwald(x, alpha, gamma))

# compute BIC
k = 3 # two parameters
N = length(X$RT)
BIC1 = k*log(N) + 2*model$value









