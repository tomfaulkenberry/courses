# notes for lecture 2


# setup: 20 trials of therapeutic touch experiment
# model: binomial, with parameter w = prob of success
# on each trial
# let's recall how to plot probability distributions 
# for a couple of possible values
# of w: say, w = 0.5 and w = 0.8
x = 0:20  # possible values of observed data
y1 = dbinom(x, size=20, prob=0.5)
y2 = dbinom(x, size=20, prob=0.8)

# plot probability distributions
# these give us "potential" data for a fixed population parameter

barplot(y1, names.arg = 0:20,
        xlab = "Number of correct answers",
        ylab = "probability")
barplot(y2, names.arg = 0:20, 
        xlab = "Number of correct answers",
        ylab = "probability")

# problem of inference is in reverse
# given observed data and two competing models:
# 1. which model most likely to produce data?
# 2. what is the most likely value for the model's parameter(s)?

# our goal is to compute P(M | data)

# Bayes theorem: P(M | data) = P(M) x P(data | M)/P(data)
# or roughly: posterior = prior x likelihood

# example data
d = 9 # gives similar results to Emily Rosa study

# build and plot binomial likelihood
likelihood = function(x) dbinom(d, size=20, prob=x)
curve(likelihood(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="likelihood")

# likelihood function is not a probability distribution
# need Bayes theorem to transform it into one

# what prior to use?
# let's consider 3 different ones

# build and plot "uniform" prior
par(mfrow=c(1,1))
prior1 = function(x) dunif(x, min=0, max=1)
curve(prior1(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

# build and plot "sharp peaked" prior
prior2 = function(x) ifelse(x<=0.5, 4*x, 4-4*x)
curve(prior2(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

# build and plot "smooth peaked" prior
prior3 = function(x) dbeta(x,2,2)
curve(prior3(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

# technical note: prior x likelihood is not a probability 
# distribution, so must "normalize" -- i.e., divide by area 
# under this curve the resulting function 
# is a probability distribution

# need area under prior x likelihood for each prior:
area1 = integrate(function(x) (prior1(x)*likelihood(x)), 0,1)$value
area2 = integrate(function(x) (prior2(x)*likelihood(x)), 0,1)$value
area3 = integrate(function(x) (prior3(x)*likelihood(x)), 0,1)$value


# construct posteriors
posterior1 = function(x){
  prior1(x)*likelihood(x)/area1
}

posterior2 = function(x){
  prior2(x)*likelihood(x)/area2
}

posterior3 = function(x){
  prior3(x)*likelihood(x)/area3
}


#  plot posteriors

par(mfrow=c(1,3))

curve(prior1(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

curve(likelihood(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="")

curve(posterior1(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="")

curve(prior2(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

curve(likelihood(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="")

curve(posterior2(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="")


curve(prior3(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="P(w)")

curve(likelihood(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="")

curve(posterior3(x), from=0, to=1,
      lwd=2, bty="n",
      xlab="w",
      ylab="")



# build and plot 95% credible intervals
par(mfrow=c(1,1))

# uniform prior 
# need to roll my own CDF function
cdf = function(x) integrate(posterior1, 0, x)$value
qPost = function(p){
  tempf = function(t) cdf(t)-p
  return(uniroot(tempf,c(0,1))$root)
}

a = qPost(0.025)
m = qPost(0.5) # median
b = qPost(0.975)

w = seq(0,1,length.out=200)
plot(w, posterior1(w), type="l", 
     ylim=c(0,5), lwd=2, lty=1, bty="n",
     ylab="P(w)")
region = (w > a & w < b)
y = posterior1(w)
X = c(w[region][1], w[region], rev(w[region])[1])
Y = c(0,y[region],0)
polygon(X,Y, border=1, col="gray")
text(a,1.1*posterior1(a), sprintf("%0.3f",a), pos=2)
text(b,1.1*posterior1(b), sprintf("%0.3f",b), pos=4)
lines(c(m,m), c(0,posterior1(m)), lty=2)
text(m,posterior1(m), sprintf("%0.3f",m), pos=3)

# sharp peaked prior 
# need to roll my own CDF function
cdf = function(x) integrate(posterior2, 0, x)$value
qPost = function(p){
  tempf = function(t) cdf(t)-p
  return(uniroot(tempf,c(0,1))$root)
}

a = qPost(0.025)
m = qPost(0.5) # median
b = qPost(0.975)

w = seq(0,1,length.out=200)
plot(w, posterior2(w), type="l", 
     ylim=c(0,5), lwd=2, lty=1, bty="n",
     ylab="P(w)")
region = (w > a & w < b)
y = posterior2(w)
X = c(w[region][1], w[region], rev(w[region])[1])
Y = c(0,y[region],0)
polygon(X,Y, border=1, col="gray")
text(a,1.1*posterior2(a), sprintf("%0.3f",a), pos=2)
text(b,1.1*posterior2(b), sprintf("%0.3f",b), pos=4)
lines(c(m,m), c(0,posterior2(m)), lty=2)
text(m,posterior2(m), sprintf("%0.3f",m), pos=3)

# smooth peaked prior 
cdf = function(x) integrate(posterior3, 0, x)$value
qPost = function(p){
  tempf = function(t) cdf(t)-p
  return(uniroot(tempf,c(0,1))$root)
}

a = qPost(0.025)
m = qPost(0.5) # median
b = qPost(0.975)

w = seq(0,1,length.out=200)
plot(w, posterior3(w), type="l", 
     ylim=c(0,5), lwd=2, lty=1, bty="n",
     ylab="P(w)")
region = (w > a & w < b)
y = posterior3(w)
X = c(w[region][1], w[region], rev(w[region])[1])
Y = c(0,y[region],0)
polygon(X,Y, border=1, col="gray")
text(a,1.1*posterior3(a), sprintf("%0.3f",a), pos=2)
text(b,1.1*posterior3(b), sprintf("%0.3f",b), pos=4)
lines(c(m,m), c(0,posterior3(m)), lty=2)
text(m,posterior3(m), sprintf("%0.3f",m), pos=3)



# Bayes factors from prior predictive

N=20

priorPred1 = function(x, N, lo, hi) {  # uniform prior
  y = numeric(length(x))
  for (i in 1:length(x)) {
    y[i] = integrate(function(w) dunif(w, lo, hi) * dbinom(x[i], N, w),
                     0, 1, subdivisions = 10000L,
                     rel.tol = 1e-4, 
                     abs.tol = 1e-4,
                     stop.on.error = TRUE,
                     keep.xy = FALSE, 
                     aux = NULL)$value
  }
  y
}

priorPred2 = function(x, N) {  # beta(2,2) prior
  y = numeric(length(x))
  for (i in 1:length(x)) {
    y[i] = integrate(function(w) dbeta(w, 2, 2) * dbinom(x[i], N, w),
                     0, 1, subdivisions = 10000L,
                     rel.tol = 1e-4, 
                     abs.tol = 1e-4,
                     stop.on.error = TRUE,
                     keep.xy = FALSE, 
                     aux = NULL)$value
  }
  y
}

# plot prior predictives under each model M0 and M1

input = 0:N
output0 = function(x,n) dbinom(x, n, 0.5)
output1 = function(x,n) priorPred1(x, n, lo=0, hi=1)
output2 = function(x,n) priorPred2(x, n)

yax0 = output0(input, N)
yax1 = output1(input, N)
yax2 = output2(input, N)


# plot 1 -- uniform prior
may0 = max(yax0)
may1 = max(yax1)
rang = may0 + may1
arfrac = 1/36
arlen = 8

# plot text

barplot(yax1, 1, space=.1, names.arg=input, col = "gray50",
        xlab = "predicted number of wins under each hypothesis",
        ylab = "probability",
        ylim=c(-may0-rang*0.25, may1+rang*0.75)
        )
barplot(-yax0, 1, space=.1, col = "gray75", add=TRUE)
abline(h=0)

# observed data?

d=9

x0 = d*1.1 + .65
arrows(x0,  yax1[d+1]+rang*(arlen*arfrac), x0,  yax1[d+1]+rang*(arfrac)*3, .1, lwd = 2)
arrows(x0, -yax0[d+1]-rang*(arlen*arfrac), x0, -yax0[d+1]-rang*(arfrac)*3, .1, lwd = 2)

p0 = output0(d,N)
p1 = output1(d,N)
bf = p0/p1

text(x0, yax1[d+1]+rang*(arlen*arfrac)*1.2, sprintf("P = %.4f", p1), pos=4)
text(x0, -yax0[d+1]-rang*(arlen*arfrac)*1.2, sprintf("P = %.4f", p0), pos=4)
#text(9,0.23, sprintf("Bayes factor = %.2f", bf), pos=1)



# plot 2 - beta(2,2) prior
may0 = max(yax0)
may1 = max(yax2)
rang = may0 + may1
arfrac = 1/36
arlen = 8

# plot text

barplot(yax2, 1, space=.1, names.arg=input, col = "gray50",
        xlab = "predicted number of wins under each hypothesis",
        ylab = "probability",
        ylim=c(-may0-rang*0.25, may1+rang*0.75)
)
barplot(-yax0, 1, space=.1, col = "gray75", add=TRUE)
abline(h=0)

# observed data?

x0 = d*1.1 + .65
arrows(x0,  yax2[d+1]+rang*(arlen*arfrac), x0,  yax1[d+2]+rang*(arfrac)*3, .1, lwd = 2)
arrows(x0, -yax0[d+1]-rang*(arlen*arfrac), x0, -yax0[d+1]-rang*(arfrac)*3, .1, lwd = 2)

p0 = output0(d,N)
p1 = output2(d,N)
bf = p0/p1

text(x0, yax2[d+1]+rang*(arlen*arfrac)*1.2, sprintf("P = %.4f", p1), pos=4)
text(x0, -yax0[d+1]-rang*(arlen*arfrac)*1.2, sprintf("P = %.4f", p0), pos=4)
text(2,0.1, sprintf("Bayes factor = %.2f", bf), pos=4)



