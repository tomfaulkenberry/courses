n=10
mu=100
sd=15

population=rnorm(1e6, mu, sd)

sample = sample(population, size = n, replace=FALSE)
sampleMean = mean(sample); sampleMean
sampleSD = sqrt(mean((sample-sampleMean)^2)); sampleSD

sampleMean=numeric(1000)
sampleSD=numeric(1000)
for (i in 1:1000){
  sample = sample(population, size=n, replace=FALSE)
  sampleMean[i] = mean(sample)
  sampleSD[i] = sqrt(mean((sample-sampleMean[i])^2))
}

plot(sampleMean)
abline(h=100)
length(sampleMean[sampleMean<mu])/1000

plot(sampleSD)
abline(h=15)
length(sampleSD[sampleSD<sd])/1000


############################
# plot t distribution

x=seq(-3,3,0.01)
plot(x, dnorm(x,0,1), type="l", lwd=2)
for (i in 1:4){
  lines(x,dt(x, df=i), col=i)
}
legend(0,0.1, c("df=1", "df=2", "df=3", "df=4"), lty=1,col=1:4)

