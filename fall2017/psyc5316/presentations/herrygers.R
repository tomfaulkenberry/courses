install.packages("boot")
library(boot) 

set.seed(333); x<- rnorm(30)
bootMean <- rep(NA, 1000); sampledMean <- rep(NA, 1000)
for(i in 1:1000){bootMean[i] <-mean(sample(x,replace=TRUE))}
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))}
plot(density(bootMean)); lines(density(sampledMean), col="red")

set.seed(333); x <- rnorm(30); sampledMean <- rep(NA, 1000)
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))}
meanFunc <- function(x,i){mean(x[i])}
bootMean <- boot(x,meanFunc,1000)

plot(density(bootMean$t)); lines(density(sampledMean), col="red")

data(nuclear)
nuke.lm <- lm(log(cost) ~ date,data=nuclear)
plot(nuclear$date, log(nuclear$cost), pch=19)
abline(nuke.lm,col="red",lwd=3)

par(mfrow=c(1,3))
for(i in 1:3){
  nuclear0=nuclear[sample(1:dim(nuclear)[1],replace=TRUE),]
  nuke.lm0=lm(log(cost) ~ date,data=nuclear0)
  plot(nuclear0$date,log(nuclear0$cost),pch=19)
  abline(nuke.lm0,col="red",lwd=3)
}

bs <- function(data, indices,formula) {
  d <- data[indices,];fit <- lm(formula, data=d);return(coef(fit))
}
results <- boot(data=nuclear, statistic=bs, R=1000, formula=log(cost) ~ date)
plot(density(results$t[,2]),col="red",lwd=3)
lines(rep(nuke.lm$coeff[2],10),seq(0,8,length=10),col="blue",lwd=3)

boot.ci(results)
