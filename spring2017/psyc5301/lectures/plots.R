setwd("~/github/courses/spring2017/psyc5301/lectures/")

# Lecture 1 plots
# plot likelihood for coin flipping example
png(filename="figures/coinFlip.png",width=600,height=450, units = "px", res = 75)
f = function(theta) choose(10,6)*theta^6*(1-theta)^4
plot.function(f,from=0,to=1,axes=FALSE,xlab="true proportion",ylab="likelihood")
axis(side=1)
axis(side=2)
abline(v=0.6,lty=2,col="gray")
abline(v=0.5,lty=2,col="gray")
abline(h=f(0.6),lty=2,col="gray")
abline(h=f(0.5),lty=2,col="gray")
points(x=c(0.5,0.6), y=c(f(0.5),f(0.6)))
text(0.1,0.24,"H1 likelihood = 0.251")       
text(0.1,0.19, "H2 likelihood = 0.205")
text(0.1,0.10, "likelihood ratio = 1.22")
dev.off()
