#Logistic Regression R
########################

#Install Pacakages
install.packages("aod")
install.packages("popbio")

#Load Dataset
d0 = read.csv("http://www.stanford.edu/class/psych252/data/hw2data.csv")
summary(d0)
d0$Type = factor(d0$Type)
head(d0)

#closer look at if responsibility influences whether to complain data
par(mfrow=c(1,2))
plot(factor(d0$complain), d0$Responsible, xlab=("Considered Complaining (0=NO, 1=YES)"), ylab="Feeling of Responsibility")
title(main="Visualization of the Data")

#Scatterplot
plot(jitter(d0$complain, factor=0.5)~d0$Responsible, ylab="Considered Complaining (0=NO, 1=YES)", xlab="Feeling of Responsibility")

#How responsibility influences tendency to complain
#model data with linear regression 
plot(jitter(d0$complain, factor=0.5)~d0$Responsible, ylab="P(Complain = 1)", xlab="Feeling of Responsibility", xlim=c(0,30))
abline(lm(d0$complain~d0$Responsible), col='red')
title(main="Data & Predicted Values from \nLinear Regression Model")

#Sigmoid (logistic) Function 
x <- c(-10:10)
b = 0 # intercept
m = 1 # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(x, y, xlab="X", ylab="P(Y=1)")
title(main="Sigmoid (Logistic) Function")

#Generalized Linear Model
rs1 = glm(complain ~ Responsible, data = d0, family = binomial, na.action = na.omit)
summary(rs1)

# show coeffecients
rs1$coefficients

# plot the data
plot(jitter(d0$complain, factor=0.5)~d0$Responsible, ylab="P(Complain = 1)", xlab="Feeling of Responsibility", ylim=c(0,1), xlim=c(0,20))

# plot the predicted values using the sigmoid function
x <- c(0:20)
b = rs1$coefficients[1] # intercept
m = rs1$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
par(new=TRUE)
plot(x, y, xlab="", ylab="", pch = 16, ylim=c(0,1), xlim=c(0,20), col='red')
title(main="Data & Predicted Values from \nLogistic Regression Model")

#interpereting the output from Gerneral Linear Model for log regression 
x = 5
b = rs1$coefficients[1] # intercept
m = rs1$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
as.numeric(y)

#Frequency of Data
logi.hist.plot(d0$Responsible,d0$complain,boxp=FALSE,type="hist",col="gray")
