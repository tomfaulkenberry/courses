# Margaret Risher AIC and Bic#
# 12/8/2017

# help code AIC

?AIC

# mtcars is a data set in R

head(mtcars)

#Mpg dependent variable all others variable are independent/ predictor variables. 

# Fit1 trying to prdict mpg by using cylinder and weight. 

Fitl= lm(formula = mpg ~ cyl + wt, data = mtcars)
Fitl

# check AIC fit
AIC(Fitl)

# Fit2 trying to prdict mpg by using cylinder, weight, an am. 

Fit2= lm(formula = mpg ~ cyl + wt + am, data = mtcars)
Fit2

# Comparing models 
# Smaller number indicates better model fit when comparing several competing models.

# Note AIC will not tell how good a model is. It will only indicate which one of the
# competing models fit the data best. If all the model are bad it will indicat the 
# best fit out of the bad models. 

AIC(Fitl)
AIC(Fit2)

# compare outputs
# > AIC(Fitl)
# [1] 156.0101
# > AIC(Fit2)
# [1] 157.9892
# Fit1 is lowest Fit1 is the better model of the two

# AIC protects against overfitting. Recall the equation for AIC AIC=2K???2log(???(??^|y)).
# The 2k2k term in AIC means that the AIC will go up by 2 for every additional 
# parameter estimated. This is how AIC penalizes models for  the addition of 
# additional Variables. this can be seen by the increased AIC value in Fit2 
# when the third variable "am" was added.

# Another example 3 varriables only 1 changed 

# Fit3 trying to prdict mpg by using drat, weight, and am. 
Fit3= lm(formula = mpg ~ drat + wt + am, data = mtcars)
Fit3

# Fit4 trying to prdict mpg by using drat, weight, and vs. 

Fit4= lm(formula = mpg ~ drat + wt + vs, data = mtcars)
Fit4


# Comparing Fit3 and Fit4

AIC(Fit3)
AIC(Fit4)

# compare outputs
# > AIC(Fit3)
# [1] 168.699
# > AIC(Fit4)
# [1] 162.223

# Fit4 has the lower AIC of the two models indicating 
# Fit4 is the better fit.

# Fit5 trying to prdict mpg by using drat, weight,
# am and vs. 

Fit5= lm(formula = mpg ~ drat + wt +am + vs, data = mtcars)
Fit5

# Counter example of additional varriable by comparing 
# AICs of Fit3, Fit4, and Fit 5.

AIC(Fit3) # am
AIC(Fit4) # vs
AIC(Fit5) # am + Vs

# Comparing outputs 
# > AIC(Fit3)
# [1] 168.699
# > AIC(Fit4)
# [1] 162.223
# > AIC(Fit5)
# [1] 163.7381
#
# Fit5 fall between Fit3 and Fit4, and is a better
# fit than Fit3 even with Fit5's penalty. Indicating "am" 
# may not be a good predictor variable.

# Fit6 trying to prdict mpg by using drat + weight.

Fit6=lm(formula = mpg ~ drat + wt, data = mtcars)
Fit6

AIC(Fit6)

# comparing the additonal variables

AIC(Fit6) # drat +wT
AIC(Fit3) # am
AIC(Fit4) # vs

# Outputs 


# > AIC(Fit6) # drat +wt
# [1] 166.968
# > AIC(Fit3) # adding "am" made the model worse
# [1] 168.699
# > AIC(Fit4) # adding "vs" made the model better
# [1] 162.223

#Note it is recommend to Confirm  model fit with the
# appropriate hypothisis test. to nsur model is valid.

# one way Anova tests

AFitl= aov(formula = mpg ~ cyl*wt, data = mtcars)
AFitl
summary(AFitl)

AFit2= aov(formula = mpg ~ cyl*wt*am, data = mtcars)
AFit2
summary(AFit2)

AFit3= lm(formula = mpg ~ drat*wt*am, data = mtcars)
AFit3
summary(AFit3)

AFit4= lm(formula = mpg ~ drat*wt*vs, data = mtcars)
AFit4
summary(AFit4)

AFit5= aov(formula = mpg ~ drat*wt*am*vs, data = mtcars)
AFit5
summary(AFit5)

AFit6= aov(formula = mpg ~ drat*wt, data = mtcars)
AFit6
summary(AFit6)

