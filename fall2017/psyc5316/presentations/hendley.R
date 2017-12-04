#Hierarchical Linear Modeling
library(car)
library(ggplot2)
library(nlme)
library(reshape)

surgerydataset = read.csv("surgerydataset.csv")
surgeryLinearModel<-lm(PostQoL~Surgery,data=surgerydataset)
summary(surgeryLinearModel)

#Predicting postQoL from the BaseQol and Surgery
surgeryLinearModel<-lm(PostQoL~Surgery+BaseQoL, data=surgerydataset)
summary(surgeryLinearModel)

#Incorporating Hierarchical Structure
#Fixed Intercepts
interceptOnly<-gls(PostQoL~1,data=surgerydataset, method="ML")
summary(interceptOnly)

#Leting the intercept vary
randomInterceptOnly<-lme(PostQoL~1,data=surgerydataset,random = ~1|Clinic,method = "ML")
summary(randomInterceptOnly)

#Finding -2LL (log likelihoods)
interceptOnly
logLik(interceptOnly)*-2
logLik(randomInterceptOnly)*-2

#alternate method
anova(interceptOnly,randomInterceptOnly)


#adding fixed effects (adding predictors)
randomINterceptSurgery<-lme(PostQoL~Surgery,data = surgerydataset, random=~1|Clinic, method="ML")
summary(randomINterceptSurgery)

#adding a variable
randomInterceptSurgeryQoL<-lme(PostQoL~Surgery+BaseQoL, data=surgerydataset, random = ~1|Clinic, method = "ML")
summary(randomInterceptSurgeryQoL)

anova(randomInterceptOnly,randomInterceptSurgery,randomInterceptSurgeryQoL)

#introducing random slopes 
addRandomSlope<-lme(PostQoL~Surgery+BaseQoL, data = surgerydataset, random = ~Surgery|Clinic, method = "ML")
summary(addRandomSlope)
anova(randomInterceptSurgeryQoL,addRandomSlope)
