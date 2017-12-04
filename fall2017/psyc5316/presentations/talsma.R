# Bayesian Linear Regression


#Description:
#From a survey of the clerical employees of a large financial 
#organization, the data are aggregated from the questionnaires of the 
#approximately 35 employees for each of 30 (randomly selected) 
#departments. The numbers give the percent proportion of favourable 
#responses to seven questions in each department.

data("attitude")

# Classical Regression
summary(fml <- lm(rating ~ ., data=attitude))

# Compute Bayes Factors for all regression models
library(BayesFactor)

output= regressionBF (rating ~ ., data=attitude, progress=FALSE)
head(output)              #best model is 'complaints' only

model = regressionBF(rating ~ complaints*learning, data=attitude)
summary(model)

# compute the bestModel

bestModel = regressionBF(rating~complaints, data=attitude)
            
# pull samples

chains = posterior(bestModel, iterations=10000)

summary(chains)
plot(chains)

chains = posterior(bestModel, iterations = 100000)






