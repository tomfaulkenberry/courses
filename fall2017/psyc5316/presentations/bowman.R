# Kris Bowman - Linear Mixed Effects Models
install.packages("lme4")
library(lme4)

politeness = read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

# Let's look at the data
which(is.na(politeness$frequency))

# The relationship between politeness and pitch in boxplot
boxplot(frequency ~ attitude*gender, col=c("white","lightgray"),politeness)
# median line is lower for the polite than for the informal

# construct the model
lmer(frequency ~ attitude, data=politeness)
# error because this fuction needs a random effect
politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
summary(politeness.model)

# senario ("item") has less variablity than subject.
# residual is "e" = random dev. from the predicted values not due to the subjects and items.
# adding gender (we expect women to have higher pitch)
politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness)
summary(politeness.model)

# subject st. dev. dropped considerably
# p values for mixed effects are a pit trickier than linear models
# Likelihood ratios text
# The null model is without the factor you are interested in
politeness.null = lmer(frequency ~ gender + (1|subject) + (1|scenario), data=politeness, REML=FALSE)

# then the modle with the factor that you are interested in
politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness, REML=FALSE)

# compare the models
anova(politeness.null,politeness.model)
coef(politeness.model)
politeness.model = lmer(frequency ~ attitude + gender + (1+attitude|subject) + (1+attitude|scenario), data=politeness, REML=FALSE)

# Now lets look at slopes versus intercepts
# Find a p value
coef(politeness.model)

# each senario and subject has a different intercept
# Ransom intercept model will account for the aseline-differences in pitch, but we assume politeness is the same
# But, we need to run a random slope model
politeness.model = lmer(frequency ~ attitude + gender + (1+attitude|subject) + (1+attitude|scenario), data=politeness, REML=FALSE)
coef(politeness.model)

# for all speakers, the voice tends to go down, but for some poeple is goes down more than others
# obtaining the p value
politeness.null = lmer(frequency ~ gender + (1+attitude|subject) + (1+attitude|scenario), data=politeness, REML=FALSE)
anova(politeness.null,politeness.model)
# our model is sigificant