# PSYC 5301
# using R for basic statistical inference

# single sample t-test
x = c(216,213,207,194,227,218,193,210,194,199)
t.test(x, alternative="greater", mu=200)

# independent samples t-test
drug = c(5,10,13,7,9,8,21,9,14,8)
placebo = c(15,14,12,8,14,7,16,10,15,12)
t.test(drug, placebo, alternative="less", var.equal=TRUE)

# paired samples t-test
before = c(2,1,4,1,4,3,3,2,3,5)
after = c(3,0,5,2,5,5,5,4,4,5)
t.test(before, after, alternative="less", paired=TRUE)

# one way ANOVA
graderA = c(4,3,4,5,2,3,4,5)
graderB = c(4,4,5,5,4,5,4,4)
graderC = c(3,4,2,4,5,5,4,4)
scores = data.frame(graderA, graderB, graderC)
scores = stack(scores)

names(scores) = c("score", "grader")

attach(scores)
boxplot(score ~ grader)
tapply(score, INDEX=grader, FUN="mean")

# method 1 (works only for one-way anova)
oneway.test(score ~ grader, data=scores, var.equal=TRUE)

# method 2 (works for more complex models)
model = aov(score~grader, data=scores)
summary(model)

# factorial anova
data = read.csv("https://git.io/fNbnc")
attach(data)
tapply(correctRecall, INDEX=list(encodingCue,retrievalCue), FUN="sd")
model = aov(correctRecall ~ encodingCue*retrievalCue, data=data)
summary(model)
