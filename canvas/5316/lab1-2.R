# PSYC 5316
# using R for basic statistical inference

# Example 1: independent samples t-test
# A new drug is supposed to reduce recovery time after 
# surgery. Suppose we measure the recovery time (in days) 
# for patients taking the new drug. Suppose we also measure 
# recovery time for a control group which takes a placebo. 
# The data are as follows:
#
# with drug: 15 10 13 7 9 8 21 9 14 8
# placebo:   15 14 12 8 14 7 16 10 15 12
#
# Do these data indicate that the drug reduces recovery times?

# define data "vectors"
drug = c(15,10,13,7,9,8,21,9,14,8)
placebo = c(15,14,12,8,14,7,16,10,15,12)

# descriptives
mean(drug)
sd(drug)
mean(placebo)
sd(placebo)

# perform t-test
t.test(drug, placebo, alternative="less", var.equal=TRUE)

# report
# t(18) = -0.53, p=0.30


# Example 2 - paired samples t-test
# Ten participants rate their enjoyment of a certain statistical 
# software package before and after a day-long training session. 
# Their scores (out of 5 possible points) are listed below:
#
# Before: 2 1 4 1 4 3 3 2 3 5 
# After:  3 0 5 2 5 5 5 4 4 5
#
# Was the workshop effective at increasing participant 
# satisfaction with the statistical software package?

# define data vectors
before = c(2,1,4,1,4,3,3,2,3,5)
after = c(3,0,5,2,5,5,5,4,4,5)

# descriptives
mean(before)
mean(after)
sd(before)
sd(after)

# t-test
t.test(after, before, paired=TRUE, alternative="greater")

# report
# t(9) = 3.35, p=0.004

# Example 3 - one way ANOVA
# Suppose a committee is trying to score 27 different 
# scholarship applications. As the job is too much work for 
# one grader, suppose 3 are used. The scholarship committee 
# would like to ensure that each grader is using the same 
# grading scale, as otherwise the students aren’t being treated 
# equally. One approach to checking if the graders are using 
# the same scale is to randomly assign each grader 8 applications 
# and then compare the scores for the 3 graders, knowing that 
# the differences should be due to chance errors if the graders 
# all grade equally. Suppose the grading scale is on the 
# range 1-5 with 5 being the best and the scores are reported as:
#
# grader A: 4 3 4 5 2 3 4 5 
# grader B: 4 4 5 5 4 5 4 4 
# grader C: 3 4 2 4 5 5 4 4
#
# Are there differences among the three graders?

# define data vectors
graderA = c(4,3,4,5,2,3,4,5)
graderB = c(4,4,5,5,4,5,4,4)
graderC = c(3,4,2,4,5,5,4,4)

# ANOVA works best using "long data frame"
data = data.frame(graderA, graderB, graderC)
dataLong = stack(data)

# rename columns something useful!
names(dataLong) = c("score", "grader")

# find descriptives
# use tapply to split descriptives by factor
tapply(dataLong$score, INDEX=dataLong$grader, FUN="mean")
tapply(dataLong$score, INDEX=dataLong$grader, FUN="sd")

# perform ANOVA
model = aov(score~grader, dataLong)
summary(model)

# report
# F(2,21) = 1.13, p=0.34






