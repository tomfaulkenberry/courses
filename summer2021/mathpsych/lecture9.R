# lecture 9 - the EZ diffusion model

# load data from Faulkenberry, Bowman, and Vick (2018)
# experiment on size congruity effect
X = read.csv("https://raw.githubusercontent.com/tomfaulkenberry/physNumComparisonTask/master/results/data/subject_104.csv")

# break into congruity conditions
X_congruent = subset(X, congruity=="congruent")
X_incongruent = subset(X, congruity=="incongruent")

# plot histograms of RTs
hist(X_congruent$response_time, breaks=30)
hist(X_incongruent$response_time, breaks=30, col="blue", add=T)

# extract summary statistics for each condition
mRT_congruent = mean(X_congruent$response_time)
mRT_incongruent = mean(X_incongruent$response_time)

varRT_congruent = var(X_congruent$response_time)
varRT_incongruent = var(X_incongruent$response_time)

Pc_congruent = mean(X_congruent$correct) 
Pc_incongruent = mean(X_incongruent$correct)


# fit EZ-diffusion model

# first, fit congruent trials
mRT = mRT_congruent
varRT = varRT_congruent
Pc = Pc_congruent

# Step 1 - calculate drift rate
L = log(Pc/(1-Pc)) # logit function
x = L*(Pc^2*L - Pc*L + Pc - .5)/varRT
driftRate1 = 0.1*sign(Pc-0.5)*x^(1/4)

# calculate threshold
a = 0.01*log(Pc/(1-Pc))/driftRate
threshold1 = a/2

# calculate nondecision time
y = -100*driftRate*a
MDT = a/(2*driftRate) * (1-exp(y))/(1+exp(y))
nondecisionTime1 = mRT - MDT

# second, fit incongruent trials
mRT = mRT_incongruent
varRT = varRT_incongruent
Pc = Pc_incongruent

# Step 1 - calculate drift rate
L = log(Pc/(1-Pc)) # logit function
x = L*(Pc^2*L - Pc*L + Pc - .5)/varRT
driftRate2 = 0.1*sign(Pc-0.5)*x^(1/4)

# calculate threshold
a = 0.01*log(Pc/(1-Pc))/driftRate
threshold2 = a/2

# calculate nondecision time
y = -100*driftRate*a
MDT = a/(2*driftRate) * (1-exp(y))/(1+exp(y))
nondecisionTime2 = mRT - MDT


