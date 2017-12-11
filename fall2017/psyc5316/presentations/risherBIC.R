# Margaret Risher AIC 
# 12/8/2017

install.packages("BayesFactor")

# what the change of BIC infers about the strength of
# evidence against the model with the higher BIC value

# ??BIC	 | Evidence against higher BIC
# 0 to 2 |	Not worth more than a bare mention
# 2 to 6 |	Positive
# 6 to 10|	Strong
# >10	   |  Very Strong

head(mtcars)
# mpg is the dependent variable for this example.
attach(mtcars)
names(mtcars)

# seeing how miles per galon (mpg) as a function 
# of 1 variable. 

# Setting mpg as function of itself. In essance what is the BIC if 
# nothing influances "mpg"

BIC(lm(mpg~1))
#[1] 211.687

# Setting mpg as function to the number of cylanders 
# (cly)a car has?

BIC(lm(mpg~cyl))
# output
# [1] 173.7036

# Vomparing the funtions agaict each other. 
211.687- 173.7036
# The difference is 37.98 Showing strong evidence against
# nothing influencing "mpg".

#look at mpg being a function of horse power (hp)
BIC(lm(mpg~hp))
#Output 185.6358

# Comparing mpg as a function of hp to mpg to as a 
# funtion of itself. 
211.687-185.6358 
#difference is 20.05 which is strong evidence against
# mpg as a function of it,s self

# Compairing functions of mpg as a function of # of cyl
# to mpg as a function of HP
185.6358 - 173.7036
# we find a difference of [1] 11.9322 Indicating that 
# There is strong evidence against mpg as a function 
# horse power affecting mpg more than mpg as a function 
# of hp.

#looking at more than one variable

# BIC can also be used to look at the affects of multiple 
# independent variables have on the dependent variable. 

# investigating mpg as a function of cyl and hp
BIC(lm(mpg~hp + cyl))
# Output [1] 175.4248
# We can see that it is much lower than mpg~1 (mpg)
# or mpg~ of hp. but when Compare it to mpg~cy1 
# we find a change <2 which is within the BIC penalty 
# for multiple variable.


# me playing I looked at mpg as a function of 
# each dependent variable seperatly
BIC(lm(mpg~ 1))   # 211.687
BIC(lm(mpg~ cly)) # 173.704
BIC(lm(mpg~ disp))# 174.607
BIC(lm(mpg~ hp))  # 185.6358
BIC(lm(mpg~ drat))# 195.197
BIC(lm(mpg~ wt))  # 170.427
BIC(lm(mpg~ qsec))# 208.985
BIC(lm(mpg~ vs))  # 196.544
BIC(lm(mpg~ am))  # 200.882
BIC(lm(mpg~ gear))# 206.761
BIC(lm(mpg~ carb))# 203.578

