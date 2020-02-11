# MDSC 403: Assignment 4
# Rachel Wong
# September 26th, 2019

# note: clear directory (lists)
rm(list = ls(all = T))

# 1. Search online on what is chi-squared distribution (if you don’t know what 
# it is). Try to figure out what it is and what parameter(s) it has. 
# [Note: in this assignment, you only need to work on one most important parameter.]

# A chi-squared distribution with k degrees of freedom is the distribution of the
# sum of the squares of k independent standard normal random variables. This means
# that you are calculating the probability of a discrete variable against the expected
# value of that discrete variable (relative to the degrees of freedom).

# Chi-square distribution has only one parameter, k, a positive integer which
# will specify the number of degrees of freedom.

dchisq(x, df, ncp = 0, log = FALSE) # x = value of interest
pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) # q = vector of quantiles
qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) # p = vector of probabilities
rchisq(n, df, ncp = 0) # n = number of observations

# df = degrees of freedom

# 2. Generate 10 random samples from a chi-squared distribution (with a 
# random selection of that parameter.)

# generate 10 (n) random samples with a random selection of degrees of freedom.
rchisq(n=10,df=5)
rchisq(n=10,df=10)
rchisq(n=10,df=15)

# 3. What are the meanings of this parameter? 
# For a chi-square distribution, the degrees of freedom (df) are the number of
# independent factors that can affect your result. The mean of a chi-square 
# distribution is the df. Also, as the df increases, the chi-square distribution
# approaches a normal distribution.
# For example, if you have a hat. You have k number of independent factors for the
# hat like the type of hat, color of hat, etc. The df is the amount of
# values that can affect your final result, all of the values are contributers. 

# chi-squared distribution is a number of squared standard normal, 
# and df is that number

# 4. If you mathematics is not good enough to understand the derivations, 
# please use R to figure it out roughly. 
# Show that chi-squared is the sum of multiple squared normal

# chi-squared distribution with k degrees of freedom is the distribution of a 
# sum of the squares of k independent standard normal random variables

# don't really need this:
# x=rnorm(n=10, mean = 100, sd = 10)
# draw 10 numbers at random from a normal distribution with mean = 100 and
# sd = 10, makes sure numbers are not negative

# Here we compare a normal distribution (squared) with n = 1000 to a rchisq
# with df = 1 since we have just "normal" 
# The plot shows that chi-squared is the sum of multiple squared normal (which
# is why we squared our normal to compare our rchisq to)

normal <-(rnorm(n=1000)) ^ 2 

qqplot(normal, (rchisq(n=1000,df=1)))

# Notes for Q6 from Q4:

# Using qqnorm, we can compare the QQ plots to see that as the df increases, the
# chi-square distribution approaches a normal distribution.
qqnorm(rchisq(n=10,df=5))
qqnorm(rchisq(n=10,df=10))
qqnorm(rchisq(n=10,df=15))
# large df, chi-squared is sum of normal

# note: as the df increases, the chi-square distribution approaches a normal
# distribution --> this is sufficient to prove the central limit theorem

# 5. Use an empirical distribution to represent 1000 random numbers generated 
# from a chi-squared distribution. And demonstrate that your empirical distribution 
# does represent a chi-squared distribution intended.

# install edfun package
install.packages('edfun')
library(edfun)

x <- rchisq(n=1000, df=100)
x_dist <- edfun(x)

# compare x to x_dist, instead of making another sample, we create the samples
# with rfun
qqplot(x, x_dist$rfun())

# qqplot shows that the lines are similar between x (chi-squared distribution)
# and x_dist (empirical distribution) 

# 6. Design an R procedure to demonstrate that central limit theorem may be correct. 

# Using qqnorm, we can compare the QQ plots to see that as the df increases, the
# chi-square distribution approaches a normal distribution.
qqnorm(rchisq(n=100,df=10))
qqnorm(rchisq(n=100,df=50))
qqnorm(rchisq(n=100,df=100))

# As the number of independent random variables increases, their sum tends towards
# a normal distribution. This is the central limit theorem. 

# 7. (Bonus) As stated in the presentation, the above is not a proof. You are 
# welcome to add a mathematical proof too! It is OK to consult Internet, but 
# please at least type it by yourself, instead of just send me a copy-pasted 
# document. 

# I'm sorry ): I'm bad at math
