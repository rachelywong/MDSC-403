# MDSC 403: Assignment 5
# Rachel Wong
# October 2nd, 2019

# note: clear directory (lists)
rm(list = ls(all = T))

# 1. Form a one-sample t-test to check whether the sample data has the 
# expected mean value. 

# a.	You may use rnorm to generate some data with known mean
x = rnorm(100, mean = 2.0)

# b.	Then test whether the data has the right mean (test 1)
t.test(x, mu=2, alternative ="less", var.equal=F)

# c.	Test again using a different mean
t.test(x=rnorm(100, mean=1.5), mu=2, alternative ="less", var.equal=F)

# d.	Explain what are the six steps during the course. Note that some of the 
# answers need to write new R code, instead of just reading from the outcome. 
# i.	H0 v.s. H1?
# H0 is the null hypothesis (what you want to reject), and H1 is the alternative
# hypothesis (where if H0 does not hold, we accept H1). 
# H0: no difference in your population mean (sample mean = mu = 2)
# H1: there is a difference in your population mean, since we chose alternative (less)
# (sample mean < mu where mu = 2)
# i.	How risky we are? (the type I error)
# alpha value = 0.05
# ii.	Which test statistic?
# t-test statistic
# iii.	The critical value?
# we calculate up to 95% to get our area up to our critical value (cut-off) and
# df = 99
qt(0.95, 99)
# iv.	The value of the statistic (t)?
# v. The P-value?   
# for x:
#x
#t = -1.1743, df = 99, p-value = 0.1215
# when we change the mean:
#rnorm(100, mean = 1.5)
#t = -3.9978, df = 99, p-value = 6.162e-05

# you can also calculate the p-value using:
pt (-1.1743,99)
# result = 0.121546
  
# e. Change the sample size to see how p-values change accordingly 
t.test(x=rnorm(100, mean = 2.0), mu=2, alternative ="less", var.equal=F)
t.test(x=rnorm(1000, mean = 2.0), mu=2, alternative ="less", var.equal=F)
# the p-value should decrease as the sample size increases
# this is because the H0 and H1 curves get skinnier which results in less overlap

# f. Change the variance (sd) to see how p-values change accordingly
t.test(x=rnorm(1000, mean = 2.0, sd = 1), mu=2, alternative ="less", var.equal=F)
t.test(x=rnorm(1000, mean = 2.0, sd = 1.5), mu=2, alternative ="less", var.equal=F)
# as you increase the sd, the p-value should increase

# note: you want a bigger n (sample size) and a smaller sd (variance) to have
# a better representation of data

# 2. Implement a permutation test to test whether the mean of the random numbers 
# is the same to an expected value (similar to what you have done using the t-test 
# above). Compare the P-value out of permutations and the P-value out of t-test. 
# Note that, although coding by yourself is preferable, it is Ok if you manage to 
# utilize a package. However, you need to correctly interpret what the package is 
# doing. Please also tell me which library to be installed for me to run your code. 

# install 'perm' package
install.packages('perm')
library(perm)

# combine a and b into the same group
# if they are from the same population, they still have a normal distribution
# if they are from a different population, the mean will change and they won't
# have a normal distribution
# p-value < 0.05 = they are from different populations

a = rnorm(1000, mean = 1.0)
b = rnorm(1000, mean = 2.0)

# permTS performs two sample permutation tests
permTS(a, b, alternative = c("two.sided", "less", "greater"))

# a and b have a p-value < 0.05, which means they are from different populations

c = rnorm(1000, mean = 2.0)
d = rnorm(1000, mean = 2.0)

# permTS performs two sample permutation tests
permTS(c, d, alternative = c("two.sided", "less", "greater"))

# c and d have a p-value > 0.05, which means they are from the same population.
# the difference in mean should be approximately 0

# Permutation tests test if two different groups come from the same distribution. 
# This is done by randomly shuffling the values of our groups and creating
# shuffled datasets. If these shuffled datasets look like the real data, then our
# null hypothesis is true and the mean difference should be approximately 0. We use
# permutation tests to see if our subsets have a significantly different mean than
# the actual samples. We combine our two samples into one population and then
# we take subsets of that that are the same size as our original sample, and we do
# this for every possible way of dividing the pooled values into (for every
# permutation of the two groups). 

# From the perm package:
# "If Ti is a vector of test statistics, and T0 is the observed test statistic, 
# then alternative="less" gives p.lte=Pr[Ti<=T0], alternative="greater" gives 
# p.gte=Pr[Ti>=T0], alternative="two.sided" with tsmethod="central" (default) 
# gives p.twosided=max(1, 2*min(p.lte,p.gte)), and alternative="two.sided" with 
# tsmethod="abs" gives p.twosidedAbs=Pr[abs(Ti - mean(Ti) ) >=abs(T0-mean(Ti))]."

# Ti = mean for permutation of interest
# To or T0 = mean observed overall

# Less means that we are looking for if the population mean >= sample mean, and 
# greater means that we are looking for if the population mean <= sample mean. 
# The alternative hypothesis changes based on if the sample mean is less than 
# or greater than the population mean.

# The p-value for the t-test is smaller than the p-value form the perms test.
# This is because the test statistic of a t-test are obtained from a normal distribution 
# (based off of a theoretical distribution) while a permutation is not. A 
# permutation test is therefore harder to get a significant p-value since it 
# looks directly at the samples.
