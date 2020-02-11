# MDSC 403: Assignment 6
# Rachel Wong
# October 10th, 2019

# note: clear directory (lists)
rm(list = ls(all = T))

# Install stats4 package.
library("stats4")

# 1.We have a sample drawn from a chi-squared distribution: 
# x=rchisq(n=1000, df=5). Please use mle to estimate df.

# the data
x=rchisq(n=1000, df=5)

# the likelihood
nLL <- function(dof) -sum(dchisq(x, df = dof,log=TRUE))

dof_estimation1 <-mle(nLL,start = list(dof=1), method = "L-BFGS-B")
# The degrees of freedom estimation is 4.905831 (close to 5)
dof_estimation1

# 2. Try different optimization methods to see how are their precisions.

dof_estimation2 <-mle(nLL,start = list(dof=1), method = "BFGS")
dof_estimation2 # 5.03992

dof_estimation3 <-mle(nLL,start = list(dof=1), method = "CG")
dof_estimation3 # 5.03992

dof_estimation3 <-mle(nLL,start = list(dof=1), method = "SANN")
dof_estimation3 # 5.039909, 5.039897, changes each time I run it

dof_estimation3 <-mle(nLL,start = list(dof=1), method = "Brent")
dof_estimation3 # 5.039897

dof_estimation3 <-mle(nLL,start = list(dof=1), method = "Nelder-Mead")
dof_estimation3 # 5.040625

# All of the methods produce a similar result (around 5 dof). SANN method changed
# each time I ran it, and they all produced results larger than 5 and larger than
# L-BFGS-B method.
 
# 3. You are given a vector of length 1000: xp
# We know that this sample follows a distribution called “Poisson distribution”. 
# Please try to figure out what parameters a Poisson distribution should have, 
# how to use R to analyze it, and use mle to estimate the related parameters. 

xp <- c(5,4,5,6,6,4,1,8,1,4,4,8,6,6,5,5,7,4,9,6,4,5,7,5,2,5,5,4,5,5,3,7,0,3,0,6,5,3,4,1,8,6,2,8,3,6,6,4,4,2,5,6,5,6,2,2,8,4,4,3,10,5,1,4,5,3,7,4,6,7,7,4,8,8,6,7,5,7,1,9,5,4,8,4,5,3,4,4,4,3,5,4,7,5,9,5,6,9,2,5,2,1,1,5,3,5,6,7,4,6,5,10,5,9,5,3,6,7,9,6,2,8,6,4,4,5,6,7,2,3,6,5,3,3,8,3,3,6,6,6,3,2,6,7,7,5,8,6,5,3,7,3,6,3,4,5,8,5,2,4,6,4,6,4,3,6,6,7,4,5,3,3,6,4,4,7,5,6,6,5,7,2,4,3,3,10,6,8,5,9,5,7,4,4,7,9,8,9,5,3,6,4,4,4,5,6,6,4,5,9,7,12,5,3,7,3,9,1,5,6,1,6,4,6,7,4,6,2,1,3,4,9,8,5,5,3,7,8,5,6,6,10,7,5,3,3,6,3,9,7,2,5,7,5,3,8,5,5,5,8,7,4,3,4,7,3,5,3,3,4,3,3,5,4,3,3,6,5,3,6,6,6,3,3,5,5,4,6,7,6,4,5,3,4,2,4,6,6,5,5,3,5,9,5,2,6,5,8,6,3,4,3,12,11,6,3,6,4,5,6,7,4,5,6,3,8,4,4,3,6,5,5,7,7,4,6,5,4,2,6,7,2,10,4,4,6,10,3,4,5,5,5,3,5,6,3,2,7,6,5,4,4,5,6,1,1,6,6,4,7,5,10,7,6,1,5,4,3,7,7,7,8,5,6,4,3,3,5,3,2,3,5,9,3,6,5,4,6,7,5,3,6,3,7,5,3,4,7,4,8,3,3,10,6,4,5,10,7,5,5,5,2,4,3,6,12,7,5,4,6,6,3,0,3,2,6,7,3,3,2,6,8,6,7,5,3,6,0,6,6,4,7,3,5,0,5,2,7,3,5,4,6,5,3,9,6,4,1,6,10,2,6,1,2,6,5,1,8,3,2,3,5,5,6,4,9,0,5,8,7,4,9,4,3,5,5,7,1,5,4,10,3,4,4,3,2,5,4,4,4,4,6,5,3,4,1,3,3,8,11,3,8,7,4,6,9,4,4,5,6,8,3,4,6,4,6,11,4,5,4,6,5,5,4,10,2,1,7,7,2,8,4,4,5,5,4,5,6,7,3,7,4,5,7,6,4,5,7,2,2,6,7,3,9,0,2,6,10,5,2,6,4,8,5,10,3,6,7,6,3,6,2,3,1,3,3,2,4,4,8,9,3,8,5,6,6,3,8,2,4,1,6,6,6,4,7,6,4,4,0,2,3,6,2,4,3,4,6,8,6,4,4,7,8,8,6,3,4,8,5,2,2,4,4,4,5,0,3,2,4,6,4,4,5,2,6,6,4,5,5,5,8,6,8,6,4,4,4,2,2,4,4,6,3,3,2,7,9,14,2,6,4,6,5,3,7,7,4,5,5,6,6,7,2,2,3,6,4,8,5,7,4,4,7,7,9,9,6,7,6,7,7,6,6,8,5,6,5,6,6,4,5,4,6,3,4,4,4,5,2,5,5,7,2,8,2,5,4,5,2,5,2,3,4,6,1,10,8,6,4,4,5,3,4,8,8,5,7,10,1,3,5,0,1,11,3,5,5,4,11,6,2,6,2,4,3,7,2,6,4,4,3,6,11,4,6,3,9,3,5,6,2,8,4,4,4,5,5,6,3,3,1,3,4,8,4,3,6,5,5,6,4,5,5,2,2,4,7,3,2,1,4,3,5,7,8,3,9,8,3,3,4,5,8,5,6,11,4,6,3,4,5,6,2,1,5,6,3,3,3,2,8,2,2,7,8,2,3,6,4,0,9,5,7,8,4,9,4,4,3,7,4,3,6,4,6,3,2,9,6,6,6,6,7,4,8,3,3,5,3,5,7,7,2,8,6,2,2,3,12,6,9,3,6,9,5,4,3,3,5,2,6,6,2,2,7,5,2,3,8,5,3,4,8,2,1,3,7,7,6,2,3,5,5,3,4,7,6,3,9,6,4,6,7,3,1,6,4,6,8,3,1,4,4,7,4,2,8,6,1,10,5,8,6,3,8,4,6,7,6,5,6,4,5,8,5,4,5,8,5,7,3,6,4,7,8,7,3,2,2,5,4,4,3,5,7,5,5,6,6)

# A poisson distribution expresses the probability of a given number of events
# occuring in a fixed interval of time or space if these events occur with a known
# constant rate and independently of the time since the last event (wiki). Density
# is calculated using p(x) = λ^x exp(-λ)/x!

# The parameters a Poisson distribution should have are k (number of events like
# 0, 1, 2, ...), lambda (mean number of events in the given time interval or 
# region of space).

# R can analyze the Poisson Distribution using:
# dpois(x, lambda, log = FALSE)
# ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)
# qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)
# rpois(n, lambda)

# We can use mle to estimate the related parameters

nLL_pd <- function(lamb) -sum(dpois(xp, lambda=lamb,log=TRUE))

pd_estimation <-mle(nLL_pd,start = list(lamb=1), method = "L-BFGS-B")
pd_estimation

# this estimates that lambda is 4.964

# 4. Design an mle procedure to solve a linear regression 
#	Y~ β0 + β1X1 + β2X2 + ε
#	Figure out how precise is your outcome with respect to the following properties:
  #	Sample size (the length of Y);
  #	The variance of ε (σ).

# n = 100
x1 = rnorm(100, mean = 2.0)
x2 = rnorm(100, mean = 1.0)
# Created a Y function of random values I picked
Y = 5*x1 + 5*x2 + rnorm(100, mean = 5, sd = 2) # mean = B0, sd = random number I picked?


# Looking for these parameters: 
# We only know the distribution of epsilon, solve for epsilon where ε N(0,σ)
# B0 = ? should be 5
# B1 = ? should be 5
# B2 = ? should be 5
# sigma = ?

nLL_Y <- function(B0, B1, B2, sigma) -sum(dnorm(Y-B0-(B1*x1)-(B2*x2), mean=0, sd=sigma, log=TRUE))

Y_estimation <-mle(nLL_Y,start = list(B0 = 2, B1 = 2, B2 = 2, sigma = 2), method = "SANN")
Y_estimation

# Estimations are all close to 5, and sigma is similar to 2

# Sample size
# If sample size (n) increases, then the estimations get more accurate. 
x1 = rnorm(1000, mean = 2.0)
x2 = rnorm(1000, mean = 1.0)
Y = 5*x1 + 5*x2 + rnorm(1000, mean = 5, sd = 2)
nLL_Y <- function(B0, B1, B2, sigma) -sum(dnorm(Y-B0-(B1*x1)-(B2*x2), mean=0, sd=sigma, log=TRUE))
Y_estimation <-mle(nLL_Y,start = list(B0 = 2, B1 = 2, B2 = 2, sigma = 2), method = "SANN")
Y_estimation

x1 = rnorm(10000, mean = 2.0)
x2 = rnorm(10000, mean = 1.0)
Y = 5*x1 + 5*x2 + rnorm(10000, mean = 5, sd = 2)
nLL_Y <- function(B0, B1, B2, sigma) -sum(dnorm(Y-B0-(B1*x1)-(B2*x2), mean=0, sd=sigma, log=TRUE))
Y_estimation <-mle(nLL_Y,start = list(B0 = 2, B1 = 2, B2 = 2, sigma = 2), method = "SANN")
Y_estimation

# The variance of ε (σ).
# As the variance increases, the estimation gets worse. 
x1 = rnorm(100, mean = 2.0)
x2 = rnorm(100, mean = 1.0)
Y = 5*x1 + 5*x2 + rnorm(100, mean = 5, sd = 10)
nLL_Y <- function(B0, B1, B2, sigma) -sum(dnorm(Y-B0-(B1*x1)-(B2*x2), mean=0, sd=sigma, log=TRUE))
Y_estimation <-mle(nLL_Y,start = list(B0 = 2, B1 = 2, B2 = 2, sigma = 2), method = "SANN")
Y_estimation

x1 = rnorm(100, mean = 2.0)
x2 = rnorm(100, mean = 1.0)
Y = 5*x1 + 5*x2 + rnorm(100, mean = 5, sd = 15)
nLL_Y <- function(B0, B1, B2, sigma) -sum(dnorm(Y-B0-(B1*x1)-(B2*x2), mean=0, sd=sigma, log=TRUE))
Y_estimation <-mle(nLL_Y,start = list(B0 = 2, B1 = 2, B2 = 2, sigma = 2), method = "SANN")
Y_estimation
