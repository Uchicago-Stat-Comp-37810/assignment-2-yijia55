source("functionsforMH.R")
#generate data of the  linear model
trueA <- 5 #give value to A(slope)
trueB <- 0 #give value to B(intercept)
trueSd <- 10 #give value to the standard error of error
sampleSize <- 31 #give value to samplesize

x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create independent x-values 

y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)
# create dependent values according to ax + b + N(0,sd)
plot(x,y, main="Test Data")
#plot data

# Example: plot the likelihood profile of the slope a
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
#seq(3,7,by=0.5) as input of slopevalues and calculate each log likelihood 
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a",
      ylab = "Log likelihood")
#plot the value of log likelihood with a seq of slope as x

startvalue = c(4,0,10)#value to initialize
chain = run_metropolis_MCMC(startvalue, 10000)
#10000 iterations to get a chain using Metropolis-Hastings method

burnIn = 5000 #number to give up
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))
#calculate the rate of acceptance from burnIn+1 to 10000 iterations

graphing(chain,burnIn,30,c(5,0,10))

# for comparison:
summary(lm(y~x))#result of linear regression