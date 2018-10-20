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

#the function inputs parameters and return the value of likelihood fuction with obs of test data
likelihood <- function(param){
  a = param[1]#give value of param[1] to slope
  b = param[2]#give value of param[2] to intercept
  sd = param[3] #give value of param[3] to the standard error of error
  
  pred = a*x + b #calculate predicted value of linear model
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) 
  #calculate vector whose elements are logf(y),f(y) is density of N(pred,sd), 
  #y is from test data
  sumll = sum(singlelikelihoods) #calculate log of likelihood function
  return(sumll)   #return the value of log likelihood function calculated with param
}

# Example: plot the likelihood profile of the slope a
par(mfrow=c(1,1))
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
#the function slopevalues inputs slope of the model and returns value of log likelihood function 
#calculated with test data, given slope x, fixed intercept and sd of error
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
#seq(3,7,by=0.5) as input of slopevalues and calculate each log likelihood 
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a",
      ylab = "Log likelihood")
#plot the value of log likelihood with a seq of slope as x

# Prior distribution of parameters of linear model
prior <- function(param){
  a = param[1]#give value of param[1] to a
  b = param[2]#give value of param[2] to b
  sd = param[3]#give value of param[3] to sd
  aprior = dunif(a, min=0, max=10, log = T)
  #calculate value of log f(a),f is the density function of uniform[0,10]
  bprior = dnorm(b, sd = 5, log = T)
  #calculate value of log f(b),f is the density function of N(0,25)
  sdprior = dunif(sd, min=0, max=30, log = T)
  #calculate value of log f(sd),f is the density function of uniform[0,30]
  return(aprior+bprior+sdprior)
  #return the value of log likelihood function of parameters
}

#This function posterior gives the value of log of posterior likelihood function 
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

######## Metropolis algorithm ################

#generate parameters from jumping distribution
proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

#The function takes initial value of parameters and iteration times as input and
#output a chain of parameters taken from posterior function
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))#chain is a (iterations+1)*3 dimension matrix
  chain[1,] = startvalue#give initial value to chain[1,]
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])#generate candidate from jumping distribution
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))#calculate acceptance ratio
    if (runif(1) < probab){
      chain[i+1,] = proposal #accept candidate and add it to chain with probability of probab
    }else{
      chain[i+1,] = chain[i,]#don't accept candidate and stay where you are 
    }
  }
  return(chain)#return the calculated chain
}

startvalue = c(4,0,10)#value to initialize
chain = run_metropolis_MCMC(startvalue, 10000)
#10000 iterations to get a chain using Metropolis-Hastings method

burnIn = 5000 #number to give up
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))
#calculate the rate of acceptance from burnIn+1 to 10000 iterations

### Summary: #######################
summary <- function(chain,burnIn,classnum,truevalue){
  num <- ncol(chain)
  par(mfrow=c(2,num))
  for(i in 1:num){
    hist(chain[-(1:burnIn),i],nclass=classnum, main=paste("Posterior of param",i), xlab="True value = red line" )
    abline(v = mean(chain[-(1:burnIn),i]))
    abline(v = truevalue[i], col="red" )
  }
  for(i in 1:num){
    plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = paste("Chain values of param",i) )
    abline(h = truevalue[i], col="red" )
  }
}
summary(chain,burnIn,30,c(5,0,10))

# for comparison:
summary(lm(y~x))#result of linear regression