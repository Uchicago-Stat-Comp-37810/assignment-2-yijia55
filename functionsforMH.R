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

slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
#the function slopevalues inputs slope of the model and returns value of log likelihood function 
#calculated with test data, given slope x, fixed intercept and sd of error


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


### Summary: #######################
graphing <- function(chain,burnIn,classnum,truevalue){
  num <- length(truevalue)
  par(mfrow=c(2,num))
  for(i in 1:num){
    hist(chain[-(1:burnIn),i],nclass=classnum, main=paste("Posterior of param",i), xlab="True value = red line" )
    abline(v = mean(chain[-(1:burnIn),i]))
    abline(v = truevalue[i], col="red" )
  }
  for(i in 1:num){
    plot(chain[-(1:burnIn),i], type = "l", xlab="True value = red line" , main = paste("Chain values of param",i) )
    abline(h = truevalue[i], col="red" )
  }
}


