#this function computes the probabilities needed in the metropolis-hastings sampling technique for a particular coodinate of beta

#Note: the if(verbose) clauses are purely for debugging

log.st.distr.retune<-function(p, beta, pos, X, y, m, sigma.factor, verbose=FALSE)
{
  n<-length(y)
  beta<-as.matrix(beta)
  
  if(verbose)
  {
    cat("n = \n")
    print(n)
    cat("\n")
    
    cat("dim of beta = \n")
    print(dim(beta))
    cat("\n")
    
    cat("beta = \n")
    print(t(beta))
    cat("\n")
    
    cat("dim of X = \n")
    print(dim(X))
    cat("\n")
    
    cat("X = \n")
    print(X)
    cat("\n")
  }
  
  #computing (x_i)'(beta) for all rows of the dataset
  u<-X %*% beta
  
  if(verbose)
  {
    cat("X * beta = \n")
    print(t(u))
    cat("\n")
  }
  
# computing the logit inverse probability for the binomial distribution of the likelihood.
logit.inv<-exp(u)/(1+exp(u))
one.minus.logit.inv<-1-logit.inv 
  
  if(verbose)
  {
    cat("logit.inv = \n")
    print(t(logit.inv))
    cat("\n")
    
    cat("one.minus.logit.inv = \n")
    print(t(one.minus.logit.inv))
    cat("\n")
    
    cat("y = \n")
    print(y)
    cat("\n")
    
    cat("m = \n")
    print(m)
    cat("\n")
    
    cat("m - y = \n")
    print(m-y)
    cat("\n")
  }
  
  #taking logs of the quatities involved for ease of computation by R
  log.logit.inv<-log(logit.inv)
  log.one.minus.logit.inv<-log(one.minus.logit.inv)
  
  if(verbose)
  {
    sum<-0
    for(i in 1:(length(y)))
    {
      sum<- sum + y[[i]]*log.logit.inv[[i]] + (m[[i]]-y[[i]])*log.one.minus.logit.inv[[i]] 
    }
    
    cat("sum = \n")
    print(sum)
    cat("\n")
    
  }
  
  #likelihood probability
  log.prob.y.given.beta<-(y %*% log.logit.inv) + ((m-y) %*% log.one.minus.logit.inv)
  #prior probability
  log.prob.beta.cond<-(-1/2)*sigma.factor*(beta[[pos]])^2
  #posterior probability
  log.pi<-log.prob.y.given.beta + log.prob.beta.cond
  
  if(verbose)
  {
    cat("log.logit.inv = \n")
    print(t(log.logit.inv))
    cat("\n")
    
    cat("log.one.minus.logit.inv = \n")
    print(t(log.one.minus.logit.inv))
    cat("\n")
    
    cat("p(y|beta) = \n")
    print(log.prob.y.given.beta)
    cat("\n")
    
    cat("p(cond beta) = \n")
    print(log.prob.beta.cond)
    cat("\n")
    
    cat("p(pi) = \n")
    print(log.pi)
    cat("\n")
    
  }
  
  return(log.pi)
  
  
}