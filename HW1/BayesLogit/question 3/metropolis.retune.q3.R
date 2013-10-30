#This function does the metropolis-hastings sampling with the Gibbs iterations of BLR.

#Note:  the if(verbose) clauses are purely for debugging purposes.

metropolis.retune.q3<-function(p, beta, pos, num.accepts, sd, X, y, m, sigma.factor, verbose=FALSE)
{
  #sampling from proposal distribution 
  beta.t<-beta[[pos]]
  beta.propose<-rnorm(1, beta.t, sd)
  
  if(verbose)
  {
    cat("beta.t = \n")
    print(beta.t)
    cat("\n")
    
    cat("beta.propose = \n")
    print(beta.propose)
    cat("\n")
  }
  
  #computing probabilities for the current and proposed values of beta[pos]
  beta[[pos]]<-beta.t
  log.pi.beta.original<-log.st.distr.retune.q3(p, beta, pos, X, y, m, sigma.factor, verbose=FALSE)
  
  beta[[pos]]<-beta.propose  
  log.pi.beta.propose<-log.st.distr.retune.q3(p, beta, pos, X, y, m, sigma.factor, verbose=FALSE)
  
  if(verbose)
  {
    cat("log.pi.beta.original = \n")
    print(log.pi.beta.original)
    cat("\n")
    
    cat("log.pi.beta.propose = \n")
    print(log.pi.beta.propose)
    cat("\n")
  }
  
  log.u<-log(runif(1, 0, 1))
  
  #computing the value of alpha in metropolis-hastings algorithm
  #Note - logarithms were taken for ease of computation
  log.alpha<-log.pi.beta.propose - log.pi.beta.original
  
  if(verbose)
  {
    cat("log.u = \n")
    print(log.u)
    cat("\n")
    
    cat("log.alpha = \n")
    print(log.alpha)
    cat("\n")
    
    cat("comparison = \n")
    print(log.u < log.alpha)
    cat("\n")
  }
  
  #checks whether to accept or reject the proposal for beta[pos]
  if(log.u < log.alpha)
  {
   num.accepts<-num.accepts+1
   beta.t<-beta.propose
  }
  
  result<-c(beta.t, num.accepts)
  return(result)
}