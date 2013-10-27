metropolis.retune<-function(p, beta, pos, num.accepts, sd, X, y, m, sigma.factor, verbose=FALSE)
{
  #proposal distribution mean with SD 'v'
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
  
  beta[[pos]]<-beta.propose  
  log.pi.beta.propose<-log.st.distr.retune(p, beta, pos, X, y, m, sigma.factor, verbose=FALSE)
  
  beta[[pos]]<-beta.t
  log.pi.beta.original<-log.st.distr.retune(p, beta, pos, X, y, m, sigma.factor, verbose=FALSE)
  
  if(verbose)
  {
    cat("log.pi.beta.propose = \n")
    print(log.pi.beta.propose)
    cat("\n")
          
    cat("log.pi.beta.original = \n")
    print(log.pi.beta.original)
    cat("\n")
    
  }
  
  log.u<-log(runif(1, 0, 1))
  
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
  
  if(log.u < log.alpha)
  {
   num.accepts<-num.accepts+1
   beta.t<-beta.propose
  }
  
  result<-c(beta.t, num.accepts)
  return(result)
}