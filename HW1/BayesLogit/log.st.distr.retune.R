log.st.distr.retune<-function(p, beta, pos, X, y, m, verbose=FALSE)
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
    print(beta)
    cat("\n")
    
    cat("dim of X = \n")
    print(dim(X))
    cat("\n")
    
    cat("X = \n")
    print(X)
    cat("\n")
  }
  
  u<-X %*% beta
  
  if(verbose)
  {
    cat("X * beta = \n")
    print(u)
    cat("\n")
  }
  
 logit.inv<-exp(u)/(1+exp(u))
 one.minus.logit.inv<-1-logit.inv 
  
  if(verbose)
  {
    cat("logit.inv = \n")
    print(logit.inv)
    cat("\n")
    
    cat("one.minus.logit.inv = \n")
    print(one.minus.logit.inv)
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
  
  log.logit.inv<-log(logit.inv)
  log.one.minus.logit.inv<-log(one.minus.logit.inv)
  
  if(verbose)
  {
    sum<-0
    for(i in 1:100)
    {
      sum<- sum + y[[i]]*log.logit.inv[[i]] + (m[[i]]-y[[i]])*log.one.minus.logit.inv[[i]] 
    }
    
    cat("sum = \n")
    print(sum)
    cat("\n")
    
  }
  
  log.prob.y.given.beta<-(y %*% log.logit.inv) + ((m-y) %*% log.one.minus.logit.inv)
  log.prob.beta.cond<-(-1/2)*(beta[[pos]])^2
  log.pi<-log.prob.y.given.beta + log.prob.beta.cond
  
  return(log.pi)
  
  
  if(verbose)
  {
    cat("log.logit.inv = \n")
    print(log.logit.inv)
    cat("\n")
    
    cat("log.one.minus.logit.inv = \n")
    print(log.one.minus.logit.inv)
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
  
  
  
}