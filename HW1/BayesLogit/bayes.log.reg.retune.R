bayes.log.reg.retune<-function(p, m, y, X, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=500, verbose=FALSE)
{


#marginal posterior distribitons for all coordinates
marg.post.distr<-0

#percentiles of marginals for all coordinates
percentiles.marg<-0
  
#vector of zeros as starting point for MCMC 
beta<-vector("numeric", p)
sd.initial<-1

#keeps track of number of accepts for retune iterations
num.accepts<-vector("numeric", p)
#SDs for each coordinate
sds<-rep(sd.initial, p)

#should we continue retuning the particular coordinate?
retune.done<-vector("logical", p)

if(verbose)
{
  cat("beta = \n")
  print(beta)
  cat("\n")
  
  cat("num.accepts = \n")
  print(num.accepts)
  cat("\n")
  
  cat("sds = \n")
  print(sds)
  cat("\n")
}


for(i in 1:niter)
{
  
  if(verbose && (i %% 500 == 0))
  {
    cat("iteration ",i,"   ")
    print(beta)    
  }
  
  for(pos in 1:p)
  {    
    result<- metropolis.retune(p, beta, pos, num.accepts[[pos]], sds[[pos]], X, y, m, verbose=FALSE)
    
    beta[[pos]]<-result[[1]]
    num.accepts[[pos]]<-result[[2]]
  }
  
  #retuning during the burnin period
  if((i <= burnin) && (i %% retune == 0))
  {
    
    k<-i/retune
    for(j in 1:p)
    {
      #if done retuning, then leave SD as it is
      if(retune.done[[j]])
      {
        next
      }
      
      accept.rate<-num.accepts[[j]]/retune
      if(accept.rate<0.22)
      {
        sds[[j]]<-sds[[j]]-(.5)^k
      }
      else 
      {
        if(accept.rate>0.6)
        {
          sds[[j]]<-sds[[j]]+(.5)^k
        }
        else
        {
          retune.done[[j]]<-TRUE
        }
      }
    }
    
    if(verbose)
    {
      cat("num.accepts = \n")
      print(num.accepts)
      cat("\n")
      
      cat("sds = \n")
      print(sds)
      cat("\n")
      
      cat("retune.done = \n")
      print(retune.done)
      cat("\n")
      
    }
    num.accepts<-rep(0, p)
    
  }
    
  if(i>burnin)
  {
    marg.post.distr<-cbind(marg.post.distr, beta)
  }
  
    
}

marg.post.distr<-marg.post.distr[1:2,2:(niter-burnin+1)]

percentiles.marg<-apply(marg.post.distr,1,quantile,probs=seq(.01,.99,.01))

write.table(percentiles.marg, file="quantiles.csv",sep=",",row.names = FALSE, col.names = FALSE)

   

 
 if(verbose)
 {
   cat("marg.post.distr = \n")
   print(dim(marg.post.distr))
   print(marg.post.distr[1:2,1:10])
   cat("\n")
 }

 if(verbose)
 {
   cat("num.accepts = \n")
   print(num.accepts)
   cat("\n")
 }

if(verbose)
{
  cat("quantiles of beta = \n")
  print(percentiles.marg)
  cat("\n")
}
   
 

}




