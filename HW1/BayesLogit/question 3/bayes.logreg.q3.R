bayes.logreg.q3<-function(p, m, y, X, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=500, verbose=FALSE, file.num)
{


#diagonal element of sigma matrix for prior
sigma.factor<-sigma.0.inv[1,1]
  
#marg posterior distribitons for all coordinates
marg.post.distr<-0

#percentiles of marginals for all coordinates
percentiles.marg<-0
  
#starting point for MCMC 
beta<-rep(0,p)

#keeps track of number of accepts for retune iterations
num.accepts<-vector("numeric", p)

#SDs for each coordinate
sd.initial<-1
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
    #if(verbose){cat(pos," ")}
    result<- metropolis.retune.q3(p, beta, pos, num.accepts[[pos]], sds[[pos]], X, y, m, sigma.factor, verbose=FALSE)
    
    beta[[pos]]<-result[[1]]
    num.accepts[[pos]]<-result[[2]]
  }
  
  #if(verbose){cat("metropolis done")}
  
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

marg.post.distr<-marg.post.distr[1:p,2:(niter-burnin+1)]

output.file.name<-paste("blr_res_",as.character(file.num),".csv", sep="")
output.file.path<-paste("results/",output.file.name, sep="")

percentiles.marg<-apply(marg.post.distr,1,quantile,probs=seq(.01,.99,.01))

write.table(percentiles.marg, output.file.path, sep=",", row.names = FALSE, col.names = FALSE)

 
 if(verbose)
 {
   cat("marg.post.distr = \n")
   print(dim(marg.post.distr))
   print(marg.post.distr[1:p,1:10])
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
   
return(marg.post.distr)

}




