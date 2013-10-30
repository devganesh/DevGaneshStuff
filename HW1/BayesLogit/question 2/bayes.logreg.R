#this function does a BLR fit for a given dataset


#Note:  the if(verbose) clauses are purely for debugging

bayes.logreg<-function(p, m, y, X, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=500, verbose=FALSE, file.num)
{


#marg posterior distribitons for each beta coordinate
marg.post.distr<-vector("numeric", 0)
  
#percentiles of marginals for each beta coordinate
percentiles.marg<-0
  
  
#diagonal element of sigma matrix for prior
sigma.factor<-sigma.0.inv[1,1]
  

  
#vector of zeros as starting point for MCMC 
beta<-vector("numeric", p)
sd.initial<-1

#to keep tracks of number of acceptances for every 500 iterations of the burnin period
num.accepts<-vector("numeric", p)

#SDs to be used for proporsal distribution for each beta coordinate
sds<-rep(sd.initial, p)

#keeps track of whether retuning is done or not for each beta coordinate
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
  
  if(verbose )
  {
    if(i %% 500 == 0)
      {
      cat("iteration ",i,"   ")
      print(beta)    
    }
  }
  
  #runs the metropolis-hastings sampling for each beta coordinate
  for(pos in 1:p)
  {    
    result<- metropolis.retune(p, beta, pos, num.accepts[[pos]], sds[[pos]], X, y, m, sigma.factor, verbose=FALSE)
    
    beta[[pos]]<-result[[1]]
    num.accepts[[pos]]<-result[[2]]
  }
  
  #retuning during the burnin period
  if((i <= burnin) && (i %% retune == 0))
  {
    
    k<-i/retune
    for(j in 1:p)
    {
      #if done retuning, then leave SD as it is and goto next coordinate
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
    # resets the num.accepts variable for the next 500 iterations of the burin period
    num.accepts<-rep(0, p)
    
  }
    
  #keeps track of the MC for each beta coordinate after the burnin period
  if(i>burnin)
  {
    marg.post.distr<-cbind(marg.post.distr, beta)
  }
    
}

marg.post.distr<-marg.post.distr[1:p,1:(niter-burnin)]

output.file.name<-paste("blr_res_",as.character(file.num),".csv", sep="")
output.file.path<-paste("results/",output.file.name, sep="")

percentiles.marg<-apply(marg.post.distr,1,quantile,probs=seq(.01,.99,.01))

write.table(percentiles.marg, output.file.path, sep=",", row.names = FALSE, col.names = FALSE)

 
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




