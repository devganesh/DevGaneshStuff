library(RCUDA)
library(MASS)
library(truncnorm)

probit_mcmc_cpu =function(n, Z, X, XX,a,b,beta_0,Sigma_0_inv,niter,burnin, sd, beta_sample)
{
  for(t in 1:(burnin+niter))
  {
    #Sample Z using beta and Y
    mean = X%*%beta
    Z = rtruncnorm(1, a, b, mean, sd)
    #Sample beta using Z
    beta = cond_sample(Z, X, XX, beta_0, Sigma_0_inv)
    beta_sample= rbind(beta_sample, beta) 
  }
 beta_mean = beta_sample[(burnin+niter),]
 filename<-paste("beta_mcmc_CPU,txt",sep="")
  write.table(as.matrix(beta_mean), file=filename,row.names = FALSE, col.names = FALSE, sep=" ")
}


