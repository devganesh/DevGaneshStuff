library(RCUDA)
library(MASS)
library(truncnorm)

probit_mcmc_gpu =function( n, Z, X, XX,a, b, beta_0, Sigma_0_inv,niter,burnin, 
                maxtries, rng_a, rng_b, block_dims, grid_dims, sd, beta_sample)

{
  cuGetContext(TRUE)
  m <- loadModule("truncnorm.ptx")
  truncnorm_kernel <- m$truncnorm_kernel
  
  for(i in 1:(burnin+niter))
    {
    #Sample Z using beta and Y
    mean = X%*%beta
    cudaDeviceSynchronize()
    .cuda(truncnorm_kernel, x, n, mean, sd, a, b, rng_a, rng_b, "rng_c"=i, maxtries,grid_dims, block_dims, outputs)})
    
    #Sampling beta using Z
    beta = cond_sample(Z, X, XX, beta_0, Sigma_0_inv)
    beta_sample = rbind(beta_sample, beta) 
    }
#Write the betas to file
beta_mean = beta_sample[(burnin+niter),]
filename<-paste("beta_mcmc_GPU,txt",sep="")
write.table(as.matrix(beta_mean), file=filename,row.names = FALSE, col.names = FALSE, sep=" ")
}



