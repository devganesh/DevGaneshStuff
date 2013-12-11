library(RCUDA)
library(MASS)
library(truncnorm)

#function samples from the posterior beta|Z
cond_sample <- function(Z, X, XX, beta_0, sigma_0_inv ){
  ZX = colSums(Z*X)
  
  sigma_beta = solve(sigma_0_inv + XX)
  mu_beta = sigma_beta%*%(sigma_0_inv%*%beta_0 + ZX)
  beta_sample = mvrnorm(1,mu_beta,sigma_beta)
  beta_sample = as.vector(cond_sample)
  return(beta_sample)
}