library(RCUDA)
library(MASS)
library(truncnorm)

#function samples from the posterior beta|Z
cond_sample <- function(Z, X, XX, beta_0, sigma_0_inv ){
  ZX = colSums(Z*X)
  Sigma = solve(sigma_0_inv + XX)
  Mu = Sigma%*%(sigma_0_inv%*%beta_0 + ZX)
  betas = mvrnorm(1,Mu,Sigma)
  betas = as.vector(cond_sample)
  return(betas)
}
