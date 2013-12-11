library(RCUDA)
library(truncnorm)


truncnorm=function(rng_a, rng_b, rng_c, maxtries, a, b, N, mu, sigma)
{
  cuGetContext(TRUE)
  m <- loadModule("truncnorm.ptx")
  truncnorm_kernel <- m$truncnorm_kernel

  #sampling using R's function
  x = rtruncnorm(N, a, b, mean = mu, sd = sigma)

  #sampling using GPU
  threads_per_block <- 512L
  block_dims <- c(threads_per_block, 1L, 1L)
  grid_d1 <- as.integer(ceiling(N/threads_per_block))
  grid_dims <- c(grid_d1, 1L, 1L)
  nthreads <- prod(grid_dims)*prod(block_dims)
  cudaDeviceSynchronize()
  cu_time <- system.time({
  cu_copy_to_time <- system.time({mem <- copyToDevice(x)})
  cu_kernel_time <- system.time({.cuda(truncnorm_kernel, mem, N, mu, sigma, a, b, rng_a, rng_b, rng_c, maxtries,
                                       gridDim=gritruncnorm_kerneld_dims, blockDim=block_dims, outputs=NULL)})
  cu_copy_back_time <- system.time({cu_ret <- copyFromDevice(obj=mem,nels=mem@nels,type)})
  })
  cu_kernel2_time <- system.time({y <- .cuda(truncnorm_kernel, x, N, mu, sigma, a, b, rng_a, rng_b, rng_c, maxtries,
                                           gridDim=grid_dims, blockDim=block_dims, outputs)})
  r_time <- system.time({r_ret <- rtruncnorm(N, a, b, mean = mu, sd = sigma)})
  tdiff <- sum(abs(cu_ret - r_ret))
}

