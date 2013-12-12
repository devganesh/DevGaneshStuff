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
  block_dim <- c(512L, 1L, 1L)STA 250 - 
  grid_dim <- c(as.integer(ceiling(N/threads_per_block)), 1L, 1L)
  cudaDeviceSynchronize()
  gpu_time <- system.time(time_copy_forward <- system.time({mem <- copyToDevice(x)})
  time_kernel <- system.time(.cuda(truncnorm_kernel, mem, N, mu, sigma, a, b, 
                                       rng_a, rng_b, rng_c, maxtries,
                                       grid_dim, block_dim, outputs=NULL))
  time_copy_backward <- system.time({time_cuda <- copyFromDevice(mem,mem@nels,type)}))
  cpu_time <- system.time(rtruncnorm(N, a, b, mean = mu, sd = sigma))
  
  return(c(gpu_time, cpu_time))
}

