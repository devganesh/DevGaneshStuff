#include <stdio.h>
#include <stdlib.h>
#include <cuda.h>
#include <curand_kernel.h>
#include <math_constants.h>
extern "C"
{
__global__ void truncnorm_kernel(float *x, int n, float *mu, float *sigma,
float *a, float *b,int rng_a, int rng_b, int rng_c,int maxtries)
{
  // Usual block/thread indexing... 
  int myblock = blockIndex.x + blockIndex.y * gridDim.x;
  int blocksize = blockDim.x * blockDim.y * blockDim.z;
  int subthread = threadIndex.z*(blockDim.x * blockDim.y) + threadIndex.y*blockDim.x + threadIndex.x;
  int index = myblock * blocksize + subthread;
  curandState rng;
  curand_init(rng_a + index*rng_b, rng_c, 0, &rng);                                                                           
  int numtries = 1;
  float uExp, alpha, z, psi, u;
  
  if(index>n) return;

  // Rejection sampling 
  while(numtries <= maxtries)
  {
    x[index] = mu[index] + sigma[index]*curand_normal(&rng);
    if( x[index] >= a[index] && x[index] <= b[index]) return;
    numtries++;
  }
  //If all samples rejected, use methods given in Robert(2009)  
  while(1)
  {
    // for the case when the lower limit of the truncation region is not finite
    if(!isfinite(a[index]))
    {
      a[index] = -b[index];
      b[index] = CUDART_NAN_F;
    }
    uExp = curand_uniform(&rng);                
    alpha = (a[index] + sqrt(a[index]*a[index] + 4))/2;
    z = a[index] - (log(uExp)/alpha);
    
    if(a[index] < alpha)  psi = exp(-(alpha - z)*(alpha - z)/2);
    else  psi = exp(-(alpha - z)*(alpha - z)/2)*exp(-(a[index] - alpha)*(a[index] - alpha)/2);
    u = curand_uniform(&rng);
    if(u < psi)
    {
      x[index] = sigma[index]*z + mu[index];
      if(!isfinite(a[index])) x[index] = -x[index];
      return;
    }
  }     return;}} // END extern "C"
