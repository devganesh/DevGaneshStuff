# script implements the "Bag of Little Bootstraps" algorithm to fit
# a linear regression model to a massive dataset


# Loading packages
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)


# number of distinct subsets
s = 5
# number of bootstrap samples
r = 50

#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 1000
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

# Find r and s indices
jn=sim_num-sim_start
if (jn %% r == 0)
{
  s_index=jn/r
  r_index=r
}else
{
  s_index=ceiling(jn/r)
  r_index=jn %% r
}

# ====================Fitting the linear model using BLB==============

# mini or full?
if (mini){
  rootfilename <- "blb_lin_reg_mini"
} else {
  rootfilename <- "blb_lin_reg_data"
}

# paths for input and output (folders on Gauss were made to 
# imitate those on local Desktop)
input.path <- "~/Desktop/STA250/DevGaneshStuff/HW2/BLB/data/"
output.path <- "~/Desktop/STA250/DevGaneshStuff/HW2/BLB/output/"

# getting data from file
full.dataset = attach.big.matrix(paste(input.path, rootfilename, ".desc", sep = ""))

# Other BLB parameters
gamma = 0.7
n = nrow(full.dataset)
b = floor(n^gamma)

# Extracting subset
set.seed(s_index)
sample.rows = sample(1:n, b, replace = FALSE)
sample<-data.frame(full.dataset[sample.rows,])

# Taking bootstrap sample 
set.seed(jn)
weights = rmultinom(1 , n, prob = rep(1/b, b))

# Fitting the linear model with no intercept
coefficients = lm(X1001~.-1, data = sample, weights = weights)$coefficient

# Outputting estimates to file
outfile.name = paste0(output.path,"coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt", sep = "")
write.table(coefficients, file = outfile.name, row.names=FALSE, col.names = FALSE)



