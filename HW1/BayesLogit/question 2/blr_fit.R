# #to clear screen
# cat("\014")

setwd("~/Desktop/STA250/DevGaneshStuff/HW1/BayesLogit")

source("question 2/log.st.distr.retune.R")
source("question 2/metropolis.retune.R")
source("question 2/bayes.logreg.R")

##
#
# Logistic regression
# 
# Y_{i} | \beta \sim \textrm{Bin}\left(n_{i},e^{x_{i}^{T}\beta}/(1+e^{x_{i}^{T}\beta})\right)
# \beta \sim N\left(\beta_{0},\Sigma_{0}\right)
#
##

#library(mvtnorm)
#library(coda)

########################################################################################
########################################################################################
## Handle batch job arguments:

# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 1000
length.datasets <- 200
#######################

if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}

#Simulation datasets numbered 1001-1200

########################################################################################
########################################################################################

p<-2
#################################################
# Set up the specifications:
beta.0 <- matrix(c(0,0))
sigma.0.inv <- diag(rep(1.0,p))
niter <- 15000
#################################################

#setting up the file name and path for the particular dataset being read
file.num<-1000+as.numeric(args[1])
input.file.name<-paste("blr_data_",as.character(file.num),".csv", sep="")
input.file.path<-paste("data/",input.file.name, sep="")

#reading in from dataset
true.beta<-read.csv(input.file.path)
dataset<-read.csv(input.file.path)

#readying function inputs from dataset
y<-dataset[["y"]]
m<-dataset[["n"]]
X<-as.matrix(subset(dataset, select=X1:X2))

#reset seed to random one
rm(.Random.seed, envir=globalenv())

bayes.logreg(p, m, y, X, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=500, verbose=FALSE, file.num)











