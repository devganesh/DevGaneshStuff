#to clear screen
cat("\014")

setwd("~/Desktop/STA250/DevGaneshStuff/HW1/BayesLogit")
source("log.st.distr.retune.R")
source("metropolis.retune.R")
source("bayes.logreg.R")

#reading in from dataset
true.beta<-read.csv("sample_datasets/blr_pars_1002.csv")
dataset<-read.csv("sample_datasets/blr_data_1002.csv")

#readying function inputs from dataset
y<-dataset[["y"]]
m<-dataset[["n"]]
X<-as.matrix(subset(dataset, select=X1:X2))

p<-2
beta.0 <- matrix(c(0,0))
sigma.0.inv <- diag(rep(1/1000,p))
file.num<-20

#beta<-c(1, 0.5)
#pos<-1

#log.st.distr(beta, pos, X, y, m, TRUE)

#metropolis(beta, pos, X, y, m, verbose=TRUE)

#bayes.log.reg.(m, y, X, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=1000, verbose=TRUE)

bayes.logreg(p, m, y, X, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=500, verbose=TRUE, 2)

#bayes.logreg.q3(p, m, y, X, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=500, verbose=TRUE, file.num)
