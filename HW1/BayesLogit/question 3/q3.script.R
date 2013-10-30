#to clear screen
cat("\014")

library(coda)

setwd("~/Desktop/STA250/DevGaneshStuff/HW1/BayesLogit")
source("question 3/log.st.distr.retune.q3.R")
source("question 3/metropolis.retune.q3.R")
source("question 3/bayes.logreg.q3.R")

#reading in from dataset
dataset<-read.table("question 3/breast_cancer.txt", header = TRUE)

#readying function inputs from dataset
y<-as.vector(as.integer(dataset[,"diagnosis"]=="M"))
n<-length(y)
m<-1+vector("numeric", n)

# number of beta coordinates
p<-length(colnames(dataset))

X<-as.matrix(dataset[,1:p-1])
means<-as.matrix(apply(X,2,mean))
sds<-as.matrix(apply(X,2,sd))

#centering and scaling the X covariates
normalized.X<-scale(X, center=TRUE, scale=TRUE)

#adding intercepts to the normalized X covariates
intercepts.covariates<-cbind(matrix(rep(1,n)),normalized.X)

beta.0 <- matrix(c(0,0))
sigma.0.inv <- diag(rep(1/1000,p))

beta<-bayes.logreg.q3(p, m, y, intercepts.covariates, beta.0, sigma.0.inv, niter=15000, burnin=5000, print.every=1000, retune=500, verbose=TRUE)

#convert the marginal posterior distributions of beta into an MCMC object for diagnostics and analysis
beta.mcmc<-mcmc(t(beta))

#gives the important quantiles for each beta marginal.  
#If for a particular coordinate, 0 lies between the 2.5th and 97.5th quantile, then we conclude that 
#the coordinate does not influence the final cancer diagnosis
summary(beta.mcmc)

#to find the lag-1 autocorrelation of each marginal beta distribution
apply(beta, 1, acf, lag.max=1)

#for posterior predictive check
true.response.mean<-mean(y)

predictive.means<-vector("numeric", 0)

#samples a beta vector of length 'p' and genrates a dataset of responses for each sampled beta-vector
for(i in 1:500){
beta.sample<-apply(beta, 1, sample, size=1, replace=TRUE)
u<-intercepts.covariates %*% beta.sample
probs<-exp(u)/(1+exp(u))

responses<-apply(probs, 1, rbinom, n=1, size=1)
response.mean<-mean(responses)
predictive.means<-append(predictive.means, response.mean, after=i-1)
}

#plots a histrogram of the means of the generated datasets
hist(predictive.means, plot=TRUE)

#marks the mean of the actual dataset for the sake of comparing it with the generated means
abline(v=true.response.mean,col=12,lty=1)

