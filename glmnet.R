# Copyright (c) 2013
# Panos Toulis, ptoulis@fas.harvard.edu
# Modified by Kevin Kuate Fodouop
#
# Using the simulation setup as in glmnet JoSS paper(Friedman, Hastie, Tibshirani)
# http://www.jstatsoft.org/v33/i01/
#
# EXAMPLE run:
#  run.glmnet(1e4, 10)
#
rm(list=ls())
source("pset3.R")
library(mvtnorm)
library(glmnet)
library(lars)

# genjerry, genx2 are functions taken from the above paper.
# These functions generate the simulation data.
# NOTE: use function sample.data(..) instead.
genjerry = function(x, snr){
  # generate data according to Friedman's setup
  n=nrow(x)
  p=ncol(x)
  b=((-1)^(1:p))*exp(-2*((1:p)-1)/20)
  # b=sample(c(-0.8, -0.45, 0.45, 0.9, 1.4), size=p, replace=T)
  # ((-1)^(1:p))*(1:p)^{-0.65}#exp(-2*((1:p)-1)/20)
  f=x%*%b
  e=rnorm(n)
  k=sqrt(var(f)/(snr*var(e)))
  y=f+k*e
  return(list(y=y, beta=b))
}

genx2 = function(n,p,rho){
  #    generate x's multivariate normal with equal corr rho
  # Xi = b Z + Wi, and Z, Wi are independent normal.
  # Then Var(Xi) = b^2 + 1
  #  Cov(Xi, Xj) = b^2  and so cor(Xi, Xj) = b^2 / (1+b^2) = rho
  z=rnorm(n)
  if(abs(rho)<1){
    beta=sqrt(rho/(1-rho))
    x0=matrix(rnorm(n*p),ncol=p)
    A = matrix(z, nrow=n, ncol=p, byrow=F)
    x= beta * A + x0
  }
  if(abs(rho)==1){ x=matrix(z,nrow=n,ncol=p,byrow=F)}
  
  return(x)
}

sample.data <- function(dim.n, dim.p, rho=0.0, snr=1) {
  # Samples the dataset according to Friedman et. al.
  #
  # 1. sample covariates
  X = genx2(dim.n, dim.p, rho)
  # 2. ground truth params.
  theta = ((-1)^(1:dim.p))*exp(-2*((1:dim.p)-1)/20)
  
  f= X %*% theta
  e = rnorm(dim.n)
  k= sqrt(var(f)/(snr*var(e)))
  y=f+k*e
  return(list(Y=y, X=X, theta=theta))
}

dist <- function(x, y) {
  if(length(x) != length(y))
    stop("MSE should compare vectors of same length")
  sqrt(mean((x-y)^2))
}

# Main function to run this experiment.
run.timing <- function(dim.n, dim.p,
                    rho.values=c(0.0, 0.1, 0.2, 0.5, 0.9, 0.95),
                    type="naive",
                    nreps=3, 
                    verbose=F) {
  ## Runs glmnet() for various param values.
  ##
  niters = 0
  cols = c("rho", "rep", "time")
  timings = matrix(nrow=0, ncol=length(cols))
  colnames(timings) <- cols
  rownames(timings) = NULL
  total.iters = nreps * length(rho.values)
  
  pb = txtProgressBar(style=3)
  
  seeds=sample(1:1e9, size=total.iters)
  for(i in 1:nreps) {
    for(rho in rho.values) {
      niters = niters + 1
      set.seed(seeds[niters])
      # 1. (For every repetition) Sample the dataset
      dataset = sample.data(dim.n=dim.n, dim.p=dim.p, rho=rho, snr=3.0)
      true.theta = dataset$theta
      x = dataset$X
      y = dataset$Y
      stopifnot(nrow(x) == dim.n, ncol(x) == dim.p)
      # 1b. Define metrics:
      #   dt = time for the method to finish
      new.dt = 0
      # 2. Run the method.
      if (type == "naive") {
        new.dt = system.time({ fit = glmnet(x, y, alpha=1, standardize=FALSE, type.gaussian="naive")})[1]
      } else if (type == "cov") {
        new.dt = system.time({ fit = glmnet(x, y, alpha=1, standardize=FALSE, type.gaussian="covariance")})[1]
      } else if (type == "lars") {
        use.gram <- ifelse(dim.n > dim.p, TRUE, FALSE)
        new.dt = system.time({ fit = lars(x, y, type="lasso", use.Gram=use.gram) })[1]
      } else if (type == "sgd") {
        new.dt = system.time({ fit = sgd(dataset, plot=F) })[1]
      }
      
      # 3. Tabulate timings
      timings = rbind(timings, c(rho, i, 
                                 new.dt))
      setTxtProgressBar(pb, niters/total.iters)
    }
    
  }
  return(timings)
}

# Main function to run this experiment.
run.mse <- function(dim.n, dim.p,
                       rho.values=c(0.0, 0.1, 0.2, 0.5, 0.9, 0.95),
                       type="naive",
                       nreps=3, 
                       verbose=F) {
  ## Runs glmnet() for various param values.
  ##
  niters = 0
  cols = c("rho", "rep", "mse")
  mse = matrix(nrow=0, ncol=length(cols))
  colnames(mse) <- cols
  rownames(mse) = NULL
  total.iters = nreps * length(rho.values)
  
  pb = txtProgressBar(style=3)
  
  seeds=sample(1:1e9, size=total.iters)
  for(i in 1:nreps) {
    for(rho in rho.values) {
      niters = niters + 1
      set.seed(seeds[niters])
      # 1. (For every repetition) Sample the dataset
      dataset = sample.data(dim.n=dim.n, dim.p=dim.p, rho=rho, snr=3.0)
      true.theta = dataset$theta
      x = dataset$X
      y = dataset$Y
      stopifnot(nrow(x) == dim.n, ncol(x) == dim.p)
      # 1b. Define metrics:
      #   mse = Distance (e.g. RMSE) of the estimates to the ground truth.
      #         (q1, q2, q3) representing the quartiles (since glmnet returns grid of estimates)
      #         Implicit has (x, x, x) i.e., the same value in all places.
      new.mse = NA
      # 2. Run the method.
      if (type == "naive" || type == "cov") {
        fit = glmnet(x, y, alpha=1, standardize=FALSE, type.gaussian="naive")
        new.mse <- median(apply(fit$beta, 2, function(est) dist(est, true.theta)))
      } else if (type == "cov") {
        fit = glmnet(x, y, alpha=1, standardize=FALSE, type.gaussian="covariance")
        new.mse <- median(apply(fit$beta, 2, function(est) dist(est, true.theta)))
      } else if (type == "sgd") { # SGD
        fit <- sgd(dataset, plot=F)
        new.mse <- dist(fit[, dim(fit)[2]], true.theta)
      }
      
      # 3. Tabulate mse
      mse = rbind(mse, c(rho, i,  
                          new.mse))
      setTxtProgressBar(pb, niters/total.iters)
    }
    
  }
  return(mse)
}