source("pset3.R")
library(mvtnorm)

## Question 2

# 2 (a)

if (Sys.getenv("SLURM_JOB_ID") != "") { # Divide computation per tasks
  
  job.id <- as.numeric(Sys.getenv("SLURM_JOB_ID"))
  print(paste("Job id", job.id, sep=": "))
  task.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  print(paste("Task id", task.id, sep=": "))
  exp.id <- task.id %% 3 + 1 # Parameter handled by task
  
  # Model constants
  p <- 100
  A.eigen <- c(c(1, 1, 1), rep(0.02, p - 3))
  D <- diag(A.eigen)
  # Random orthogonal matrix to get A with same eigenvalues
  Q <- random.orthogonal(p)
  A <- Q %*% D %*% t(Q)
  # True theta
  theta.star = rep(0, p)
  
  # Initial guess for iterative methods
  theta0 <- runif(p, 0, 1)
  
  # Running estimates
  theta.sgd <- theta0
  theta.sgd_asgd <- theta0 # current estimate before ASGD averaging
  theta.asgd <- theta0
  theta.sgd_bad <- theta0
  theta.asgd_bad <- theta0
  theta.impl <- theta0
  theta.batch <- theta0 # Will not be taken into consideration in the sum
  
  # Running excess risk
  sgd.risk <- c()
  asgd.risk <- c()
  sgd_bad.risk <- c()
  asgd_bad.risk <- c()
  impl.risk <- c()
  batch.risk <- c()
  
  # SGD parameters
  exp <- c(4, 5, 6)
  print(paste("exp id:", exp.id))
  T <- 10^exp[exp.id]
  print(T)
  alpha <- 1e-2
  
  for (t in 1:T) {
    if (t %% 1000 == 0) {print(t)}
    # Generate covariates
    x <- rmvnorm(1, mean=rep(0, p), sigma=diag(p)) # Note: change for A?
    
    # SGD and ASGD
    a_t.sgd <- (1 + 0.02 * t)^(- 1)
    theta.sgd <- theta.sgd + a_t.sgd * A %*% (t(x) - theta.sgd)
    sgd.risk <- c(sgd.risk, t(theta.sgd) %*% A %*% theta.sgd)
    a_t.asgd <- (1 + 0.02 * t)^(- 2 / 3)
    theta.sgd_asgd <- theta.sgd_asgd + a_t.asgd * A %*% (t(x) - theta.sgd_asgd)
    theta.asgd <- (1 - 1 / t) * theta.asgd + 1 / t * theta.sgd_asgd
    asgd.risk <- c(asgd.risk, t(theta.asgd) %*% A %*% theta.asgd)
    # SGD and ASGD bad
    a_t.bad <- (1 + t)^(- 1 / 2)
    theta.sgd_bad <- theta.sgd_bad + a_t.bad * A %*% (t(x) - theta.sgd_bad)
    sgd_bad.risk <- c(sgd_bad.risk, t(theta.sgd_bad) %*% A %*% theta.sgd_bad)
    theta.asgd_bad <- (1 - 1 / t) * theta.asgd_bad + 1 / t * theta.sgd_bad
    asgd_bad.risk <- c(asgd_bad.risk, t(theta.asgd_bad) %*% A %*% theta.asgd_bad)
    # Implicit
    theta.impl <- solve(diag(p) + a_t.sgd * A) %*% (theta.impl + a_t.sgd * A %*% t(x))
    impl.risk <- c(impl.risk, t(theta.impl) %*% A %*% theta.impl)
    
    # Batch
    theta.batch <- (t - 1) / t * theta.batch + 1 / t * t(x)
    batch.risk <- c(batch.risk, t(theta.batch) %*% A %*% theta.batch)
  }
  
  # Store results in output folder
  save(list=c("sgd.risk", "asgd.risk", "impl.risk", "batch.risk"), 
       file=paste("./out/2a_risk_exp", exp[exp.id], ".Rdata", sep=""))

}