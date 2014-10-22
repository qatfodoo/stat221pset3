source("pset3.R")

# 2 (b)

# Model constants
p <- 100
A.eigen <- seq(0.01, 1, length=p)
D <- diag(A.eigen)
# Random orthogonal matrix to get A with same eigenvalues
Q <- random.orthogonal(p)
A <- t(Q) %*% D %*% Q
# True theta
theta.star <- rep(1, p)

T <- 1e6
alpha <- 1e-2
# params for the learning rate seq.
gamma0 = 1 / (sum(seq(0.01, 1, length.out=p)))
lambda0 = 0.01

# Keep track of training and design matrix for batch estimates
X <- c()
Y <- c()

# Initial guess for iterative methods
theta0 <- rep(0, p)

# Running estimates
theta.sgd <- theta0
theta.asgd <- theta0
theta.impl <- theta0

# Running excess risk
sgd.risk <- c()
asgd.risk <- c()
impl.risk <- c()
batch.risk <- c()

for (t in 1:T) {
  if (t %% 100 == 0) {print(t)}
  # Generate covariates
  x <- rmvnorm(1, mean=rep(0, p), sigma=A)
  # Generate vector of epsilons
  epsilon <- rnorm(1, mean=0, sd=1)
  # Compute observations
  y <- x %*% theta.star + epsilon
  
  # Design and training matrix for batch method
  X <- rbind(X, x)
  Y <- rbind(Y, y)
  
  # Get previous step's estimates
  theta.sgd_prev <- theta.sgd
  theta.asgd_prev <- theta.asgd
  theta.impl_prev <- theta.impl
  # SGD and ASGD
  a_t <- gamma0 / (1 + gamma0 * lambda0 * t)
  theta.sgd <- theta.sgd + as.numeric(a_t * (y - x %*% theta.sgd)) * t(x)
  sgd.risk <- c(sgd.risk, t(theta.sgd - theta.star) %*% A %*% (theta.sgd - theta.star))
  theta.asgd <- (1 - 1 / t) * theta.asgd + 1 / t * theta.sgd
  asgd.risk <- c(asgd.risk, t(theta.asgd - theta.star) %*% A %*% (theta.asgd - theta.star))
  
  # Implicit
  theta.impl <- (diag(p) - as.numeric(a_t / (1 + a_t * x %*% t(x))) * t(x) %*% x) %*%
    (theta.impl + as.numeric(a_t * y) * t(x))
  impl.risk <- c(impl.risk, t(theta.impl - theta.star) %*% A %*% (theta.impl - theta.star))
  
  # Batch
  if (t %% 100 == 0) { # Batch update every 100 iterations
    theta.batch <- pinv(t(X) %*% X) %*% t(X) %*% Y
    batch.risk <- c(batch.risk, t(theta.batch - theta.star) %*% A %*% (theta.batch - theta.star))
  }
  
}

# Store results in output folder
save(list=c("sgd.risk", "asgd.risk", "impl.risk", "batch.risk"), 
     file="./out/2b_risk.Rdata")
