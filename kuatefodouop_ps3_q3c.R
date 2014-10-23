source("glmnet.R")

# 3(b)

# Timing for SGD

N.list <- c(1000, 5000, 100, 100, 100, 100)
p.list <- c(100, 100, 1000, 5000, 20000, 50000)
m <- length(N.list)
rho <- c(0.00, 0.10, 0.20, 0.50, 0.90, 0.95)
n.cor <- length(rho)

chrono.list <- replicate(m, list(N=0, p=0, timing=data.frame(sgd=numeric(n.cor))),
                         simplify=F)

# Run timing for set of N,p and type of models
for (i in 1:m) {
  print(i)
  N <- N.list[i]
  p <- p.list[i]
  chrono.list[[i]]$N <- N
  chrono.list[[i]]$p <- p
  
  timing <- run.timing(N, p, type="sgd")
  for (r in 1:length(rho)) {
    chrono.list[[i]]$timing["sgd"][r, 1] <- mean(timing[which(timing[, 1]== rho[r]), 3])
  }
}

save(chrono.list, file="./out/sgd_timing.RData")

# MSE comparison SGD - glmnet

mse.list <- replicate(m, list(N=0, p=0, mse=data.frame(naive=numeric(n.cor),
                                                          cov=numeric(n.cor),
                                                          sgd=numeric(n.cor))),
                      simplify=F)

# Run MSE for set of N,p and type of models
for (i in 1:m) {
  print(i)
  N <- N.list[i]
  p <- p.list[i]
  chrono.list[[i]]$N <- N
  chrono.list[[i]]$p <- p
  
  for (t in c("naive", "cov", "sgd")) {
    print(t)
    mse <- run.mse(N, p, type=t)
    for (r in 1:length(rho)) {
      mse.list[[i]]$mse[t][r, 1] <- mean(mse[which(mse[, 1]== rho[r]), 3])
    }
  }
}

save(mse.list, file="./out/sgd_mse.RData")