source("glmnet_odys.R")

# 3 (d)

N.list <- c(1e4, 1e4, 1e3, 1e2, 1e1, 1e7)
p.list <- c(1e2, 1e3, 1e3, 1e2, 1e3, 1e1)
m <- length(N.list)
rho <- c(0.00, 0.10, 0.20, 0.50, 0.90, 0.95)
n.cor <- length(rho)

if (Sys.getenv("SLURM_JOB_ID") != "") { # Divide computation per tasks
  
  job.id <- as.numeric(Sys.getenv("SLURM_JOB_ID"))
  print(paste("Job id", job.id, sep=": "))
  task.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  print(paste("Task id", task.id, sep=": "))
  id <- task.id %% 6 + 1 # Parameter handled by task
  
  N <- N.list[id]
  p <- p.list[id]
  
  comp <- list(N=0, p=0, timing=data.frame(naive=numeric(n.cor),
                                           cov=numeric(n.cor),
                                           sgd=numeric(n.cor)),
                                 mse=data.frame(naive=numeric(n.cor),
                                                cov=numeric(n.cor),
                                                sgd=numeric(n.cor)))

  comp$N <- N
  comp$p <- p
  
  for (t in c("naive", "cov", "sgd")) {
    print(t)
    comp.res <- run.comp(N, p, type=t)
    for (r in 1:length(rho)) {
      comp$timing[t][r, 1] <- mean(comp.res[which(comp.res[, 1]== rho[r]), 3])
      comp$mse[t][r, 1] <- mean(comp.res[which(comp.res[, 1]== rho[r]), 4])
    }
  }
  
  # Store results in output folder
  save(comp, 
       file=paste("./out/comp_", id, ".Rdata", sep=""))
}