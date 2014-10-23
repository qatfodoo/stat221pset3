source("pset3.R")

# 2 (c)

N <- 10000
p <- 100

methods <- c("SGD", "Implicit", "ASGD")
amin <- 0.01
amax <- 100
seq <- seq(amin, amax, length.out=10)
m <- 1000

run.method <- function(data, method, alpha) {
  if (method == "SGD") {
    return(run.sgd(data, alpha))
  } else if (method == "Implicit") {
    return(run.implicit(data, alpha)) 
  } else if (method == "ASGD") {
    return(run.asgd(data, alpha))
  }
}

if (Sys.getenv("SLURM_JOB_ID") != "") { # Divide computation per tasks
  
  job.id <- as.numeric(Sys.getenv("SLURM_JOB_ID"))
  print(paste("Job id", job.id, sep=": "))
  task.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  print(paste("Task id", task.id, sep=": "))
  meth.id <- task.id %% 3 + 1 # Method handled by task
  seq.id <- task.id %% 10 + 1 # Alpha handled by task
  
  gc() # garbage collection
  t1.sim <- as.numeric(Sys.time())
  
  meth <- methods[meth.id]
  alpha <- seq[seq.id]
  
  D <- replicate(m, list(method="SGD", alpha=alpha, rep=0,
                         theta=matrix(0, nrow=p, ncol=1)), simplify=F)
  
  for (rep in 1:m) {
    y = sample.data(N, p)
    theta = run.method(data=y, method=meth, alpha=alpha)
    D[[rep]]$rep <- m
    D[[rep]]$theta <- theta
  }
  
  t2.sim <- as.numeric(Sys.time())
  dt.sim <- (t2.sim - t1.sim) / 60 # dt in min
  print(paste(paste("Simulation elapsed time (min), task", task.id, sep=" "), dt.sim, sep=": "))
  
  gc() # required memory
  
  # Store results in output folder
  save(D, 
       file=paste("./out/theta_method_", meth, "_alpha_", alpha, ".Rdata", sep=""))
  
}