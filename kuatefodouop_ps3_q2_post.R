
# Plotting for question 2
rm(list=ls())
load("./out/2a_risk_exp5.Rdata")

# Plot risks for 2(a)

plot(1 / log(10) * log(1e2:length(sgd.risk)), sgd.risk[1e2:length(sgd.risk)], type="l", col="red", lwd=1,
     xlab="training size t", ylab="excess risk")
lines(1 / log(10) * log(1e2:length(asgd.risk)), asgd.risk[1e2:length(asgd.risk)], col="blue", lwd = 2)
lines(1 / log(10) * log(1e2:length(impl.risk)), impl.risk[1e2:length(impl.risk)], col="purple", lwd = 2)
lines(1 / log(10) * log(1e2:length(batch.risk)), batch.risk[1e2:length(batch.risk)], col="green", lwd = 2)
lines(1 / log(10) * log(1e2:length(asgd_bad.risk)), asgd_bad.risk[1e2:length(asgd_bad.risk)], col="black", lwd = 2)
b <- c("SGD", "ASGD", "Implicit", "Batch", "ASGD_BAD")
legend("topright", b , cex=0.8, col=c("red", "blue", "purple", "green", "black"), lwd=2, bty="n")

plot(1 / log(10) * log(1e4:length(sgd.risk)), sgd.risk[1e4:length(sgd.risk)], type="l", col="red", lwd=1,
     xlab="training size t", ylab="excess risk")
lines(1 / log(10) * log(1e4:length(asgd.risk)), asgd.risk[1e4:length(asgd.risk)], col="blue", lwd = 2)
lines(1 / log(10) * log(1e4:length(impl.risk)), impl.risk[1e4:length(impl.risk)], col="purple", lwd = 2)
lines(1 / log(10) * log(1e4:length(batch.risk)), batch.risk[1e4:length(batch.risk)], col="green", lwd = 2)
lines(1 / log(10) * log(1e4:length(asgd_bad.risk)), asgd_bad.risk[1e4:length(asgd_bad.risk)], col="black", lwd = 2)
b <- c("SGD", "ASGD", "Implicit", "Batch", "ASGD_BAD")
legend("topright", b , cex=0.8, col=c("red", "blue", "purple", "green", "black"), lwd=2, bty="n")



# Plot risks for 2(b)
load("./out/2a_risk_exp5.Rdata")

plot(1 / log(10) * log(1e2:length(sgd.risk)), sgd.risk[1e2:length(sgd.risk)], type="l", col="red", lwd=1,
     xlab="training size t", ylab="excess risk")
lines(1 / log(10) * log(1e2:length(asgd.risk)), asgd.risk[1e2:length(asgd.risk)], col="blue", lwd = 2)
lines(1 / log(10) * log(1e2:length(impl.risk)), impl.risk[1e2:length(impl.risk)], col="purple", lwd = 2)
lines(1 / log(10) * log(1e2:length(batch.risk)), batch.risk[1e2:length(batch.risk)], col="green", lwd = 2)
b <- c("SGD", "ASGD", "Implicit", "Batch")
legend("topright", b , cex=0.8, col=c("red", "blue", "purple", "green"), lwd=2, bty="n")
