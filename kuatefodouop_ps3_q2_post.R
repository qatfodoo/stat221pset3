
# Plotting for question 2

# Plot risks

plot(4000:length(sgd.risk), asgd_bad.risk[4000:length(sgd.risk)], type="l", col="red", lwd=1,
     xlab="training size t", ylab="excess risk")
lines(4000:length(asgd.risk), asgd.risk[4000:length(asgd.risk)], col="blue", lwd = 2)
lines(4000:length(impl.risk), impl.risk[4000:length(impl.risk)], col="purple", lwd = 2)
lines(4000:length(batch.risk), batch.risk[4000:length(batch.risk)], col="green", lwd = 2)
lines(4000:length(asgd_bad.risk), asgd_bad.risk[4000:length(asgd_bad.risk)], col="black", lwd = 2)
b <- c("SGD", "ASGD", "Implicit", "Batch", "ASGD_BAD")
legend("topright", b , cex=0.8, col=c("red", "blue", "purple", "green", "black"), lwd=2, bty="n")




# Plot risks

plot(40:length(sgd.risk), asgd_bad.risk[40:length(sgd.risk)], type="l", col="red", lwd=1,
     xlab="training size t", ylab="excess risk")
lines(40:length(asgd.risk), asgd.risk[40:length(asgd.risk)], col="blue", lwd = 2)
lines(40:length(impl.risk), impl.risk[40:length(impl.risk)], col="purple", lwd = 2)
lines(40:length(batch.risk), batch.risk[40:length(batch.risk)], col="green", lwd = 2)
b <- c("SGD", "ASGD", "Implicit", "Batch")
legend("topright", b , cex=0.8, col=c("red", "blue", "purple", "green"), lwd=2, bty="n")