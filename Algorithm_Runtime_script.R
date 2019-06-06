#Portfolio 7 script

set.seed(17)
aTimes <- c(168,99,113,119,125,164,165,165,127,117,128,133)
cTimes <- c(113,127,117,120,120,106,136,132,118,135,126,126)

frame <- data.frame(Times=c(aTimes, cTimes), Group=as.factor(rep(c("A", "C"), each=length(aTimes))))
alpha <- 0.05
m <- 100000

diff.boot.mean <- replicate(m, {
  aSample <- sample(aTimes, length(aTimes), replace=TRUE)
  cSample <- sample(cTimes, length(cTimes), replace=TRUE)
  mean(aSample - cSample)
})

diff.boot.sd <- replicate(m, {
  aSample <- sample(aTimes, length(aTimes), replace=TRUE)
  cSample <- sample(cTimes, length(cTimes), replace=TRUE)
  sd(aSample - cSample)
})

diff.mean.density <- density(diff.boot.mean)
diff.mean.ci <- quantile(diff.boot.mean, probs=c(0.025,0.975))

diff.sd.density <- density(diff.boot.sd)
diff.sd.ci <- quantile(diff.boot.sd, probs=c(0.025,0.975))


boxplot(Times ~ Group, data=frame, main="Algorithm Runtime Comparison", ylab="Runtime in Seconds", xlab="Algorithm")

a.dens <- density(aTimes)
c.dens <- density(cTimes)
plot(density(aTimes), xlab="Runtime in seconds", main="Raw data runtime distributions", ylim=c(0,max(c(unlist(a.dens[2]),unlist(c.dens[2])))))
lines(density(cTimes), col="red")
legend(150, 0.035, legend=c("Algorithm A","Algorithm C"), col=c("black", "red"), lty=c(1,1))

plot(diff.mean.density, 
     main="Mean Algorithm Runtime Difference Distribution", 
     xlab="Runtime difference in seconds")
abline(v=diff.mean.ci, lty=3)

plot(diff.sd.density, 
     main="Algorithm Runtime Variance Density Distributions", 
     xlab="Standard deviation of runtime in seconds")
abline(v=diff.sd.ci, lty=3)

