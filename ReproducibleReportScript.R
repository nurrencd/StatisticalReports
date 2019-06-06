# ---- loadData ----
set.seed(17)
algorithm.data <- read.csv("C:/Users/nurrencd/Documents/1-Rose-Hulman/MA/386/Datasets/AlgorithmRuntimes.csv")

# ---- splitByAlgorithm ----
aTimes <- algorithm.data[algorithm.data$Algorithm=="A",]$Runtime
cTimes <- algorithm.data[algorithm.data$Algorithm=="C",]$Runtime

# ---- bootstrap ----
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

# ---- distributionGeneration ----
diff.mean.density <- density(diff.boot.mean)
diff.mean.ci <- quantile(diff.boot.mean, probs=c(0.025,0.975))

diff.sd.density <- density(diff.boot.sd)
diff.sd.ci <- quantile(diff.boot.sd, probs=c(0.025,0.975))

# ---- graphRawData ----
a.dens <- density(aTimes)
c.dens <- density(cTimes)
plot(a.dens, xlab="Runtime in seconds", main="Raw data runtime distributions", ylim=c(0,max(c(unlist(a.dens[2]),unlist(c.dens[2])))))
lines(c.dens, col="red")
legend(145, 0.035, legend=c("Algorithm A","Algorithm C"), col=c("black", "red"), lty=c(1,1))

# ---- tableResults ---- 
meanRuntimes <- matrix(c(diff.mean.ci, diff.sd.ci), ncol=2, byrow = TRUE)
rownames(meanRuntimes) <- c("Mean Difference: ", "SD Difference: ")
colnames(meanRuntimes) <- c("2.5%", "97.5%")
knitr::kable(meanRuntimes)

# ---- graphResultsMean ----
plot(diff.mean.density, 
     main="Mean Algorithm Runtime Difference Distribution", 
     xlab="Runtime difference in seconds")
abline(v=diff.mean.ci, lty=3)

# ---- graphResultsSd ----
plot(diff.sd.density, 
     main="Algorithm Runtime Variance Density Distributions", 
     xlab="Standard deviation of runtime in seconds")
abline(v=diff.sd.ci, lty=3)
