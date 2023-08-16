# Simulation
simGBM <- function(S0, mu, sigma, T, numSteps, numRepl){
  dt <- T/numSteps
  muT <- (mu-sigma^2/2)*dt
  sigmaT <- sqrt(dt)*sigma
  pathMatrix <- matrix(nrow = numRepl,ncol = numSteps+1)
  pathMatrix[,1] <- S0
  for (i in 1:numRepl){
    for (j in 2:(numSteps+1)){
      pathMatrix[i,j] <- 
        pathMatrix[i,j-1]*exp(rnorm(1,muT,sigmaT))
    }
  }
  return(pathMatrix)
}

#Plot for Diffs
set.seed(12345)
S0 <- 40
mu <- 0.25
sigma1 <- 0.3
sigma2 <- 0.5
T <- 1
numSteps <- 50
numRepl <- 10
Paths1 <- simGBM(S0,mu,sigma1,T,numSteps, numRepl)
Paths2 <- simGBM(S0, mu, sigma2, T, numSteps, numRepl)
maxval <- max(Paths1, Paths2)
minval <- min(Paths1, Paths2)
labx <- bquote(italic(t))
laby <- bquote(italic(S(t)))
par(mfrow = c(1,2))
plot(Paths1[1,], type='l', ylab=laby, xlab=labx,
     main='Vol. 30%', ylim = c(minval, maxval))
for (k in 2:10) lines(Paths1[k,])
plot(Paths2[1,], type='l', ylab=laby, xlab=labx,
     main="vol. 50%", ylim=c(minval,maxval))
for (k in 2:10) lines(Paths2[k,])
par(mfrow = c(1,1))