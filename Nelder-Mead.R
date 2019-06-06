GammaMLE <- function(x, maxIter=500){
  #grab initial alpha and beta as estimates
  xBar <- mean(x)
  beta <- xBar/((sum(x^2)/length(x))-xBar^2)
  alpha <- xBar* beta
  print(c(alpha, beta))
  #t for theta
  t1 <- log(c(alpha, beta))
  t2 <- t1 + c(t1[1], 0)
  t3 <- t1 + c(0, t1[2])
  # theta[k, i] indexes to 
  #setup theta 3D array, [alpha/beta, i, k]
  tMatrix <- array(rep(0, 2 * 3 * maxIter), c(2,3,maxIter))
  tMatrix[,,1] <- c(t1, t2, t3)
  
  
  lt1 <- likelihood(x, tMatrix[,,1])
  lt2 <- likelihood(x, tMatrix[,,2])
  lt3 <- likelihood(x, tMatrix[,,3])
  
  # likelihood(theta) matrix
  ltMatrix <- array(rep(0, 3 * maxIter), c(3,maxIter))
  
  ltMatrix[,1] <- c(lt1, lt2, lt3)
  currentIter <- 1
  
  while (currentIter < maxIter){
    # Terminating Check
    if (sd(ltMatrix[,currentIter]) < 10^-8){
      break
    }
    # Ordering Step
    t <- which.max(ltMatrix[,currentIter])
    r <- which.min(ltMatrix[,currentIter])
    # resultant median is sum of indices minus prescribed indices
    s <- 6 - r - t
    
    
    #assignment of vectors for later use
    tVec <- tMatrix[,t,currentIter]
    rVec <- tMatrix[,r,currentIter]
    sVec <- tMatrix[,s,currentIter]
    # Centroid Step
    cVec <- (rVec + sVec)/2
    # Reflcetion Step
    hVec <- cVec + (cVec - tVec)
    # Conditional Check
    lh <- likelihood(x, hVec)
    b1 <- ltMatrix[r,currentIter] < lh
    b2 <- lh < ltMatrix[s, currentIter]
    if (b1 & b2){
      # c(r, s, h)
      tMatrix[,,currentIter + 1] <- c(rVec, sVec, hVec)
    } else if (lh < ltMatrix[r,currentIter]) {
      # Expansion Step
      print("Expanding...")
      iVec <- cVec + 2 * (hVec - cVec)
      print(iVec)
      if (likelihood(x, iVec)< lh){
        #c(r,s,i)
        tMatrix[,,currentIter + 1] <- c(rVec, sVec, iVec)
      } else {
        #c(r,s,h)
        tMatrix[,,currentIter + 1] <- c(rVec, sVec, hVec)
      }
      
    } else if (!b2) {
      # Contraction Step
      print("Contracting...")
      jVec <- cVec + (tVec - cVec)/2
      
      if (likelihood(x, jVec) < ltMatrix[t, currentIter]){
        tMatrix[,,currentIter + 1] <- c(rVec, sVec, jVec)
      } else {
        # Shrink Step
        print("Shrinking")
        tMatrix[,,currentIter + 1] <- c(rVec + (tMatrix[,1,currentIter] - rVec)/2,
                                        rVec + (tMatrix[,2,currentIter] - rVec)/2,
                                        rVec + (tMatrix[,3,currentIter] - rVec)/2)
      }
    }
    
    #
    currentIter <- currentIter + 1
    print(tMatrix[,,currentIter])
    ltMatrix[,currentIter] <- c(likelihood(x, tMatrix[,1,currentIter]), 
                               likelihood(x, tMatrix[,2,currentIter]),
                               likelihood(x, tMatrix[,3,currentIter]))
  }
  print(currentIter)
  finalIndex <- which.min(ltMatrix[,currentIter])
  print(exp(tMatrix[,finalIndex,currentIter]))
  return(exp(tMatrix[,finalIndex,currentIter]))
}

likelihood <- function(x, abVec){
  # abVec is c(alpha, beta), for conveniently calling from the main algorithm

  abVec <- exp(abVec)
  prod <- sum(dgamma(x, abVec[1], scale=abVec[2], log=TRUE))
  return(-1*prod)
}
y <- c(31.00, 2.82, 3.98, 4.02, 9.50, 4.50,
       11.4, 10.71, 6.31, 4.95, 5.64, 5.51,
       13.4, 9.72, 6.47, 10.16, 4.21, 11.60,
       4.75, 6.85, 6.25, 3.42, 11.80, .8,
       3.69, 3.10, 22.22, 7.43, 5.00, 4.58,
       4.46, 8.00, 3.73, 3.50, 6.20, .67)
answer <- GammaMLE(y)
answer[1]/answer[2]