# Portfolio 6. Simulation.

#Simulating a Lucky Charms box game.
# Address 3 questions:
# How often does player 3 win?
# How many rounds does a game last on average?
# How likely is it that at least one of the players lands on a foward bridge?
set.seed(17)

m <- 100000
positions <- c(1,1,1,1)
hasTurn <- c(1,1,1,1) #whether or not a player lost their turn
rolls <- c(1,2,3,4,5,6,7,8) # number of charm choices
board <- c(1,2,3,4,5,6,7,8,
           1,2,3,4,5,6,7,8,
           1,2,3,4,5,6,1,8,
           7,2,3,4,5,6,7,8,
           1,2,3,4,5,6,7,8,
           1,2,5,4,5,6,7,8,1)

tiles <- length(board)

rounds <- rep(0, m)
ladderTaken <- rep(0, m)
winners <- rep(0, m)


takeTurn <- function(p, i){
  turnRolls <- sample(rolls, 4, replace=TRUE)
  
  takenBridgeThisTurn <- 0
  
  spacesToMove <- apply(matrix(c(p, turnRolls), ncol=2), 1, FUN=distanceToMove)
  spacesToMove <- spacesToMove * hasTurn
  p <- p + spacesToMove
  hasTurn <- c(1,1,1,1)
  ## account for slides and lost turns
  cloverLadder <- (p == 4)
  p <- p + 4 * cloverLadder
  
  hourGlassChute <- (p == 15)
  p <- p + -3 * hourGlassChute
  
  rainbowLadder <- (p == 25)
  p <- p + 6 * rainbowLadder
  
  shootingStarLadder <-  (p == 42)
  p <- p + 3 * shootingStarLadder
  
  heartChute <- (p == 49)
  p <- p + -3 * heartChute
  hasTurn <- hasTurn * (1-heartChute)
  
  # calculate if bridges have been taken
  takenBridgeThisTurn <- sum(cloverLadder + rainbowLadder + shootingStarLadder)
  return(c(p, takenBridgeThisTurn))
}

distanceToMove <- function(x){
  which(board[x[1]+1:tiles]==x[2])[1]
}


for (i in 1:m){
  positions <- c(1,1,1,1)
  roundCount <- 0
  
  while (TRUE){
    results <- takeTurn(positions, i)
    positions <- results[1:4]
    
    ladderTaken[i] <- sum(c(ladderTaken[i], results[5]), na.rm = TRUE)
    roundCount <- roundCount + 1
    
    if (length(which(is.na(positions)))>0){
      winners[i] <- which(is.na(positions))[1]
      rounds[i] <- roundCount
      break
    }
  }
}