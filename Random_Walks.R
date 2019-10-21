#Random Walk in 2 dimensions

#choose direction left, up, right, down
ChooseDir <- function(dir){
  x <- switch(dir, c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
  return(x)
}

#walk randomly steps in 2d lattice and plot the result
PlotRandomWalk <- function(steps){
  currentPos <- array(c(0,0), dim=c(2, steps))
  for (i in c(2:steps-1)) {
    x <- sample(1:4, 1)
    currentPos[,i+1] <- (currentPos[,i] + ChooseDir(x))
  }
  plot(x=currentPos[1,], y=currentPos[2,], col="blue", pch=21, main="Random Walk in 2D",xlab="x", ylab="y","n")    
  lines(x=currentPos[1,], y=currentPos[2,], col="red")
}

#random walk in 2d returning the last position (x,y)
RandomWalk <- function(steps){
  currentPos <- c(0,0)
  for (i in c(1:steps)) {
    x <- sample(1:4, 1)
    currentPos <- (currentPos + ChooseDir(x))
  }
  return(currentPos)
}

#calculates the mean end-to-end length <R>
EndToEndLength <-function(steps, precision){
  sum <- 0
  for (i in c(1:precision)) {
    v <- RandomWalk(steps)
    sum <- (sum + abs(v[1]) + abs(v[2]))
    #print(sum)
  }
  sum <- sum/precision
  return(sum)
}

#plot end-to-end length against the number of steps 
PlotEndToEndLength <- function(steps, precision){
  s <- c(1:steps)
  for(i in c(1:steps)){
    s[i] <- EndToEndLength(i, precision)
  }
  plot(x=c(1:steps), y=s, ylim=c(0,30), col="green", pch=21, main="", xlab = "N", ylab ="<R>")
}

#Execution
N = 200
precision = 100
#RandomWalk(N)
PlotEndToEndLength(N, precision)