#Tutorial 3 R Code Luke Whitehill
#Exercise 3
DFCritVals <- function(samplesize) {
  #Values
  ba0.01 <- matrix(c(-2.56574, -2.2358, -3.627, 0),1,4)
  ba0.05 <- matrix(c(-1.94100, -0.2686, -3.365, 31.223),1,4)
  ba0.10 <- matrix(c(-1.61682, 0.2656, -2.714, 25.364),1,4)
  operator <- matrix(c(1, 1/(samplesize), 1/(samplesize^2), 1/(samplesize^3)),4,1)
  
  #Mechanics
  critstat0.01 <- ba0.01%*%operator
  critstat0.05 <- ba0.05%*%operator
  critstat0.10 <- ba0.10%*%operator
  
  #Output
  critstats <- round(c(critstat0.01, critstat0.05, critstat0.10),4)
  return(critstats)
}