#Tutorial 3 R Code Luke Whitehill

#Exercise 3

DFCritVals <- function(samplesize) {
  #Estimated Valuees
  ba0.01 <- c(-2.56574, -2.2358, -3.627, 0) 
  ba0.05 <- c(-1.94100, -0.2686, -3.365, 31.223)
  ba0.10 <- c(-1.61682, 0.2656, -2.714, 25.364)
  #Mechanics
  critstat0.01 <- ba0.01[1]+ba0.01[2]/samplesize+
    ba0.01[3]/samplesize^2+ba0.01[4]/samplesize^3
  critstat0.05 <- ba0.05[1]+ba0.05[2]/samplesize+
    ba0.05[3]/samplesize^2+ba0.05[4]/samplesize^3
  critstat0.10 <- ba0.10[1]+ba0.10[2]/samplesize+
    ba0.10[3]/samplesize^2+ba0.10[4]/samplesize^3
  critstats <- round(c(critstat0.01, critstat0.05, critstat0.10),4)
  #Output
  return(critstats)
}


#Alternative 3 (More efficient)

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

#Exercise 4:
T <- 500

data1<-read.xlsx("Tutorial 3.xlsx", "Exercise 4", colIndex = 2)
data2<-read.xlsx("Tutorial 3.xlsx", "Exercise 4", colIndex = 3)
data3<-read.xlsx("Tutorial 3.xlsx", "Exercise 4", colIndex = 4)
data4<-read.xlsx("Tutorial 3.xlsx", "Exercise 4", colIndex = 5)
data5<-read.xlsx("Tutorial 3.xlsx", "Exercise 4", colIndex = 6)

x1<-ts(data = data1, start = 1, end = 500, frequency = 1)
x2<-ts(data = data2, start = 1, end = 500, frequency = 1)
x3<-ts(data = data3, start = 1, end = 500, frequency = 1)
x4<-ts(data = data4, start = 1, end = 500, frequency = 1)
x5<-ts(data = data5, start = 1, end = 500, frequency = 1)


summary(ur.df(x1, type="none", lags=0))
dfreg <- summary(lm(diff(x1)~0+x1[1:T-1]))
dfstat <- dfreg$coefficients[1]/dfreg$coefficients[2]
print(paste0("theta: ", round(dfreg$coefficients[1],4)))
print(paste0("StdErr(theta): ", round(dfreg$coefficients[2],4)))
print(paste0("Dickey -Fuller test statistic: ", round(dfstat ,4)))

summary(ur.df(x2, type="none", lags=0))
dfreg <- summary(lm(diff(x2)~0+x2[1:T-1]))
dfstat <- dfreg$coefficients[1]/dfreg$coefficients[2]
print(paste0("theta: ", round(dfreg$coefficients[1],4)))
print(paste0("StdErr(theta): ", round(dfreg$coefficients[2],4)))
print(paste0("Dickey -Fuller test statistic: ", round(dfstat ,4)))

summary(ur.df(x3, type="none", lags=0))
dfreg <- summary(lm(diff(x3)~0+x3[1:T-1]))
dfstat <- dfreg$coefficients[1]/dfreg$coefficients[2]
print(paste0("theta: ", round(dfreg$coefficients[1],4)))
print(paste0("StdErr(theta): ", round(dfreg$coefficients[2],4)))
print(paste0("Dickey -Fuller test statistic: ", round(dfstat ,4)))

summary(ur.df(x4, type="none", lags=0))
dfreg <- summary(lm(diff(x4)~0+x4[1:T-1]))
dfstat <- dfreg$coefficients[1]/dfreg$coefficients[2]
print(paste0("theta: ", round(dfreg$coefficients[1],4)))
print(paste0("StdErr(theta): ", round(dfreg$coefficients[2],4)))
print(paste0("Dickey -Fuller test statistic: ", round(dfstat ,4)))

summary(ur.df(x5, type="none", lags=0))
dfreg <- summary(lm(diff(x5)~0+x5[1:T-1]))
dfstat <- dfreg$coefficients[1]/dfreg$coefficients[2]
print(paste0("theta: ", round(dfreg$coefficients[1],4)))
print(paste0("StdErr(theta): ", round(dfreg$coefficients[2],4)))
print(paste0("Dickey -Fuller test statistic: ", round(dfstat ,4)))