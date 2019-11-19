#Tutorial 3 R Code Luke Whitehill
#Exercise 4:
library(urca)
library(xlsx)
library(xlsxjars)
library(rJava)

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
summary(ur.df(x2, type="none", lags=0))
summary(ur.df(x3, type="none", lags=0))
summary(ur.df(x4, type="none", lags=0))
summary(ur.df(x5, type="none", lags=0))

layout(matrix(c(1,1,2,2), ncol = 1))
acf2 <- acf(x1, main="ACF1", ylab="", ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")
acf3 <- acf(x2, main="ACF2", ylab="", ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")
acf4 <- acf(x3, main="ACF3", ylab="", ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")
acf5 <- acf(x4, main="ACF4", ylab="", ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")
acf6 <- acf(x5, main="ACF5", ylab="", ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")


plot(x1, main = "x1_t Time Series")
plot(x2, main = "x2_t Time Series")
plot(x3, main = "x3_t Time Series")
plot(x4, main = "x4_t Time Series")
plot(x5, main = "x5_t Time Series")