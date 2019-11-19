# Tutorial 7 R Code Luke Whitehill 13215948

#Exercise 1
library(xlsx)
library(urca)
data <- read.xlsx('Tutorial 7.xlsx', 'Series')
tsx <- ts(data[,2]) #Random walk w/ drift
tsy <- ts(data[,3]) #Random walk w/ pulse break
tsz <- ts(data[,4]) #Random walk w/ drift and change in drift


plot(tsz, col = 'green', ylab = "", ylim = c(0,180))
abline(v=250, lty = "dotted", col = 'gray26')
par(new = T)
plot(tsy, col = 'red', ylab = "", ylim = c(0,180))
par(new = T)
plot(tsx, col = 'blue', ylab = 'Series Value', ylim = c(0,180), main = "3 Time Series'")
par(new = F)
legend("topleft", c("Random walk w/ drift", "random walk w/ pulse break", 
                    "random walk w/ drift & ∆ drift", "Break time"),
       col = c("blue", "red", 'green', 'gray26'), lty = c(1, 1, 1, 3))

#Exercise 2

summ1<- summary(ur.za(tsx, model = 'trend', lag = NULL))
summ2 <- summary(ur.za(tsy, model = 'intercept', lag= NULL))
summ3 <- summary(ur.za(tsz, model = 'trend', lag = NULL)) 
