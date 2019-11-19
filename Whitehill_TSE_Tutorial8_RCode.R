# Tutorial 8 R Code Luke Whitehill 13215948

library(xlsx)
library(urca)
data <- read.xlsx('Tutorial 8.xlsx','Intel Log-Returns')

#Exercise 1:
layout(matrix(c(1,2), ncol = 1))
acf1 <- acf(data$Log.Return, main = "ACF log-returns", lwd = 2, col = "blue", ci.col = "red")
acf2 <- acf(abs(data$Log.Return), main = "ACF absolute log-returns", lwd = 2, col = "blue", ci.col = "red")

#Exercise 2:
b1 <- Box.test(data$Log.Return,lag = 12, type = 'Ljung-Box')
x2crit1 <- qchisq(0.95, b1$parameter)
b2 <- Box.test(abs(data$Log.Return), lag = 12, type = 'Ljung-Box')
x2crit2 <- qchisq(0.95, b2$parameter)


#Exercise 3:
ttest <- t.test(data$Log.Return)
qt(0.95, ttest$parameter)

#Exercise 4:
logrres <- data$Log.Return - ttest$estimate
b3 <- Box.test((logrres^2), lag = 12, type = 'Ljung-Box')
x2crit3 <- qchisq(0.95, b3$parameter)

#Exercise 5:
source("archTest.R")
arct <- archTest(logrres, m = 12)
f1 <- qf(0.95, df1 = arct$fstatistic[2], df2 = arct$fstatistic[3])

#Exercise 6:
layout(matrix(c(1,2), ncol = 1))
acf3 <- acf(logrres^2, main = "ACF estimated squared residuals of log-returns",
            lwd = 2, col = "blue", ci.col = "red")
pacf1 <- pacf(logrres^2, main = "PACF estimated squared residuals of log-returns",
              lwd = 2, col = 'blue', ci.col = "red")

#Exercise 7:
library(fGarch)

mm1 <- garchFit(~1+garch(3,0), data = data$Log.Return, trace = FALSE)
summary(mm1)
#From exercise 3 we get the t critical values for the Log.Returns. 
#If you want each exercise to be independent, just uncomment the following:....
# ttest <- t.test(data$Log.Return)
# qt(0.95, ttest$parameter)

#Exercise 8:
mm2 <- garchFit(~1+garch(1,0), data = data$Log.Return, trace = FALSE)
summary(mm2)
#From exercise 3 we get the t critical values for the Log.Returns. 
#If you want each exercise to be independent, just uncomment the following:....
# ttest <- t.test(data$Log.Return)
# qt(0.95, ttest$parameter)
