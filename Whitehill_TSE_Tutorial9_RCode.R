# Tutorial 9 R Code Luke Whitehill 13215948

library(xlsx)
data <- read.xlsx("Tutorial 9.xlsx", "SPY")
df <- data.frame(data)

#Exercise 1:
t1 <- t.test(df$SPY)
qt(0.95, t1$parameter)

#Exercise 2:
layout(matrix(c(1,2), ncol = 1))
acf1 <- acf(df$SPY, main = "ACF of log-returns", 
            lwd = 2, col = "mediumvioletred", ci.col = "purple3")
acf2 <- acf((df$SPY)^2, main = "ACF of squared log-returns", 
            lwd = 2, col = "mediumvioletred", ci.col = "purple3")

#Exercise 3:
b1 <- Box.test((df$SPY)^2, lag = 20, type = "Ljung-Box")
x2crit1 <- qchisq(0.95, b1$parameter)

#Exercise 4:
library(fGarch)

mm1 <- garchFit(~1+garch(2, 1), data=df$SPY , trace=FALSE)
summary(mm1)


#Exercise 5:
resi <- residuals(mm1, standardize = TRUE)
b2 <- Box.test(resi, lag = 20, type = "Ljung-Box")
x2crit2 <- qchisq(0.95, b2$parameter)

#Exercise 6:
v <- volatility(mm1)
date = df$Date
layout(matrix(c(1,2), ncol = 1))
plot(df$Date, v, type = 'l', main = 'GARCH(2,1) Volatility Series',
     col= "mediumvioletred", xlab = 'Date', ylab = 'Volatility')
plot(df$Date, resi, type = 'l', main = "GARCH(2,1) Standardised Residuals",
     col= "mediumvioletred", xlab = "Date", ylab = "Standardised Residuals")

#Exercixse 7 
upp <- mean(df$SPY) + 2*v
low <- mean(df$SPY) - 2*v
layout(matrix(c(1,1), ncol = 1))
plot(date,df$SPY, type="l", col="mediumvioletred", lwd = 0.5,
     main = "SPY Log-Returns and 2 SDev Confidence Intervals")
lines(date,upp, lty = 2, lwd = 1, col="purple3")
lines(date,low, lty = 2, lwd = 1,  col="purple3")