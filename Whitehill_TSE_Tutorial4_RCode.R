# Tutorial 4 R Code Luke Whitehill 13215948

#Exercise 1

library(xlsx)
data<-read.xlsx("Tutorial 4.xlsx", "Exercise 1")
df <- data.frame(data)

layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
plot(df$Date, df$SPY,  type ="l", lwd = '0.5', main = 'SPY Time Series', 
     xlab = 'Date', ylab = 'SPY')
plot(df$Date, df$SP500, type ="l", lwd = '0.5' , main = 'SP500 Time Series',
     xlab = 'Date', ylab = 'SP500')
plot(df$SP500, df$SPY, main = "Scatter Plot of SPY and SP500", 
     xlab = "SP500", ylab = "SPY")

#Exercise 2
library(urca)
summary(ur.df(df[,2], type = "none", lags = 0))
summary(ur.df(df[,3], type = "none", lags = 0))

#Exercise 3
SP500subsample <- df[2:4026,2]
SPYsubsample <- df[2:4026,3]
eg.reg1 <- lm(SPYsubsample ~ SP500subsample) 
summary(eg.reg1)

#Exercise 4
z <- eg.reg1$residuals
summary(ur.df(z, type = "none", lags = 0))

#Exercise 5
z_lag <- z[-c(4025, 4026)]
diffSP500 <- diff(df[1:4026,2])
diffSPY <- diff(df[1:4026,3])
eg.dat <- data.frame(embed(cbind(diffSP500, diffSPY), 2))
colnames(eg.dat) <- c("diffSP500", "diffSPY", "SP500_lag", "SPY_lag")

eg.reg2 <- lm(diffSPY ~ SP500_lag + SPY_lag + z_lag, data=eg.dat)
summary(eg.reg2)

#Exercise 6
Datesubsample <- df[2:4026,1]
esttheta <- summary(eg.reg1)$coefficients[2,1]
SPYminusthetaSP500 <- SPYsubsample - esttheta*SP500subsample
layout(matrix(c(1), 1, 1, byrow = TRUE))
plot(Datesubsample, SPYminusthetaSP500, xlab = "Time", 
     ylab = "y_t-theta_hat*x_t", type = 'l',
     main = 'Time Series of the Residuals')

#Exercise 7
SP500subsample2 <- df[4027:4943, 2]
SPYsubsample2 <- df[4027:4943, 3]
Datesubsample2 <- df[4027:4943, 1]
SPYbar <- esttheta*SP500subsample2
layout(matrix(c(1), 1, 1, byrow = TRUE))
plot(Datesubsample2, SPYbar, 
     col = 'red', xlab = "Time", ylab ="SPY", type = 'l', 
     main = "Observed SPY vs Forecasted SPY ")
par(new = T)
plot(Datesubsample2, SPYsubsample2, col = 'royalblue4', xlab = "", ylab = "", axes = F, type = 'l')
par(new = F)
legend("topleft", c("SPY", "Forecasted SPY"),
       col = c("royalblue4", "red"), lty = c(1, 2))
