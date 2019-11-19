#Tutorial 2 R Code Luke Whitehill


#Exercise 1
data1<-read.xlsx("Tutorial 2.xlsx", "Exercise 1")

x<-ts(data = x_t, start = 1, end = 1000, frequency = 1)

layout(matrix(c(1,1,2,2), ncol = 1))
acf3 <- acf(x, main="ACF", ylab="", ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")
pacf3 <- pacf(x, main="PACF", ylab="", ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")
# Because the ACF is decaying, the process is at least an AR(?), the PACF determines the order.
# It appears to be of order 3

#Estimating the candidate AR(3) model, and check its stationarity and the significance of 
#its coefficients
arma30 <- arima(x, order=c(3,0,0))
arma30
rootdf <- data.frame(Re(polyroot(c(1, -arma30$coef[1], -arma30$coef[2], -arma30$coef[3]))),
                    Im(polyroot(c(1, -arma30$coef[1], -arma30$coef[2], -arma30$coef[3]))))
colnames(rootdf)[1] = "real"
colnames(rootdf)[2] = "imag"
modulus <- sqrt(rootdf$real^2+rootdf$imag^2)
rootdf$modulus <- modulus
format(round(rootdf, 4), nsmall =4)

#The roots are > 1 therefore it is a stationary process.

#Testing the residuals of the candidate AR(3) model fro uncorrelatedness and normality
res30 <- residuals(arma30)
Box.test(res30, lag = 20, type = "Ljung-Box")
shapiro.test(res30)
#The results for both tests are not less than 0.05 (which is the alpha). So the null cannot be rejected.
#There is no correlation between the residuals and the residuals are normally distributed.

#Estimating an AR(4) model, and checking its stationarity and the significance of its coefficients. 
arma40 <- arima(x, order=c(4,0,0))
arma40
rootdf <- data.frame(Re(polyroot(c(1, -arma40$coef[1], -arma40$coef[2], -arma40$coef[3], -arma40$coef[4]))),
                     Im(polyroot(c(1, -arma40$coef[1], -arma40$coef[2], -arma40$coef[3], -arma40$coef[4]))))
colnames(rootdf)[1] = "real"
colnames(rootdf)[2] = "imag"
modulus <- sqrt(rootdf$real^2+rootdf$imag^2)
rootdf$modulus <- modulus
format(round(rootdf, 4), nsmall =4)

#AR(4) Model, the coefficients 3 & 4 are insignificant according to the t student statistic

#Estimating an AR(2) model. and checking its stationarity and the significantce of its coefficients. 
arma20 <- arima(x, order=c(2,0,0))
arma20
rootdf <- data.frame(Re(polyroot(c(1, -arma20$coef[1], -arma20$coef[2]))),
                     Im(polyroot(c(1, -arma20$coef[1], -arma20$coef[2]))))
colnames(rootdf)[1] = "real"
colnames(rootdf)[2] = "imag"
modulus <- sqrt(rootdf$real^2+rootdf$imag^2)
rootdf$modulus <- modulus
format(round(rootdf, 4), nsmall =4)
#AR(2) model, both the coefficients are significant according to the t student statistic. 

#Testing the residuals of the AR(2) model
res20 <- residuals(arma20)
Box.test(res30, lag = 20, type = "Ljung-Box")
shapiro.test(res20)
#The residuals pass the test

#Comparing the AR3 and AR2 AIC, AR3 has (2814.81-2811.49) lower AIC. We choose the lower AIC value, the AR3. 
#However we will now test ARMA Models

#Estimate an ARMA(2,1)
arma21 <- arima(x, order=c(2,0,1))
arma21
rootdf <- data.frame(Re(polyroot(c(1, -arma21$coef[1], -arma21$coef[2]))),
                     Im(polyroot(c(1, -arma21$coef[1], -arma21$coef[2]))))
colnames(rootdf)[1] = "real"
colnames(rootdf)[2] = "imag"
modulus <- sqrt(rootdf$real^2+rootdf$imag^2)
rootdf$modulus <- modulus
format(round(rootdf, 4), nsmall = 4)
#Doesn't have a lower AIC than the AR(3)


#Estimate ARMA(3,1)
arma31 <- arima(x, order=c(3,0,1))
arma31
rootdf <- data.frame(Re(polyroot(c(1, -arma31$coef[1], -arma31$coef[2], -arma31$coef[3]))),
                     Im(polyroot(c(1, -arma31$coef[1], -arma31$coef[2], -arma31$coef[3]))))
colnames(rootdf)[1] = "real"
colnames(rootdf)[2] = "imag"
modulus <- sqrt(rootdf$real^2+rootdf$imag^2)
rootdf$modulus <- modulus
format(round(rootdf, 4), nsmall = 4)
#Doesnt have a lower AIC than the AR(3)

#Estimate ARMA(3,2)
arma32 <- arima(x, order=c(3,0,2))
arma32
rootdf <- data.frame(Re(polyroot(c(1, -arma32$coef[1], -arma32$coef[2], -arma32$coef[3]))),
                     Im(polyroot(c(1, -arma32$coef[1], -arma32$coef[2], -arma32$coef[3]))))
colnames(rootdf)[1] = "real"
colnames(rootdf)[2] = "imag"
modulus <- sqrt(rootdf$real^2+rootdf$imag^2)
rootdf$modulus <- modulus
format(round(rootdf, 4), nsmall = 4)
#Not a lower AIC than the AR3

#Tested for robustness, AR3 still produces the lowest AIC, which is 2811.49



#Exercise 4
library(ggplot2)
library(latex2exp)
library(dse)
library(vars)
library(grid)
library(gridExtra)

const <- c(3,2,1)
lagpoly <- array(c(1, -0.2, -0.3, 0, -0.4, -0.6, 
                   0, -0.6, -0.9, 0, 0.7, 0.1, 1, 0.5, 0.2, 0, 
                   0.3, -0.4, 0, 0, 0, 0, -0.6, 0,6, 1, -0.7, -0.8), 
                 c(3,3,3))
residcovmat <- diag(3)

var2 <- ARMA(A= lagpoly, B= residcovmat, TREND=const)
varsim <- simulate(var2, sampleT=500, noise=list(w=matrix(rnorm(1500), nrow=500, ncol=3)),
                   rng=list(seed=c(123456)))

time <- c(1:500)
var2df <- data.frame(time, matrix(varsim$output, nrow=500, ncol=3))
p1 <- ggplot(var2df, aes(x=time, y=X1)) + geom_line(size=0.5, colour="red") +
  labs(x="Time", y=TeX("x_1"))
p2 <- ggplot(var2df, aes(x=time, y=X2)) + geom_line(size=0.5, colour = "green") + 
  labs(x="Time", y=TeX("x_2"))
p3 <- ggplot(var2df, aes(x=time, y=X3)) + geom_line(size=0.5, colour = "blue") +
  labs(x="Time", y=TeX("x_3"))
grid.arrange(p1, p2, p3, ncol=1, top=textGrob("VAR(3) Time Series", gp=gpar(fontsize = 15, font = 2)))
arrangeGrob(p1, p2, p3, ncol=1, main=textGrob("VAR(3) Time Series", gp=gpar(fontsize = 15, font = 2)))



#Exercise 5
library(dse)
library(vars)

const <- c(3,2,1)
lagpoly <- array(c(1, -0.2, -0.3, 0, -0.4, -0.6, 
                   0, -0.6, -0.9, 0, 0.7, 0.1, 1, 0.5, 0.2, 0, 
                   0.3, -0.4, 0, 0, 0, 0, -0.6, 0,6, 1, -0.7, -0.8), 
                 c(3,3,3))
residcovmat <- diag(3)

var2 <- ARMA(A = lagpoly, B=residcovmat, TREND=const)
var2sim <- simulate(var2, sampleT=500, noise=list(w=matrix(rnorm(1500), nrow=500, ncol=3)),
                    rng=list(seed=c(123456)))
var2df <- data.frame(matrix(var2sim$output, nrow = 100, ncol=3))
colnames(var2df) <- c("x1", "x2", "x3")
VARselect(var2df, lag.max=5, type="const")

varsimest <- VAR(var2df, type ="const", lag.max=5, ic="AIC")
summary(varsimest, equation = "x1")
summary(varsimest, equation = "x2")
summary(varsimest, equation = "x3")

roots(varsimest)

#Exercise 6
serial.test(varsimest, lags.pt = 20, type="PT.asymptotic")
#Auto Regressive Heterscedasticity (ARCH) test
arch.test(varsimest, multivariate.only = TRUE)

normality.test(varsimest, multivariate.only = TRUE)


#Exercise 7
#Granger Causality Test
causalityTest <- causality(varsimest, cause = "x3")
causalityTest$Granger
format(round(qf(.95, df1=causalityTest$Granger$parameter[3],
                df2=causalityTest$Granger$parameter[1:2]), 4), nsmall=4)
#p-value is not less than 0.05, i.e. not signficiantly different from the null. 
#Therefore we cannot reject the null