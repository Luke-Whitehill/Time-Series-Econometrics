# Tutorial 5 R Code Luke Whitehill 13215948

#Exercise 5 

library(ggplot2)
library(grid)
library(gridExtra)
library(latex2exp)
library(forecast)

#Read data from .txt file
data <- read.table("Tutorial 5.txt", header = T)
yield <- data$yield
day <- data$day
month <- data$mon
year <- data$year
dates <- as.Date.character(paste(year,"-", day, "-", month, sep=""))

ln_yield <- log(yield)
dln_yield <- diff(ln_yield)

df <- data.frame(dates , ln_yield, c(0, dln_yield))
colnames(df) <- c("date", "ln_yield", "dln_yield")
g1 <- ggplot(df, aes(x=dates , y=ln_yield)) + geom_line(size=1, colour="blue") +
  labs(x="Year", y=TeX("dlog(yield)")) +
  theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"))
g2 <- ggplot(df, aes(x=dates , y=dln_yield)) + geom_line(size=1, colour="blue") +
  labs(x="Year", y="dlog(yield)") +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))
grid.arrange(g1, g2, ncol=1,
                top=textGrob("Moody's Aaa", gp=gpar(fontsize=15, font=2)))


acf(dln_yield, lag.max=20, main="ACF of Differenced Log-Yield", ylab="Autocorrelation",
    ylim=c(-1, 1), lwd = 2, col="red", ci.col="blue")


arima011 <- arima(ln_yield, order=c(0,1,1))
arima011

Box.test(arima011$residuals, lag = 10, type = "Ljung-Box")
qchisq(p=0.90, df=10)



  
  