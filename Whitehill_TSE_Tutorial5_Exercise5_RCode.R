# Tutorial 5 R Code Luke Whitehill 13215948

#Exercise 5 

#This week, due to my inability to get the w^n+1*T-n+1 exponentailly weighted average loop
#working, I will be putting my faith into a package called "forecast".

require(forecast)

data <- read.table("Tutorial 5.txt", header = T)
yield <- data$yield
day <- data$day
month <- data$mon
year <- data$year
dates <- as.Date.character(paste(year,"-", day, "-", month, sep=""))
df <- data.frame(dates, yield)
plot(df, type ="l", col = "green3", main = "Aaa Yields")

#Reducing the time to November 2010 creating a subsample
reducedtyield <- ts(df$yield[1:1103], start = c(1919), frequency = 12)

#Exponential Smoothing State Space Model
expsmthssm <- forecast(ets(reducedtyield), h = 12, level = c(95))
plot(expsmthssm)

expsmthssm

fval <- c(4.8815, 4.8905,4.8982,4.9089, 4.9158,4.9182,4.9201,4.9182,4.9201,4.9217,4.9229,4.9239)
plot(fval, type = 'l', col = "royalblue4", main = "Forecasted Aaa Yield vs Actual Aaa Yield ",
     xlab = "Month", ylab = "Yield")
par(new = T)
val <- c(5.02, 5.04, 5.22, 5.13, 5.16, 4.96, 4.99, 4.93, 4.37, 4.09, 3.98, 3.87)
plot(val, type = 'l', col = "red", xlab="", ylab = "")
par(new = F)
legend("bottom", c("Forecasted Yield", "Actual Yield"),
       col = c("royalblue4", "red"), lty = c(1, 2))