#25573 Time Series Econometrics Group Assignment
#Isaac Zanker 12899389
#Luke Whitehill 13215948
#Jameson Coombs 12895809

#s1 = Vale
#s2 = BHP
#Maybe make time series objects instead of a data.frame?

#Question 1
library(xlsx)
data<-read.xlsx("25573_Assignment_Data.xlsx", "Share Prices")
df <- data.frame(data)


#Question 2
library(urca)
summary(ur.df(df$logBHP, type = "drift", selectlags = 'AIC'))
summary(ur.df(df$logVALE, type = "drift", selectlags = 'AIC'))
# Also do type = trend just to be sure?

#Question 3
eg.reg1 <- lm(df$logBHP ~ df$logVALE) 
reg1smry <- summary(eg.reg1)
qt(0.95, reg1smry$df[2])

#Question 4
z <- eg.reg1$residuals
summary(ur.df(z, type = "none", selectlags = 'AIC'))

#Question 5 - Spread process (wt) graph, this process needs to be I(0), ADF test this
sprdprcss <- reg1smry$coefficients[1,1] + z
df$SpreadProcess <- sprdprcss
data1 <- data.frame(Date = df$Date, Process = df$SpreadProcess)
library(ggplot2)
ggplot() + ggtitle("Spread Process") +
  geom_line(data = data1, aes(x = Date, y = Process), colour = "black")
summary(ur.df(sprdprcss, type = "none", selectlags = 'AIC')) # We reject the null hypothesis of a unit root t stat < crit val



#Question 6 - Estimate the ECM
z_lag <- z[-c(1204, 1205)]
diffVale <- diff(df$logVALE)
diffBHP <- diff(df$logBHP)
eg.dat <- data.frame(embed(cbind(diffVale, diffBHP), 2))
colnames(eg.dat) <- c("diffVale", "diffBHP", "Vale_lag", "BHP_lag")

eg.reg2 <- lm(diffBHP ~ Vale_lag + BHP_lag + z_lag, data=eg.dat)
summary(eg.reg2)

#Question 7 - Testing the ECm

jotest=ca.jo(data.frame(df$logBHP,df$logVALE), type="trace", ecdet = "const", K=2, spec="longrun")
summary(jotest)

#Question 8 - simulare a pairs trading strategy outlined above, assuming the BHP position is always 1000 shares
#Plot cumulative Profit and Loss over the period.

s1 <- df$logVale
s2 <- df$logBHP
sprdprcss <- reg1smry$coefficients[1,1] + z #AKA w_t

wstar <- sd(sprdprcss)
smplebhp <- (df[1:834,4])
smplevale <- (df[1:834,5])
testbhp <- (df[835:1205,4])
testvale <- (df[835:1205,5])

# geom_dotplot(aes(y=sel$SpreadProcess), colour = "red") +
#   geom_dotplot(aes(y = buy$SpreadProcess), colour = "green")
# 
# 
# library(ggplot2)
# ggplot(data = df$SpreadProcess, mapping = aes(x = time, y = vals)) + geom_line(size = 0.5, colour = "blue")

# plot(sprdprcss, type = "l")
# abline(a = mean(sprdprcss)+wstar, b = FALSE, h = TRUE, col = "red")
# abline(a = mean(sprdprcss)-wstar, b = FALSE, h = TRUE, col = "red")
# abline(a = mean(sprdprcss), b = FALSE, h = TRUE)

sel <- subset(df, SpreadProcess > (mean(sprdprcss)+wstar))
buy <- subset(df, SpreadProcess < (mean(sprdprcss)-wstar))
unwnd <- subset(df, SpreadProcess == mean(SpreadProcess))

data1 <- data.frame(Date = df$Date, Process = df$SpreadProcess)
data2 <- data.frame(Date = sel$Date, Process = sel$SpreadProcess)
data3 <- data.frame(Date = buy$Date, Process = buy$SpreadProcess)
data4 <- data.frame(Date = unwnd$Date, Process = unwnd$SpreadProcess)


sprd = df$logBHP-theta*df$logVALE

data5 <- data.frame(Date = df$Date, Process = sprd)

library(ggplot2)
ggplot() + ggtitle("Model picking right times to trade on the spread process") +
  geom_line(data = data1, aes(x = Date, y = Process), colour = "black") +
  geom_point(data = data2, aes(x = Date, y = Process), colour = "red", size = 0.8) +
  geom_point(data = data3, aes(x = Date, y = Process), colour = "green", size = 0.8) +
  geom_hline(yintercept = mean(sprdprcss), linetype = "dashed", colour = "purple") + 
  geom_hline(yintercept = mean(sprdprcss)+wstar, linetype = "dashed", colour = "cyan") +
  geom_hline(yintercept = mean(sprdprcss)-wstar, linetype = "dashed", colour = "cyan") +
  geom_point(data = data4, aes(x = Date, y = Process), colour = "blue", size = 0.5)

dataa <- data.frame(Date = df$Date, Process = df$VALE)
datab <- data.frame(Date = df$Date, Process = df$BHP)

ggplot() + ggtitle("BHP and Vale Share Prices") +
  geom_line(data = dataa, aes(x = Date, y = Process), colour = "black") +
  geom_line(data = datab, aes(x = Date, y = Process), colour = "grey")

# s1 <- df$logVale
# s2 <- df$logBHP


logValedata <- data.frame(Date = df$Date, Process = df$logVALE)
logBHPdata <- data.frame(Date = df$Date, Process = df$logBHP)
bb2 <- data.frame(Date = sel$Date, Process = sel$logBHP)
bs1 <- data.frame(Date = sel$Date, Process = sel$logVALE)
ss2 <- data.frame(Date = buy$Date, Process = buy$logBHP)
sb1 <- data.frame(Date = buy$Date, Process = buy$logVALE)

ggplot() + ggtitle("BHP and Vale Log-Share Prices with Buy and Sell position superimposed") +
  geom_line(data = logValedata, aes(x = Date, y = Process), colour = "black") +
  geom_line(data = logBHPdata, aes(x = Date, y = Process), colour = "grey") + 
  geom_point(data = bb2, aes(x = Date, y = Process), colour = "green", size = 0.5) +
  geom_point(data = bs1, aes(x = Date, y = Process), colour = "red", size = 0.5) + 
  geom_point(data = ss2, aes(x = Date, y = Process), colour = "red", size = 0.5) +
  geom_point(data = sb1, aes(x = Date, y = Process), colour = "green", size = 0.5) +
  guides(title = "Stocks" ) #Find how to make a legend work

# #Buy the spread process
# 

# buy stock 2
# sell stock 1
#   
# #Sell the spread process
# sell stock 2
# buy stock 1
# 

s1 <- df$logVale
s2 <- df$logBHP
theta <- reg1smry$coefficients[2,1]

#LETS TRADE:



trdlower <- 0
trdupper <- 0
trdcnt <- 0
flag <- 0
dough <- 0
bf <- 1000
pl <- 0
difference <- 0 
cash <- as.data.frame(matrix(ncol=1, nrow=0))
dought <- 0
doughtt <- 0 
money <- 0
capital <- 0
value <- 0
val <- 0
dif <- 0 
mahney <- 0
losgai <- 0
moy <- as.data.frame(matrix(ncol = 1, nrow = 0))

for (i in df$SpreadProcess) {
  if (flag == 0)  { # flag == 0 means no trade open
    if (i > mean(df$SpreadProcess) + wstar) {
      #Here we are 'selling' the process, which means the spread process
      #Has reached above the threshold. We will Short 1 stock of S2 (BHP)
      #And buy theta of stock S1 (VALE)
      money <- money
      mahney <- rbind(mahney, list(money))
      trdupper <- trdupper + 1 #Just counting the amount of uuper bound trades we're completing
      flag <- 1
      
      losgai <- money/1000
      moy <- rbind(moy, list(losgai))
    }
    else if (i < mean(df$SpreadProcess) - wstar) {
      #Here we are 'buying' the process, which means the spread process
      #Has reached below the threshold. We will Buy 1 stock of S2 (BHP) and 
      #Short theta of stock S1 (VALE)
      money <- money
      mahney <- rbind(mahney, list(money))
      trdlower <- trdlower + 1
      flag <- 2
      
      losgai <- money/1000
      moy <- rbind(moy, list(losgai))
      
    }
    else if ((i >= mean(df$SpreadProcess) - wstar) & (i <= mean(df$SpreadProcess) + wstar)) {
      money <- money
      mahney <- rbind(mahney, list(money))
      
      losgai <- money/1000
      moy <- rbind(moy, list(losgai))
    }
  }
  else if (flag != 0) {
    # Trade is already open
    if (flag == 1) {
      if (i < mean(df$SpreadProcess)) {
        trdcnt = trdcnt + 1
        flag <- 0
        dif <- dif+ (mean(df$SpreadProcess)-i)
        money <- money + (dif + wstar)*1000
        mahney <- rbind(mahney, list(money))
        
        losgai <- money/1000 + (mean(df$SpreadProcess) + wstar - i)
        moy <- rbind(moy, list(losgai))
      }
      else {
        money <- money
        mahney <- rbind(mahney, list(money))
        
        losgai <- money/1000 + (mean(df$SpreadProcess) + wstar - i)
        moy <- rbind(moy, list(losgai))
      }
    }
    if (flag == 2) {
      if (i > mean(df$SpreadProcess)) {
        trdcnt = trdcnt + 1
        flag <- 0
        dif <- dif+ (i - mean(df$SpreadProcess))
        money <- money + (dif + wstar)*1000
        mahney <- rbind(mahney, list(money))
        
        losgai <- money/1000 + i-(mean(df$SpreadProcess) - wstar)
        moy <- rbind(moy, list(losgai))
      }
      else {
        money <- money
        mahney <- rbind(mahney, list(money))
        
        losgai <- money/1000 + i-(mean(df$SpreadProcess) - wstar)
        moy <- rbind(moy, list(losgai))
      }
    }
  }
  print(losgai)
}

losgai


thingy <- cbind(df$Date, moy)
thingo <- data.frame(Date = df$Date, Profit = thingy[,2])
ggplot() + geom_line(thingo, mapping = aes(x = Date, y = Profit), colour = "black") + 
  ggtitle("Cumulative P&L Over the Period (With Unrealised Gain/Losses)")

mahney <- mahney[2:1206,]
df$mahney <- mahney
dollaroos <- data.frame(matrix(unlist(mahney), nrow=length(mahney), byrow = T))
# ggplot() + geom_step(df, mapping = aes(x = Date, y = mahney), colour = "black")
mooney <- data.frame(Date = df$Date, Profit = dollaroos)
colnames(mooney) <- c("Date", "Profit")
ggplot() + geom_line(mooney, mapping = aes(x = Date, y = Profit), colour = "black") + 
  ggtitle("Cumulative P&L Over the Period (Without Unrealised Gain/Losses)")


#Question 9:
##Daily Profit Loss (Snapshot)

snapshot <- cbind(df$Date, moy*1000)
plot(snapshot, type = "l")


returns <- diff(snapshot$X0)
returnsmate <- data.frame(returns = returns)

ggplot(returnsmate, aes(x = returns)) + geom_histogram(bins = 100) + ggtitle( "Histogram of the daily P&L with 95% Confidence Interval") +
  geom_vline(aes(xintercept = mean(returns)+sd(returns)*1.96), colour = "blue", linetype ="dashed", size = 0.4) +
  geom_vline(aes(xintercept = mean(returns)-sd(returns)*1.96), colour = "blue", linetype ="dashed", size = 0.4)



#Question 10: Sharpe Ratio
#Over the 2002- 2007 period, we will be assume a risk free rate of 5%
riskf <- 0.05
sharpieratio <- ((mean(mooney[,2])-riskf)/(sd(mooney[,2])))


