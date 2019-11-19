# Tutorial 6 R Code Luke Whitehill 13215948

#Excercise 1
library(urca)
data(Raotbl3)



for (i in Raotbl3[,2:3]) {
  i <- ts(data = Raotbl3[,2:3], start = c (1966,4), end = c(1991, 2), frequency = 4)
  df <- data.frame(quart=as.numeric(time(i)), logof = as.matrix(as.numeric(i)))
  plot(i, type = "l", main = "Log-Real Income and Log-Real Wealth")
}
  

print("Log - Real Income")

df1 <- summary(ur.df(i[,1], type = 'none', selectlags = "AIC"))

df2 <- summary(ur.df(i[,1], type = 'drift', selectlags = "AIC"))

df3 <- summary(ur.df(i[,1], type = 'trend', selectlags = "AIC"))

pp1 <- summary(ur.pp(i[,1], type = 'Z-tau', model = 'constant', lags = 'long'))

pp2 <- summary(ur.pp(i[,1], type = 'Z-tau', model = 'trend', lags = 'long'))
#Differencing Once
ddf1 <- summary(ur.df(diff(i[,1]), type = 'none', selectlags = "AIC"))

ddf2 <- summary(ur.df(diff(i[,1]), type = 'drift', selectlags = "AIC"))

ddf3 <- summary(ur.df(diff(i[,1]), type = 'trend', selectlags = "AIC"))

dpp1 <- summary(ur.pp(diff(i[,1]), type = 'Z-tau', model = 'constant', lags = 'long'))
dpp2 <- summary(ur.pp(diff(i[,1]), type = 'Z-tau', model = 'trend', lags = 'long'))

print("Log Real Wealth")

df4 <- summary(ur.df(i[,2], type = 'none', selectlags = "AIC"))

df5 <- summary(ur.df(i[,2], type = 'drift', selectlags = "AIC"))

df6 <- summary(ur.df(i[,2], type = 'trend', selectlags = "AIC"))

pp3 <- summary(ur.pp(i[,2], type = 'Z-tau', model = 'constant', lags = 'long'))

pp4 <- summary(ur.pp(i[,2], type = 'Z-tau', model = 'trend', lags = 'long'))
#Differencing Once
ddf4 <- summary(ur.df(diff(i[,2]), type = 'none', selectlags = "AIC"))

ddf5 <- summary(ur.df(diff(i[,2]), type = 'drift', selectlags = "AIC"))

ddf6 <- summary(ur.df(diff(i[,1]), type = 'trend', selectlags = "AIC"))

dpp3 <- summary(ur.pp(diff(i[,1]), type = 'Z-tau', model = 'constant', lags = 'long'))
dpp4 <- summary(ur.pp(diff(i[,1]), type = 'Z-tau', model = 'trend', lags = 'long'))




#Exercise 2
data(nporg)
tsnporg <- ts(data = nporg, start = c (1860), end = c(1970), frequency = 1)
tsnporgdf <- data.frame(tsnporg)
data2 <- data.frame(log(tsnporgdf$gnp.r), log(tsnporgdf$gnp.n), tsnporgdf$bnd, log(tsnporgdf$wg.n))


df99 <- summary(ur.df(na.omit(data2[,1]), type = 'none', selectlags = "AIC"))
pp99 <- summary(ur.pp(data2[,1], type = 'Z-tau', model = "trend", lags = 'long'))
ers99 <- summary(ur.ers(data2[,1], type = c("DF-GLS"), model = c("trend"), lag.max = 4))
sp99 <- summary(ur.sp(data2[,1], type = c("tau"), pol.deg = c(2), signif = c(0.05)))
kpss99 <- summary(ur.kpss(data2[,1], type = c("tau"), lags = c("long"), use.lag = NULL) )

df98 <- summary(ur.df(na.omit(data2[,2]), type = 'none', selectlags = "AIC"))
pp98 <- summary(ur.pp(data2[,2], type = 'Z-tau', model = "trend", lags = 'long'))
ers98 <- summary(ur.ers(data2[,2], type = c("DF-GLS"), model = c("trend"), lag.max = 4))
sp98 <- summary(ur.sp(data2[,2], type = c("tau"), pol.deg = c(2), signif = c(0.05)))
kpss98 <- summary(ur.kpss(data2[,2], type = c("tau"), lags = c("long"), use.lag = NULL) )

df97 <- summary(ur.df(na.omit(data2[,3]), type = 'none', selectlags = "AIC"))
pp97 <- summary(ur.pp(data2[,3], type = 'Z-tau', model = "trend", lags = 'long'))
ers97 <- summary(ur.ers(data2[,3], type = c("DF-GLS"), model = c("trend"), lag.max = 4))
sp97 <- summary(ur.sp(data2[,3], type = c("tau"), pol.deg = c(2), signif = c(0.05)))
kpss97 <- summary(ur.kpss(data2[,3], type = c("tau"), lags = c("long"), use.lag = NULL) )

df4 <- summary(ur.df(na.omit(data2[,4]), type = 'none', selectlags = "AIC"))
pp4 <- summary(ur.pp(data2[,4], type = 'Z-tau', model = "trend", lags = 'long'))
ers4 <- summary(ur.ers(data2[,4], type = c("DF-GLS"), model = c("trend"), lag.max = 4))
sp4 <- summary(ur.sp(data2[,4], type = c("tau"), pol.deg = c(2), signif = c(0.05)))
kpss4 <- summary(ur.kpss(data2[,4], type = c("tau"), lags = c("long"), use.lag = NULL) )



tsraotbl3 <- ts(data = Raotbl3$lc, start = c(1966,4), end = c(1991, 2), frequency = 4)
data3 <- data.frame(tsraotbl3)
for (k in data3) {
df9 <- summary(ur.df(k, type = 'none', selectlags = "AIC"))
pp9 <- summary(ur.pp(k, type = 'Z-tau', model = "trend", lags = 'long'))
ers9 <- summary(ur.ers(k, type = c("DF-GLS"), model = c("trend"), lag.max = 4))
sp9 <- summary(ur.sp(k, type = c("rho"), pol.deg = c(2), signif = c(0.05)))
kpss9 <- summary(ur.kpss(k, type = c("tau"), lags = c("long"), use.lag = NULL) )
}








# Extras if I have time:
#   - Add if statements 
#   - Possibility of adding more logic operators (like &) to increase robustness 
#   - Try to loop through tests for data sets. 
#   - cbind the results to an empty list so i dont have to type results in future.
