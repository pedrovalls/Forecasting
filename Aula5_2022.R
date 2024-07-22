

#'
#' title: "Cap 5  Forecasting with Time Series Models -  US Inventories "
#' author: 
#'  - "Pedro Valls"
#' date: "4 of november 2020"
#' ---
#+ warning = FALSE, message = FALSE


rm(list = ls())
# setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time Series_Econometrics_2022/applied_economics_forecasting/appliedeconomicforecasting_R/cap5")


#'
#' ## Loading Packages 
#' 
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
load_package("urca")
load_package("vars")
load_package("tsDyn")
load_package("readxl")
load_package("vars")
load_package("car")

load_package("tsDyn")
load_package("stargazer")
load_package("readxl")
load_package("printr")

load_package("nlme")
load_package("data.table")
load_package("ggplot2")
load_package("lmtest")
load_package("stargazer")
load_package("tseries")
load_package("xtable")
load_package("forecast")
load_package("Metrics")
load_package("dplyr")
load_package("FinTS")
load_package("foreign")
load_package("mFilter")

library(data.table)
library(ggplot2)
library(lmtest)
library(Metrics)
library(stargazer)
library(xtable)
library(forecast)
library(xtable)
library(FinTS)
library(printr)
library(foreign)
library(mFilter)




#'
#' ## **5.14.2 U.S. Inventories**
#' 
arima.inven <- fread('arma_inven.csv')

par(mfrow=c(3,1))

plot.label <- c('86', '88', '90', '92', '94', '96', '98', '00',
                '02', '04', '06', '08', '10', '12')
plot(arima.inven[, rcpi], type = 'n', main = 'RCPI - Real Private Inventories',
     xlab = '', ylab = '', xaxt = 'n')
axis(1, at = (1:14) * 8, labels = plot.label)
lines(arima.inven[, rcpi], type = 'l')

#'
#' ##**ACF / PACF**
#' 
#' 
# par(mfrow=c(2,1))
Acf.inven <- list('acf' = Acf(arima.inven[, rcpi[1:72]], lag.max = 15, main=" ACF for RCPI"),
                  'pacf' = pacf(arima.inven[, rcpi[1:72]], lag.max = 15, main=" PACF for RCPI"))

#'
#' ##**ADF test for the entire sample**
#' 
Adf.inven_total <- adf.test(arima.inven[, rcpi],k=4)
adf.inven_urca <- ur.df(arima.inven[,rcpi], type = 'trend', lags=0,selectlags = 'BIC')
#, k = 1)
print(Adf.inven_total)
print(adf.inven_urca)
summary(adf.inven_urca)



#'
#' ##**ADF test for the estimation sample up to 2002Q4**
#' 
adf.inven <- adf.test(arima.inven[, rcpi[1:72]])
print(adf.inven)



#'
#' ##**ARIMA selection**
#' 
#' 
#' in the book max ar.lag = 12 but the information matrix is singular, parameters are not identified
#'  reduced to max ar.lag = 11.
#'  
ic.inven <- list('AIC' = data.table(), 'BIC' = data.table())
for (ar.lag in 0:11) {
  arma.stat <- rep(0, 6)
  for (ma.lag in 0:2) {
    arma.fit <- arima(arima.inven[1:72, rcpi], order = c(ar.lag, 0, ma.lag))
    arma.fit
    arma.stat[ma.lag + 1] <- arma.fit$aic
    arma.stat[ma.lag + 4] <- -2 * arma.fit$loglik +
      (ar.lag + ma.lag) * log(71)
  }
  ic.inven$AIC <- rbindlist(list(ic.inven$AIC, data.table(t(arma.stat[1:3]))))
  ic.inven$BIC <- rbindlist(list(ic.inven$BIC, data.table(t(arma.stat[4:6]))))
}
setnames(ic.inven$AIC, c('MA0', 'MA1', 'MA2'))
ic.inven$AIC[, AR := 0:11]
setnames(ic.inven$BIC, c('MA0', 'MA1', 'MA2'))
ic.inven$BIC[, AR := (0:11)]


BIC_selec.mat <- rbind(ic.inven$BIC[, AR := (0:11)])
print(xtable(BIC_selec.mat))

AIC_selec.mat <- rbind(ic.inven$AIC[, AR := (0:11)])
print(xtable(AIC_selec.mat))


#' 
#' ##**ARMA(3, 2)**
#' 
arma.fit$rcpi <- arima(arima.inven[1:72, rcpi], order = c(3, 0, 2))
print(arma.fit$rcpi)

par(mfrow=c(2,1))
Acf(arma.fit$rcpi$residuals, lag.max = 12, main="ACF for Residual ARMA(3,2) for RCPI")
pacf(arma.fit$rcpi$residuals, lag.max = 12, main="PACF for Residual ARMA(3,2) for RCPI")


#' 
#' ##**AR(1)**
#' 
arma.fit$rcpi <- arima(arima.inven[1:72, rcpi], order = c(1,0,0))
print(arma.fit$rcpi)

par(mfrow=c(2,1))

Acf(arma.fit$rcpi$residuals, lag.max = 12, main="ACF for Residual AR(1) for RCPI")
pacf(arma.fit$rcpi$residuals, lag.max = 12, main="PACF for Residual AR(1) for RCPI")
#' 
#' ##**Diagnostic tests**
#' 
arima.inven[1:72, eps := as.numeric(arma.fit$rcpi$residuals)]
arima.inven$eps1 <- lag(arima.inven$eps, 1)
arima.inven$eps2 <- lag(arima.inven$eps, 2)

#'
#' ##**Breusch-Godfrey serial correlation LM test**
#' 
BG.test <- lm(eps ~ eps1 + eps2, data = arima.inven[3:72])
stargazer(BG.test, title = "Breusch-Godfrey serial correlation LM test", align = TRUE)

#'
#' ##**ARCH test**
#' 
ArchTest(arma.fit$rcpi$residuals, lags = 2)


#' 
#' ##**Jarque & Bera  test**
#' 
jarque.bera.test(arma.fit$rcpi$residuals)

#'
#' ##**White test**
#' 
white.test(arma.fit$rcpi$residuals)

#'
#' ##**Forecasts**
#' 
forecast.inven <- list()

#'
#' ##**1-step ahead**
#' 
forecast.inven$rcpi$dynamic <- as.numeric(predict(arma.fit$rcpi,
                                                  n.ahead = 16)$pred)
forecast.inven$rcpi$static <- rep(0, 16)
for (c in 1:16) {
  inven.fit <- arima(arima.inven[1:(71 + c), rcpi], order = c(3, 0, 2))
  forecast.inven$rcpi$static[c] <- predict(inven.fit, n.ahead = 1)$pred
}

#'
#' ##**2-step ahead**
#' 

forecast.inven$rcpi$multi <- rep(0, 16)
for (c in 1:16) {
  inven.fit <- arima(arima.inven[1:(70 + c), rcpi], order = c(3, 0, 2))
  forecast.inven$rcpi$multi[c] <- predict(inven.fit, n.ahead = 2)$pred[2]
}

#'
#'  ##**RMSE & MAE**
#'  
forecast.inven$rcpihat <- cbind(forecast.inven$rcpi$static,
                                forecast.inven$rcpi$dynamic,
                                forecast.inven$rcpi$multi)
RMSE <- sqrt(colSums((forecast.inven$rcpihat -
                        arima.inven[73:88, rcpi])^2) / 16)
MAE <- colSums(abs(forecast.inven$rcpihat - arima.inven[73:88, rcpi])) / 16
error.mat3 <- rbind(RMSE, MAE)
print(xtable(error.mat3))
colnames(error.mat3) <- c("1-step ahead", "2-steps ahead", "1-to X steps ahead")
error.mat3




#'
#' ##**Crisis period**
#' 
arma.fit$rcpic <- arima(arima.inven[1:100, rcpi], order = c(7, 0, 0))

forecast.inven$rcpic$dynamic <- as.numeric(predict(arma.fit$rcpic,
                                                   n.ahead = 12)$pred)
forecast.inven$rcpic$static <- rep(0, 12)
for (c in 1:12) {
  inven.fit <- arima(arima.inven[1:(100 + c), rcpi], order = c(7, 0, 0))
  forecast.inven$rcpic$static[c] <- predict(inven.fit, n.ahead = 1)$pred
}

#'
#' ##**2-step ahead**
#' 
forecast.inven$rcpic$multi <- rep(0, 12)
for (c in 1:12) {
  inven.fit <- arima(arima.inven[1:(100 + c), rcpi], order = c(7, 0, 0))
  forecast.inven$rcpic$multi[c] <- predict(inven.fit, n.ahead = 2)$pred[2]
}

#'
#' ##**RMSE & MAE**
#' 
forecast.inven$rcpichat <- cbind(forecast.inven$rcpic$dynamic,
                                 forecast.inven$rcpic$static,
                                 forecast.inven$rcpic$multi)
RMSE <- sqrt(colSums((forecast.inven$rcpichat -
                        arima.inven[101:112, rcpi])^2) / 12)
MAE <- colSums(abs(forecast.inven$rcpichat - arima.inven[101:112, rcpi])) / 12
error.mat4 <- rbind(RMSE, MAE)
print(xtable(error.mat4))


error.mat5 <- matrix(data=NA,nrow=2,ncol=4)
error.mat5[,3]=error.mat3[,1]
error.mat5[, 1]=error.mat4[,2]
error.mat5[, 2]=error.mat4[,1]
error.mat5[, 4]=error.mat4[,3]

row.names(error.mat5) <- c("RMSE","MAE")
colnames(error.mat5) <- c("1-step ahead", "2-steps ahead iterated", "2-steps ahead direct", "1-to X steps ahead")
error.mat5

#'
#' ##**HP Filter**
#' 
#' 
par(mfrow = c(1, 1))
rcpi.hpf <- hpfilter(arima.inven$rcpi, freq= 16000)


out <- cbind(rcpi.hpf$x, rcpi.hpf$trend, rcpi.hpf$cycle)
colnames(out) <- c("x", "trend", "cycle")

par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(out[,"x"], t= "n", main = paste(rcpi.hpf$title, "of", rcpi.hpf$xname))
lines(out[,"x"], col = "steelblue")
lines(out[,"trend"], col = "red")
legend("bottomleft", legend = c(rcpi.hpf$xname, "trend"), col = c("steelblue", "red"), lty = rep(1, 2), ncol = 2)
plot(out[,"cycle"], t = "n", main = "Cyclical component (deviations from trend)")
lines(out[,"cycle"], col = "steelblue")


#'
#' ##**ETs**
#' 
fit_ets_rcpi <- ets(arima.inven$rcpi,model="ZZZ")
summary(fit_ets_rcpi)
