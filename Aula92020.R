#'
#' title: "Cap 9  Forecasting with Nonlinear Models"
#' author: 
#'  - "Pedro Valls"
#' date: "25 of november 2020"
#' ---
#+ warning = FALSE, message = FALSE




#'
#' ## Clear Workspace
#' 
rm(list = ls())
#'
#' ## Change working drectory
#' 
#setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time_Series_Econometrics_2022/applied_economics_forecasting/appliedeconomicforecasting_R/cap9")


#'
#'  ## Load package using a function load_package
#'  
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load_package('tseries')
load_package('urca')
load_package('dlm')
load_package('openxlsx')
load_package('xts')
load_package('class')
load_package('zoo')
load_package('fBasics')
load_package('qrmtools')
load_package('stats')
load_package('MTS')
load_package('vars')
load_package('graphics')
load_package('data.table')
load_package('ggplot2')
load_package('Metrics')
load_package('forecast')
load_package('tsDyn')






library(data.table)
library(forecast)
library(ggplot2)
library(Metrics)
library(tsDyn)

#'
#' ## 9.8 Simulated Data 
#' 
# Fix the random seed
set.seed(123456)
y <- list('TAR' = rep(0, 601), 'STAR' = rep(0, 601))
e <- rnorm(601)

#'
#'  ## **Starting Values y[1] = 1.5 or y[1] = 0.5
#'  
y$TAR[1] <- y$STAR[1] <- 1.5

for (t in 2:601) {
  # TAR DGP
  y$TAR[t] <- (0.5 - 0.3 * y$TAR[t - 1]) * (1 - (y$TAR[t - 1] > 1)) +
    (0.1 + 0.8 * y$TAR[t - 1]) * (y$TAR[t - 1] > 1)
  
  # STAR DGP
  y$STAR[t] <- (2 + 0.9 * y$STAR[t - 1])*(1-1/(1 + exp(-5 * (y$STAR[t - 1] - 1)))) +
    (1 - 1.0 * y$STAR[t - 1]) / (1 + exp(-5 * (y$STAR[t - 1] - 1)))
}
y$TAR <- y$TAR[102:601] + e[102:601]
y$STAR <- y$STAR[102:601] + e[102:601]

#'
#' ## Plots
#' 
par(mfrow=c(2,1))
plot(y$TAR, type = 'l', xlab = '', ylab = '', main = 'TAR Model', col = 'blue')
plot(y$STAR, type = 'l', xlab = '', ylab = '', main = 'STAR Model', col = 'red')

#'
#'  ## Linearity tests
#'  

setarTest(y$TAR, m=2,  thDelay = 0, nboot=10, trim=0.1, 
          test=c("1vs"), hpc=c("foreach"))

setarTest(y$TAR, m=11, thDelay=0:1, nboot=5,trim=0.1, test="1vs")
setarTest(y$STAR,m=11, thDelay=0:1, nboot=5,trim=0.1, test="1vs") 
#'
#' ## Estimation
#' 
mod.test <- list()

# TAR
mod.test$tar <- setar(y$TAR[1:400], m = 1, d = 1)
summary(mod.test$tar)
# STAR
mod.test$star <- star(y$STAR[1:400], m=2, mTh = c(0,1) , d=1, noRegimes=2, control=list(maxit=3000), starting.control=list(gammaInt=c(1,100)))
summary(mod.test$star)



#'
#' ##  Forecasting
#' 
frc.test <- lapply(mod.test, predict, n.ahead = 100)

#'
#' ## 9.8 US Industrial Production Growth ###
#' 
ind.prod <- fread('indprod_ch9.csv')
prod <- list()
prod$est <- ind.prod[date >= '1930-01-01' & date <= '2000-01-01', indpro]
prod$frc <- ind.prod[date >= '2000-04-01' & date <= '2014-10-01', indpro]


#'
#' ## Plot the ind,prod
#' 
par(mfrow=c(1,1))
plot(ind.prod$date,ind.prod$indpro,type='l',col='red',xlab='time',ylab='indprod', main='Industrial Production Growth for US')

#'
#' ## ACF and PACF for ind.prod
#' 
par(mfrow=c(2,1))
acf(prod$est,lag.max=24,main='ACF Ind Prod')
pacf(prod$est,lag.max=24,main='PACF Ind Prod')
#'
#'  ## Parameter estimates
#'  
mod.prod <- list()
mod.prod$ar <- arima(prod$est, order = c(2, 0, 0))
mod.prod$tar <- setar(prod$est, m = 1, d = 1)
mod.prod$star <- star(prod$est, m = 2, d=1, noRegimes=2)

#'
#' ## Testing AR(2) agaisnt TAR(2) and TAR(3)
#' 

setarTest(prod$est, m=2, thDelay=0:1, nboot=5,trim=0.1, test="1vs")

#'
#'  ## Recursive 1-step ahead
#'  
frc.prod <- list('ar' = rep(0, 59), 'tar' = rep(0, 59), 'star' = rep(0, 59))

#'
#' ## AR(2)
#' 
prod$est <- ind.prod[date >= '1930-01-01' & date <= '2000-01-01', indpro]
for (t in 1:59) {
  mod.prod$ar <- arima(prod$est, order = c(2, 0, 0))
  frc.prod$ar[t] <- as.numeric(predict(mod.prod$ar, n.ahead = 1)$pred)
  prod$est <- c(prod$est, frc.prod$ar[t])
}
frc.prod$ar <- predict(mod.prod$ar, n.ahead = 59)
  
#'
#' ## TAR
#' 
prod$est <- ind.prod[date >= '1930-01-01' & date <= '2000-01-01', indpro]
for (t in 1:59) {
  mod.prod$tar <- setar(prod$est, m = 2, d = 1, thDelay = 1)
  frc.prod$tar[t] <- as.numeric(predict(mod.prod$tar, n.ahead = 1))
  prod$est <- c(prod$est, frc.prod$tar[t])
}
frc.prod$tar <- predict(mod.prod$tar, n.ahead = 59)

# setar(y$TAR[1:400], m = 1, d = 1)  
#'
#' ## STAR
#' 
prod$est <- ind.prod[date >= '1930-01-01' & date <= '2000-01-01', indpro]
for (t in 1:59) {
  mod.prod$star <- star(prod$est, m = 2,mTh = c(0,1) , d=1, noRegimes=2, control=list(maxit=3000), starting.control=list(gammaInt=c(1,100)))
  frc.prod$star[t] <- as.numeric(predict(mod.prod$star, n.ahead = 1))
  prod$est <- c(prod$est, frc.prod$star[t])
}
frc.prod$star <- predict(mod.prod$star, n.ahead = 59)

#'
#'  RMSE
#'  
rmse.prod <- lapply(frc.prod, rmse, actual = prod$frc)
#error.prod <- lapply(frc.prod, rmse, actual = prod$frc)

# Diebold-Mariano test
error.prod <- lapply(frc.prod, '-', prod$frc)
dm.test(error.prod$ar, error.prod$tar) # AR vs TAR
dm.test(error.prod$star, error.prod$ar) # STAR vs AR
dm.test(error.prod$star, error.prod$tar) # STAR vs TAR

# Plot
frc.plot <- data.table('Y' = prod$frc, 'tag' = 'Y')
for (model in c('ar', 'tar', 'star')) {
  frc.plot <- rbindlist(list(frc.plot,
                             data.table('Y' = frc.prod[[model]],
                                        'tag' = paste0('Y_', toupper(model)))))
}
frc.plot[tag == 'Y_AR', tag := 'Y_LINEAR']

ggplot(frc.plot, aes(x = rep(1:59, 4), y = Y, linetype = tag)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank())