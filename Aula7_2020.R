#'
#' title: "Cap 7  Forecasting with EqCM Models"
#' author: 
#'  - "Pedro Valls"
#' date: "11 of november 2022"
#' ---
#+ warning = FALSE, message = FALSE


rm(list = ls())
#setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time Series_Econometrics_2020/applied_economics_forecasting/appliedeconomicforecasting_R/cap7")


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

load_package("vars")
load_package("readxl")
load_package("vars")
load_package("car")
load_package("nlme")
load_package("data.table")
load_package("ggplot2")
load_package("lmtest")
load_package("tseries")
load_package("xtable")
load_package("forecast")
load_package("Metrics")
load_package("dplyr")
load_package("FinTS")
load_package("tsDyn")



library(data.table)
library(ggplot2)
library(vars)
library(tsDyn)


#'
#' ## **7.9 Simulated Data**
#' 
sim.data <- fread('simulated_cointegration.csv')
sim.data <- sim.data[, .(x, y)]

#'
#' ## ** Time series plot for the two series**
#' 

yhat.plot <- data.table('id' = 2:500,
                        'y' = sim.data$y[2:500],
                        'x'= sim.data$x[2:500])

ggplot(yhat.plot) + 
  geom_line(aes(x = id, y = y, colour="y"), linetype="solid") +
  geom_line(aes(x = id, y = x, colour="x"), linetype="solid") +
  scale_color_manual(values=c("y" = "#FF0000",
                              "x" = "#000000")) + 
  labs(
    title="Simulated non stationary VAR where y and x" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificacao da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 




#'
#' # **Correlogram*
#' 
par(mfrow=c(2,2))
sim.acf <- sim.pacf <- list()
sim.acf$x <- acf(sim.data[, x], lag.max = 20, main="ACF for x")
sim.acf$y <- acf(sim.data[, y], lag.max = 20, main="ACF for y")
sim.pacf$x <- pacf(sim.data[, x], lag.max = 20, main="PACF for x")
sim.pacf$y <- pacf(sim.data[, y], lag.max = 20, main="PACF for y")

#'
#' ## ADF Unit Root Test
#' 
sim.df <- list()
sim.df$y <- ur.df(sim.data[, y], type = "trend", lags = 1)
sim.df$dy <- ur.df(diff(sim.data[, y]), lags = 1)
summary(sim.df$y)
summary(sim.df$dy)

#'
#' ## **Forecasting**
#' 
sim.est <- sim.fore <- list()

#'
#' ## **VAR(2) in levels**
#' 
sim.est$var <- VAR(sim.data[1:400], p = 2, type = 'both')
sim.fore$var <- predict(sim.est$var, n.ahead = 100)
sim.fore$var <- data.table('x' = sim.fore$var$fcst$x[, 1],
                           'y' = sim.fore$var$fcst$y[, 1])

#'
#' ## **VAR(1) in differences**
#' 
sim.est$dvar <- VAR(apply(sim.data[1:400], 2, diff), p = 1)
sim.fore$dvar <- predict(sim.est$dvar, n.ahead = 100)
sim.fore$dvar <- data.table('x' = sim.fore$dvar$fcst$x[, 1],
                            'y' = sim.fore$dvar$fcst$y[, 1])
sim.fore$dvar <- cumsum(rbind(sim.data[400], sim.fore$dvar))
sim.fore$dvar <- sim.fore$dvar[2:101]

#'
#' ## **VECM**
#' 
sim.est$vec <- ca.jo(sim.data[1:400], type = 'eigen', ecdet = 'const',
                     K = 2, spec = 'longrun')
sim.fore$vec <- predict(cajools(sim.est$vec), n.ahead = 100)
sim.fore$vec <- sim.data[401:500] + cumsum(sim.fore$vec)

sim.plot <- data.table('X' = sim.data[401:500, x],
                       'X_VAR_LEVEL' = sim.fore$vec[, x],
                       'X_VAR_DIFF' = sim.fore$dvar[, x],
                       'X_VECM' = sim.fore$vec[, x],
                       'Y' = sim.data[401:500, y],
                       'Y_VAR_LEVEL' = sim.fore$vec[, y],
                       'Y_VAR_DIFF' = sim.fore$dvar[, y],
                       'Y_VECM' = sim.fore$vec[, y])

#'
#' ##  **X**
#' 
#'
#' ## **VAR in levels**
#' 
xhat.plot <- data.table(rbindlist(list(data.table(sim.plot[, X]),
                                       data.table(sim.plot[, X_VAR_LEVEL]))),
                        rep(c('X', 'X_VAR_LEVEL'), each = 100))

#'
#' ## **VAR in differences**
#' 
xhat.plot <- data.table(rbindlist(list(data.table(sim.plot[, X]),
                                       data.table(sim.plot[, X_VAR_DIFF]))),
                        rep(c('X', 'X_VAR_DIFF'), each = 100))

#'
#' **VECM**
#'
xhat.plot <- data.table(rbindlist(list(data.table(sim.plot[, X]),
                                       data.table(sim.plot[, X_VECM]))),
                        rep(c('X', 'X_VAR_VECM'), each = 100))

setnames(xhat.plot, c('xhat', 'label'))
ggplot(xhat.plot, aes(x = rep(501:600, 2), y = xhat, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank())

#'
#' ## **Y**
#' 
#'
#' ## **VAR in levels**
#' 
yhat.plot <- data.table(rbindlist(list(data.table(sim.plot[, Y]),
                                       data.table(sim.plot[, Y_VAR_LEVEL]))),
                        rep(c('Y', 'Y_VAR_LEVEL'), each = 100))

#'
#' ## **VAR in differences**
#' 
yhat.plot <- data.table(rbindlist(list(data.table(sim.plot[, Y]),
                                       data.table(sim.plot[, Y_VAR_DIFF]))),
                        rep(c('Y', 'Y_VAR_DIFF'), each = 100))

#'
#' ## **VECM**
#' 
yhat.plot <- data.table(rbindlist(list(data.table(sim.plot[, Y]),
                                       data.table(sim.plot[, Y_VECM]))),
                        rep(c('Y', 'Y_VAR_VECM'), each = 100))

setnames(yhat.plot, c('yhat', 'label'))
ggplot(yhat.plot, aes(x = rep(501:600, 2), y = yhat, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank())

