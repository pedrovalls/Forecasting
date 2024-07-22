#'
#' title: "Cap 6  Forecasting with VAR Models"
#' author: 
#'  - "Pedro Valls"
#' date: "11 of november 2020"
#' ---
#+ warning = FALSE, message = FALSE


rm(list = ls())
setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time Series_Econometrics_2020/applied_economics_forecasting/appliedeconomicforecasting_R/cap6")


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
#' ##** 6.9 Simulated Data**
#' 

sim.data <- fread('var_simulated_ch6_sec9.csv')

#'
#'  ## **Time series plot**
#'  


yhat.plot <- data.table('id' = 101:500,
                        'y1' = sim.data$y[101:500],
                        'y2'= sim.data$x[101:500])

ggplot(yhat.plot) + 
  geom_line(aes(x = id, y = y1, colour="y1"), linetype="solid") +
  geom_line(aes(x = id, y = y2, colour="y2"), linetype="solid") +
  scale_color_manual(values=c("y1"="#FF0000",
                              "y2"="#000000")) + 
  labs(
    title="Simulated VAR where y1 = y and y2 = x" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificacao da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 







#'
#' ## ** VAR lag order**
#' 
VARselect(sim.data[101:500], lag.max = 12)

#'
#' ## **Best model VAR(1) using BIC information riterion**
#' 
summary(VAR(sim.data[101:500], p = 1))
VAR1<- VAR(sim.data[101:500], p = 1)

#'
#' ## **Estimate an overparametrized model, VAR(4), in order to compare the out-of-sample forecasts**
#' 
summary(VAR(sim.data, p = 4))
VAR4 <- VAR(sim.data, p = 4)

#'
#' ## **VAR(1) diagnostic tests**
#' 
var.sim1 <- VAR(sim.data, p = 1)

var.test1 <- list()

#'
#' ## **VAR(1) residual normality test**
#' 
var.test1$norm <- normality.test(var.sim1)

#'
#' ## **VAR(1) residual heteroskedasticity test**
#' 
var.test1$arch <- arch.test(var.sim1)

#'
#' ## **VAR(1) residual serial correlation test**
#' 
var.test1$serial <- serial.test(var.sim1, lags.pt = 2)

var.test1




#'
#' ## **VAR(4) diagnostic tests**
#' 
var.sim4 <- VAR(sim.data, p = 4)

var.test4 <- list()

#'
#' ## **VAR(4) residual normality test**
#' 
var.test4$norm <- normality.test(var.sim4)

#'
#' ## **VAR(4) residual heteroskedasticity test**
#' 
var.test4$arch <- arch.test(var.sim4)

#'
#' ## **VAR(4) residual serial correlation test**
#' 
var.test4$serial <- serial.test(var.sim4, lags.pt = 6)

var.test4

#'
#' ## **Static and dynamic forecasts for VAR(1)**
#' 
var.pred <- predict(var.sim1, n.ahead = 100)
var.hat <- cbind(var.pred$fcst$y[, 1], var.pred$fcst$x[, 1])
RMSE <- sqrt(colSums((var.hat - sim.data[501:600])^2) / 100)
MAE <- colSums(abs(var.hat - sim.data[501:600])) / 100 
error.mat <- rbind(RMSE, MAE)

#'
#' ## **RMSE and MAE for h-steps ahead VAR(1)**
#' 

error.mat
print(xtable(error.mat))

yhat <- rep(0,100)
xhat <- rep(0,100)

for (i in 1:100) {
  yhat[i]= VAR1$varresult$y$coefficients[3]+VAR1$varresult$y$coefficients[1]*sim.data[500+i-1,1]+VAR1$varresult$y$coefficients[2]*sim.data[500+i-1,2]  
  xhat[i]= VAR1$varresult$x$coefficients[3]+VAR1$varresult$x$coefficients[1]*sim.data[500+i-1,1]+VAR1$varresult$x$coefficients[2]*sim.data[500+i-1,2]  
  
}
var.pred_dyn <- matrix(data=NA,nrow=100,ncol=2)
var.pred_dyn[ ,1]=as.numeric(yhat)
var.pred_dyn[ ,2]=as.numeric(xhat)

RMSE_dyn <- sqrt(colSums((var.pred_dyn - sim.data[501:600])^2) / 100)
MAE_dyn <- colSums(abs(var.pred_dyn - sim.data[501:600])) / 100 

error.mat_dyn <- rbind(RMSE_dyn, MAE_dyn)

#'
#' ## **RMSE and MAE for one-step ahead VAR(1)**
#' 

error.mat_dyn
print(xtable(error.mat_dyn))




#'
#'  ## **Static one-step ahead**
#'  


yhat.plotfy <- data.table('id' = 501:600,
                        'y1' = sim.data$y[501:600],
                        'yhat'=var.pred_dyn[ ,1])

ggplot(yhat.plotfy) + 
    geom_line(aes(x = id, y = y1, colour="y1"), linetype="solid") +
    geom_line(aes(x = id, y = yhat, colour="yhat"), linetype="solid") +
    scale_color_manual(values=c("y1"="#FF0000",
                              "yhat"="#000000")) + 
  
  labs(
    title="Static one-step ahead for y1 = y and yhat for VAR(1)" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 



yhat.plotfx <- data.table('id' = 501:600,
                          'y2'= sim.data$x[501:600],
                          'xhat'=var.pred_dyn[ ,2])

ggplot(yhat.plotfx) + 
  geom_line(aes(x = id, y = y2, colour="y2"), linetype="solid") +
    geom_line(aes(x = id, y = xhat, colour="xhat"), linetype="solid") +
    scale_color_manual(values=c("y2"="#FF0000",
                              "xhat"="#000000")) + 
    labs(
    title="Static one-step ahead for y2 = x and xhat for VAR(1)" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 


#'
#'  ## **Dynamic h-step ahead**
#'

yhat.plotfyd <- data.table('id' = 501:600,
                          'y1' = sim.data$y[501:600],
                          'yhat'=var.pred$fcst$y[, 1])

ggplot(yhat.plotfyd) + 
  
  geom_line(aes(x = id, y = y1, colour="y1"), linetype="solid") +
    geom_line(aes(x = id, y = yhat, colour="yhat"), linetype="solid") +
    scale_color_manual(values=c("y1"="#FF0000",
                              "yhat"="#000000")) + 
    labs(
    title="Dynamic h-steps ahead for y1 = y and yhat for VAR(1)" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 



yhat.plotfxd <- data.table('id' = 501:600,
                          'y2'= sim.data$x[501:600],
                          'xhat'=var.pred$fcst$x[, 1])

ggplot(yhat.plotfxd) + 
  geom_line(aes(x = id, y = y2, colour="y2"), linetype="solid") +
  geom_line(aes(x = id, y = xhat, colour="xhat"), linetype="solid") +
    scale_color_manual(values=c("y2"="#FF0000",
                              "xhat"="#000000")) + 
    labs(
    title="Dynamic h-steps ahead for y2 = x and xhat for VAR(1)" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 



#'
#' ## **ARMA fit**
#' 
arma.sim <- arma.pred <- list()
arma.sim$y <- arima(sim.data[101:500, y], order = c(1, 0, 0))
arma.sim$x <- arima(sim.data[101:500, x], order = c(1, 0, 3))

arma.pred$y <- predict(arma.sim$y, n.ahead = 100)
arma.pred$x <- predict(arma.sim$x, n.ahead = 100)

arma.hat <- cbind(as.numeric(arma.pred$y$pred), as.numeric(arma.pred$x$pred))
RMSE <- sqrt(colSums((arma.hat - sim.data[501:600])^2) / 100)
MAE <- colSums(abs(arma.hat - sim.data[501:600])) / 100 
error.matu <- rbind(RMSE, MAE)

#'
#' ## **RMSE and MAE for h-steps ahead univariate ARMA models for each variables**
#' 


error.matu
print(xtable(error.matu))

#'
#'  ## **Dynamic h-step ahead using univariate ARMA models**
#'

yhat.plotfyd <- data.table('id' = 501:600,
                           'y1' = sim.data$y[501:600],
                           'yhat'=arma.hat[, 1])

ggplot(yhat.plotfyd) + 
  
  geom_line(aes(x = id, y = y1, colour="y1"), linetype="solid") +
  geom_line(aes(x = id, y = yhat, colour="yhat"), linetype="solid") +
  scale_color_manual(values=c("y1"="#FF0000",
                              "yhat"="#000000")) + 
  labs(
    title="Dynamic h-steps ahead for y1 = y and yhat for univariate ARMA models" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 



yhat.plotfxd <- data.table('id' = 501:600,
                           'y2'= sim.data$x[501:600],
                           'xhat'=arma.hat[, 2])

ggplot(yhat.plotfxd) + 
  geom_line(aes(x = id, y = y2, colour="y2"), linetype="solid") +
  geom_line(aes(x = id, y = xhat, colour="xhat"), linetype="solid") +
  scale_color_manual(values=c("y2"="#FF0000",
                              "xhat"="#000000")) + 
  labs(
    title="Dynamic h-steps ahead for y2 = x and xhat for univariate ARMA models" ,         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 


#'
#' ## **IRF**
#' 

irf.sim <- irf(var.sim1, impulse = c('y', 'x'), response = c('y', 'x'))
plot(irf.sim)

#'
#' ## Changing the order, now x is first and y is second
#' 
irf.sim_xy <- irf(var.sim1, impulse = c('x', 'y'), response = c('x', 'y'))
plot(irf.sim_xy)



#'
#' ## **Static and dynamic forecasts for VAR(4)**
#' 
var.pred4 <- predict(var.sim4, n.ahead = 100)
var.hat4 <- cbind(var.pred4$fcst$y[, 1], var.pred4$fcst$x[, 1])
RMSE4 <- sqrt(colSums((var.hat4 - sim.data[501:600])^2) / 100)
MAE4 <- colSums(abs(var.hat4 - sim.data[501:600])) / 100 
error.mat4 <- rbind(RMSE4, MAE4)
error.mat4
print(xtable(error.mat4))


#'
#' ## **RMSE and MAE for h-steps ahead VAR(4)**
#' 
yhat4 <- rep(0,100)
xhat4 <- rep(0,100)

for (i in 1:100) {
  yhat4[i]= VAR4$varresult$y$coefficients[9]+VAR4$varresult$y$coefficients[1]*sim.data[500+i-1,1]+VAR4$varresult$y$coefficients[2]*sim.data[500+i-1,2]+
    VAR4$varresult$y$coefficients[3]*sim.data[500+i-2,1]+VAR4$varresult$y$coefficients[4]*sim.data[500+i-2,2]+
    VAR4$varresult$y$coefficients[5]*sim.data[500+i-3,1]+VAR4$varresult$y$coefficients[6]*sim.data[500+i-3,2]+
    VAR4$varresult$y$coefficients[7]*sim.data[500+i-4,1]+VAR4$varresult$y$coefficients[8]*sim.data[500+i-4,2]
  xhat4[i]= VAR4$varresult$x$coefficients[9]+VAR4$varresult$x$coefficients[1]*sim.data[500+i-1,1]+VAR4$varresult$x$coefficients[2]*sim.data[500+i-1,2]+
    VAR4$varresult$x$coefficients[3]*sim.data[500+i-2,1]+VAR4$varresult$x$coefficients[4]*sim.data[500+i-2,2]+
    VAR4$varresult$x$coefficients[5]*sim.data[500+i-3,1]+VAR4$varresult$x$coefficients[6]*sim.data[500+i-3,2]+
    VAR4$varresult$x$coefficients[7]*sim.data[500+i-4,1]+VAR4$varresult$x$coefficients[8]*sim.data[500+i-4,2]
  
}
var.pred_dyn4 <- matrix(data=NA,nrow=100,ncol=2)
var.pred_dyn4[ ,1]=as.numeric(yhat4)
var.pred_dyn4[ ,2]=as.numeric(xhat4)

RMSE_dyn4 <- sqrt(colSums((var.pred_dyn4 - sim.data[501:600])^2) / 100)
MAE_dyn4 <- colSums(abs(var.pred_dyn4 - sim.data[501:600])) / 100 

error.mat_dyn4 <- rbind(RMSE_dyn4, MAE_dyn4)

#'
#' ## **RMSE and MAE for one-step ahead VAR(4)**
#' 

error.mat_dyn4
print(xtable(error.mat_dyn4))
