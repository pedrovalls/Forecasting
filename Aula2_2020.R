#'
#' title: "Cap 2 Model Mis-Specification"
#' author: 
#'  - "Pedro Valls"
#' date: "18 de outubro de 2022"
#' ---
#+ warning = FALSE, message = FALSE





rm(list = ls())
#setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time Series_Econometrics_2022/applied_economics_forecasting/appliedeconomicforecasting_R/cap2")
setwd("/Users/pedrovallspereira/Dropbox/Special_Topics_in_Time_Series_Econometrics_2022/applied_economics_forecasting/appliedeconomicforecasting_R/cap2")
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
load_package("tseries")
load_package("tsDyn")
load_package("stargazer")
load_package("readxl")
load_package("data.table")
load_package("ggplot2")
load_package("xtable")
load_package("lmtest")
load_package("structchange")
load_package("stats")
load_package("e1071")
load_package("car")
load_package("xts")
load_package("zoo")
load_package("fBasics")
load_package("qrmtools")
load_package("fDMA")
load_package("TSA")
load_package("roll")
load_package("MTS")
load_package("forecast")

load_package('utils')
load_package('Rtools')
load_package('moments')
# install.packages("car",depedencies = T)
library('e1071')


library(readxl)
library(urca)
library(vars)
library(data.table)
library(ggplot2)
library(xtable)
library(lmtest)
library(strucchange)
library(tseries)


#'
#' ## **Loading Simulated Data**
#' 

sim.data <- list()
sim.data$full <- fread('simulated_datac2.csv')

#'
#' ## **Estimation Period Data**
#' 

sim.data$est <- sim.data$full[102:301] # estimation sample

#'
#' ## **Forecasting Period Data**
#' 


sim.data$fore <- sim.data$full[302:401] # forecasting sample

#'
#' ## **OLS for Estimation Period**
#' 


eq1 <- lm(y ~ x, data = sim.data$est)

#'
#' ## **Plot the residuals**
#' 
res_eq1 =eq1$residuals

ggplot(sim.data$est, aes(x = 102:301, y = res_eq1)) + geom_line(colour='red') +
   theme_bw() + xlab('') + ylab('') + ggtitle('Residuals')

#'
#' ## **Plot the squared residuals**
#' 

res_eq1_sq=res_eq1^2

ggplot(sim.data$est, aes(x = 102:301, y = res_eq1_sq)) + geom_line(colour='blue') +
  theme_bw() + xlab('') + ylab('') + ggtitle('Squared Residuals')

#'
#' ## **Goldfeld -Quant Test**

TF = length(sim.data$est$x)
T1=0.2*TF
T2=TF-T1

#'
#' ## **re-order data order by x**
#' 
new_data=sim.data$est[,3:4]
o <- order(new_data$x)
sim.data_est_order <- rbind(new_data$x[o],new_data$y[o])
sim.data_est_order_first_part=sim.data_est_order[ , 1:T1]
sim.data_est_order_last_part=sim.data_est_order[ , T2:T]
Data_first_part = as.data.frame(t(sim.data_est_order_first_part))
colnames(Data_first_part) <- c("x","y")

Data_last_part = as.data.frame(t(sim.data_est_order_last_part))
colnames(Data_last_part) <- c("x","y")

#'
#' ## OLS first T1 observations
#' 

eq2_first_part <- lm(y ~x , data=Data_first_part)

#'
#' ## OLS last TF-T2+1 observations
#' 

eq2_last_part <- lm(y ~x , data=Data_last_part)

#'
#' ## GQ statistics
#' 
RSS1= sum(resid(eq2_first_part)^2)
RSS2= sum(resid(eq2_last_part)^2)

GQ = RSS2/RSS1
GQ1=1/GQ
GQ1

#'
#' ## **p-value**
#' 
p_value_GQ =  1-pf(GQ,TF-T2-2,T1-2)
p_value_GQ

p_value_GQ1 =  1-pf(GQ1,T1-2,TF-T2-2)
p_value_GQ1

#'
#' ## **Since the p-value is equal to $3.17 \time 10^{-5}$ we reject $H_{0}$**
#' 



#' 
#' ## **Breusch Pagan Test based in Koenker(1981)**
#' 
bptest(eq1)
eq3_bp <- lm(res_eq1_sq ~ sim.data$est$x)
summary(eq3_bp)

str(summary(eq3_bp))
tr2=(summary(eq3_bp)$r.squared)*200
tr2
1-pchisq(tr2,1)
#'
#' ## **Since the p-value is equal to 0.7111 we do not reject $H_{0}$**
#'



#' 
#' ## **White Test for heteroskedasticity including cross terms**
#' 
sim.data$est[, eps := eq1$residuals]
sim.data$est[, c('x2', 'eps2') := list(x^2, eps^2)]
white.fit <- lm(eps2 ~ x2 + x, data = sim.data$est) 
summary(white.fit)
tr2w = (summary(white.fit)$r.squared)*200
tr2w
1-pchisq(tr2w,1)
#'
#' ## **Since the p-value is equal to 0.5486 we do not reject $H_{0}$**
#'


#' 
#' ## **White Test for heteroskedasticity without cross terms**
#' 
sim.data$est[, eps := eq1$residuals]
sim.data$est[, c('x2', 'eps2') := list(x^2, eps^2)]
white.fit_no_cross_terms <- lm(eps2 ~ x2 , data = sim.data$est) 
summary(white.fit_no_cross_terms)
tr2w_nct = (summary(white.fit_no_cross_terms)$r.squared)*200
tr2w_nct
1-pchisq(tr2w_nct,1)

#'
#' ## **Since the p-value is equal to 0.6696 we do not reject $H_{0}$**
#'

#'
#' ## **Normality test & Histogram**
#' 
res_eq1=ts(eq1$residuals)

par(mfrow=c(1,1))
hist(res_eq1, breaks = 20, freq=F,
     xlab='', ylab='', main='')
dist<-function(n){
  dnorm(n, mean(res_eq1), sd(res_eq1))
}
curve(dist, add=T, col='red')
d<-density(res_eq1)
lines(d, col='blue')
legend('topleft', legend=c('Residuals','Normal','Kernel'),
       col=c(1,2,4), pch=15)

jarque.bera.test(res_eq1) 

#'
#' ## **Since the p-value is equal to 0.2904 we do not reject $H_{0}$**
#'



#'
#' ## **Testing Serial Correlation using Durbin-Watson test**
#' 

dwtest(eq1) 

#'
#' ## DW test can only be used to test for AR(1) with $\rho > 0$ 
#' ## here the test is used as an exact test which is not suitable.
#' ## Also this is a one-sided test, the alternative 
#' ## is positive first order autocorrelation
#' 


#'
#' ## **Testing Serial Correlation using Breusch-Godfrey test**
#' 

bgtest(eq1) 

#'
#' ## **Chow break point test** 
#' 
sctest(y ~ x, data = sim.data$est, type = 'Chow', from = 0.1, to = 0.25)
sctest(y ~ x, data = sim.data$est, type = 'Chow', from = 0.25, to = 0.5)
sctest(y ~ x, data = sim.data$est, type = 'Chow', from = 0.5, to = 0.75)
sctest(y ~ x, data = sim.data$est, type = 'Chow', from = 0.75, to = 1.0)

fs1 <- Fstats(y ~ x, data = sim.data$est, from = 0.25, to = 1)
fs2 <- Fstats(y ~ x, data = sim.data$est, from = 0.5, to = 1)
fs3 <- Fstats(y ~ x, data = sim.data$est, from = 0.75, to = 1)


#'
#' ## **Bai-Perron test**
#' 

breakpoints(y ~ x, data = sim.data$est, h = 0.15)



#'
#' ## **Dummy variable**
#' 

olsD.fit <- lm(y ~ D + x + D * x, data = sim.data$est)
summary(olsD.fit)

#'
#' ## **Simple forecasts - with / no dummy**
#'  
yhat <- list()
yhat$y <- predict(eq1, newdata = sim.data$fore) # no dummy
yhat$yD <- predict(olsD.fit, newdata = sim.data$fore) # with dummy

yhat$yD.se <- sqrt(sum(olsD.fit$residuals^2) / 198)
yhat$yD.up <- yhat$yD + 1.96 * yhat$yD.se
yhat$yD.low <- yhat$yD - 1.96 * yhat$yD.se




#'
#' ## **Plot - y / yhat_OLS / yhat_OLSD / yhat_OLSD_up / yhat_OLS_D_low
#'  


#'
#' ## **Define the data frame with Observations (y), forecast without dummy (yhat_OLS), forecast with dummy (y_OLSD) and up and lower CI**
#' 
# CODIGO NOVO

# Vamos organizar um data.frame contendo 6 colunas:
# 1) as 100 obserservaçoes de 302 a 401 (inclusive)
# 2) yhat
# 3) YHAT_OLS
# 4) YHAT_OLSD
# 5) yhat$yD.up
# 6) yhat$yD.low
yhat.plot <- data.table('id' = 302:401,
                        'yhat' = sim.data$fore[, y],
                        'YHAT_OLS' = yhat$y,
                        'YHAT_OLSD' = yhat$yD,
                        'yD.up' = yhat$yD.up,
                        'yD.low' = yhat$yD.low)

# Vamos plotar um grafico utilizando como base de dados o data.frame "yhat.plot" que acabamos de criar
# e adicionar 5 Layers de informacao
ggplot(yhat.plot) + 
  # primeiro layer: 
  # geom_line: grafico de linha
  # eixo x: coluna "id" 
  # eixo y: coluna "yhat"
  # colorida por "yhat" (a cor sera definida depois - O nome da cor será o que vai aparecer na legenda)
  # linetype: solido
  geom_line(aes(x = id, y = yhat, colour="y.hat"), linetype="solid") +
  # segundo layer: eixo x: coluna "id" e eixo y: coluna "YHAT_OLS" linetype: solid
  geom_line(aes(x = id, y = YHAT_OLS, colour="y.hat OLS"), linetype="solid") +
  # terceiro layer: eixo x: coluna "id" e eixo y: coluna "YHAT_OLSD" linetype: solid
  geom_line(aes(x = id, y = YHAT_OLSD, colour="y.hat OLSD"), linetype="solid") +
  # Quarto layer: eixo x: coluna "id" e eixo y: coluna "yD.up", linetype: traço
  geom_line(aes(x = id, y = yD.up, colour="IC y.hat"), linetype="dashed") +
  # Quinto layer: eixo x: coluna "id" e eixo y: coluna "yD.up", linetype: traço
  geom_line(aes(x = id, y = yD.low, colour="IC y.hat"), linetype="dashed") +
  # Definicoes das corres em Valores Hex
  scale_color_manual(values=c("y.hat"="#FF0000",
                              "y.hat OLS"="#000000",
                              "y.hat OLSD"="#0000FF",
                              "IC y.hat" = "#0066CC")) + 
  # Labs: Configuracoes dos titulos legendas e eixos
  labs(
    # title="Titulo",         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 


# Vamos plotar um grafico utilizando como base de dados o data.frame "yhat.plot" que acabamos de criar
# e a informacao do intervalo de confianca
ggplot(yhat.plot) + 
  # primeiro layer: 
  geom_line(aes(x = id, y = yhat, colour="y.hat"), linetype="solid") +
  # Layer de ribbom para mostrar os intervalos de confianca
  # fill: a cor de preenchimento da Ribbon
  # aplha: o nivel de transparencia da Ribbon (se for solido vai ficar na frente de tudo)
  geom_ribbon(aes(x=id, ymin = yD.low, ymax = yD.up), fill = "grey70", alpha=0.4) +
  # layer: eixo x: coluna "id" e eixo y: coluna "YHAT_OLSD" linetype: solid
  geom_line(aes(x = id, y = YHAT_OLSD, colour="y.hat OLSD"), linetype="solid") +
  # Definicoes das corres em Valores Hex
  scale_color_manual(values=c("y.hat"="#FF0000",
                              "y.hat OLSD"="#0000FF")) + 
  # Labs: Configuracoes dos titulos legendas e eixos
  labs(
    # title="Titulo",         # Titulo do grafico
    # subtitle = "Subtitulo", # Subtitulo do grafico
    # caption = "Rodape",     # Identificacao do rodape
    x = NULL,               # Rotulo do eixo X
    y = NULL,               # Rotulo do eixo y
    # tag = "Tag",            # Tag do grafico
    colour = "Legenda"      # Identificaç♠ão da legenda (para retirar colocar NULL)
  ) + 
  theme_bw()   # Tema de fundo branco simples + theme(legend.title = element_blank()) 






yhat.plot <- data.table('yhat' = rbindlist(list(data.table(sim.data$fore[, y]),
                                                data.table(yhat$y),
                                                data.table(yhat$yD))),
                        'label' = rep(c('Y', 'YHAT_OLS', 'YHAT_OLSD'), each = 100))

ggplot(yhat.plot, aes(x = rep(302:401, 3), y = yhat.plot$yhat.V1, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 

#'
#' ## **Plot - yhat_OLSD with Upper Limit  and Lower Limit **
#' 
yhat.plot1 <- data.table('yhat' = rbindlist(list(data.table(yhat$yD),
                                                data.table(yhat$yD.up),
                                                data.table(yhat$yD.low))),
                        'label' = rep(c('YHAT_OLSD', 'YHAT_OLSD_UP', 'YHAT_OLSD_LOW'),
                                      each = 100))

ggplot(yhat.plot1, aes(x = rep(302:401, 3), y = yhat.plot1$yhat.V1, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 

#'
#' ## **Plot - yhat_OLS with Upper Limit  and Lower Limit for OLSD **
#' 
yhat.plot2 <- data.table('yhat' = rbindlist(list(data.table(yhat$y),
                                                 data.table(yhat$yD.up),
                                                 data.table(yhat$yD.low))),
                         'label' = rep(c('YHAT_OLS', 'YHAT_OLSD_UP', 'YHAT_OLSD_LOW'),
                                       each = 100))

ggplot(yhat.plot2, aes(x = rep(302:401, 3), y = yhat.plot2$yhat.V1, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 



#'
#' ## **Plot - y with Upper Limit  and Lower Limit for OLSD **
#' 
yhat.plot3 <- data.table('yhat' = rbindlist(list(data.table(sim.data$fore[, y]),
                                                 data.table(yhat$yD.up),
                                                 data.table(yhat$yD.low))),
                         'label' = rep(c('Y', 'YHAT_OLSD_UP', 'YHAT_OLSD_LOW'),
                                       each = 100))

ggplot(yhat.plot3, aes(x = rep(302:401, 3), y = yhat.plot3$yhat.V1, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 




#'
#' ## ** Initialize the yhat Recursive**
#' 

yhat$y.rec <- yhat$yD.rec <- yhat$yD.recse <- rep(0, 100)

#'
#' ## **Recursive**
#' 


for (i in 1:100) {
  ols.rec <- lm(y ~ x, data = sim.data$full[102:(300 + i)])
  yhat$y.rec[i] <- predict(ols.rec, newdata = sim.data$full[301 + i])
  
  olsD.rec <- lm(y ~ D + x + D * x, data = sim.data$full[102:(300 + i)])
  yhat$yD.rec[i] <- predict(olsD.rec, newdata = sim.data$full[301 + i])
  yhat$yD.recse[i] <- sqrt(sum(olsD.rec$residuals^2) / (197 + i))
}

#'
#' ## **Plot - recursive forecasts**
#' 
yrec.plot <- data.table('yhat' = rbindlist(list(data.table(sim.data$fore[, y]),
                                                data.table(yhat$y.rec),
                                                data.table(yhat$yD.rec))),
                        'label' = rep(c('Y', 'YHAT_OLS_REC', 'YHAT_OLSD_REC'),
                                      each = 100))

#'
#' ## **Plot - recursive forecasts with dummy**
#' 
ggplot(yrec.plot, aes(x = rep(302:401, 3), y = yrec.plot$yhat.V1, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 


#'
#' ## **Plot - recursive forecasts with dummy and lower and upper limits**
#' 

yrec.plot1 <- data.table('yhat' =
                          rbindlist(list(data.table(yhat$yD.rec),
                                         data.table(yhat$yD.rec +
                                                      1.96 * yhat$yD.recse),
                                         data.table(yhat$yD.rec -
                                                      1.96 * yhat$yD.recse))),
                        'label' =
                          rep(c('YHAT_OLSD_REC', 'YHAT_OLSD_REC_UP', 'YHAT_OLSD_REC_LOW'),
                              each = 100))

ggplot(yrec.plot1, aes(x = rep(302:401, 3), y = yrec.plot1$yhat.V1, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 



#'
#' ## **RMSE & MAE**
#' 
yhat$Y <- cbind(yhat$y, yhat$yD.rec)
RMSE <- sqrt(colSums((yhat$Y - sim.data$fore[, y])^2) / 100)
MAE <- colSums(abs(yhat$Y - sim.data$fore[, y])) / 100
error.mat <- rbind(RMSE, MAE)
colnames(error.mat) <- c('Simple', 'Recursive')
error.mat


### 2.9.1 Forecasting Euro Area GDP ###
eu.gdp <- fread('ex2_misspecification_gdp.csv')
gdp.fit <- lm(y ~ ipr + su + sr, data = eu.gdp)

# Breusch Pagan Test
bptest(gdp.fit)

# White test
eu.gdp[, c('eps', 'eps2', 'ipr2', 'su2', 'sr2') :=
         list(gdp.fit$residuals, gdp.fit$residuals^2, ipr^2, su^2, sr^2)]
white.fit <- lm(eps2 ~ ipr + ipr2 + ipr * su + ipr * sr +
                  su + su2 + su * sr + sr + sr2, data = eu.gdp)
summary(white.fit)

# Durbin-Watson test
dwtest(gdp.fit)  

# Normality test & Histogram
hist(eu.gdp[, eps], xlab = '', ylab = '', main = '') 
jarque.bera.test(eu.gdp[, eps]) 

# Chow break point test
sctest(y ~ ipr + su + sr, data = eu.gdp, type = 'Chow', from = 0.1, to = 0.3)
sctest(y ~ ipr + su + sr, data = eu.gdp, type = 'Chow', from = 0.3, to = 0.7)
sctest(y ~ ipr + su + sr, data = eu.gdp, type = 'Chow', from = 0.7, to = 1)

fs <- Fstats(y ~ x, data = eu.gdp, from = 0.3, to = 1)
fs <- Fstats(y ~ x, data = eu.gdp, from = 0.7, to = 1)

# Bai-Perron test
breakpoints(y ~ ipr + su + sr, data = eu.gdp, h = 0.15)

# Recursive estimation
gdp.rr <- recresid(y ~ ipr + su + sr, data = eu.gdp$full)
plot(gdp.rr, type = 'l')

# Dummy variable
gdpD.fit <- list()
gdpD.formula <- c('y ~ ipr + su + sr + Dea', 'y ~ ipr + su + sr + D2000s',
                  'y ~ ipr + su + sr + Dea + D2000s')
for (model in 1:3) {
  gdpD.fit[[model]] <- lm(gdpD.formula[model], data = eu.gdp)
  print(summary(gdpD.fit[[model]]))
}

# Forecasting
gdp.hat <- list()
gdp.fit <- lm(y ~ ipr + su + sr, data = eu.gdp[1:60]) # Model 2 - no dummy
gdp.hat$ghat <- predict(gdp.fit, newdata = eu.gdp[61:70]) 

gdp.fit <- lm(y ~ ipr + su + sr + Dea + D2000s, data = eu.gdp[1:60])
gdp.hat$ghat3 <- predict(gdp.fit, newdata = eu.gdp[61:70]) # Model 2.3 - dummy

gdp.plot <- data.table('yhat' = rbindlist(list(data.table(eu.gdp[61:70, y]),
                                               data.table(gdp.hat$ghat),
                                               data.table(gdp.hat$ghat3))),
                       'label' = rep(c('Y', 'YFOREG2_NEW', 'YFOREG2_3'),
                                     each = 10))

ggplot(gdp.plot, aes(x = rep(1:10, 3), y = yhat, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 

# RMSE & MAE
gdp.hat$Y <- cbind(gdp.hat$ghat, gdp.hat$ghat3)
RMSE <- sqrt(colSums((gdp.hat$Y - eu.gdp[61:70, y])^2) / 10)
MAE <- colSums(abs(gdp.hat$Y - eu.gdp[61:70, y])) / 10
error.mat <- rbind(RMSE, MAE)
colnames(error.mat) <- c('Model 2', 'Model 2.3')
print(xtable(error.mat), include.rownames = T, include.colnames = T)

### 2.9.2 Forecasting US GDP ###
us.gdp <- fread('ex2_misspecification_gdp_us.csv')
gdp.fit <- lm(y ~ ipr + su + sr, data = us.gdp)

# Breusch Pagan Test
bptest(gdp.fit)

# White test
us.gdp[, c('eps', 'eps2', 'ipr2', 'su2', 'sr2') :=
         list(gdp.fit$residuals, gdp.fit$residuals^2, ipr^2, su^2, sr^2)]
white.fit <- lm(eps2 ~ ipr + ipr2 + ipr * su + ipr * sr +
                  su + su2 + su * sr + sr + sr2, data = us.gdp)
summary(white.fit)

# Durbin-Watson test
dwtest(gdp.fit)  

# Normality test & Histogram
hist(us.gdp[, eps], xlab = '', ylab = '', main = '') 
jarque.bera.test(us.gdp[, eps]) 

# Chow break point test
sctest(y ~ ipr + su + sr, data = us.gdp, type = 'Chow', from = 0.55, to = 0.6)
sctest(y ~ ipr + su + sr, data = us.gdp, type = 'Chow', from = 0.75, to = 0.8)

fs <- Fstats(y ~ x, data = us.gdp, from = 0.55, to = 1)
fs <- Fstats(y ~ x, data = us.gdp, from = 0.75, to = 1)

# Bai-Perron test
breakpoints(y ~ ipr + su + sr, data = eu.gdp, h = 0.15)

# Recursive estimation
gdp.rr <- recresid(y ~ ipr + su + sr, data = us.gdp)
plot(gdp.rr, type = 'l')

# Dummy variable
gdpD.fit <- list()
gdpD.formula <- c('y ~ ipr + su + sr + Dfincris', 'y ~ ipr + su + sr + D2000s',
                  'y ~ ipr + su + sr + Dfincris + D2000s')
for (model in 1:3) {
  gdpD.fit[[model]] <- lm(gdpD.formula[model], data = us.gdp)
}
summary(gdpD.fit[[1]])
summary(gdpD.fit[[2]])
summary(gdpD.fit[[3]])

## Forecasting
gdp.hat <- list()

# Model 2 - no dummy
gdp.fit <- lm(y ~ ipr + su + sr, data = us.gdp[1:104]) 
gdp.hat$ghat <- predict(gdp.fit, newdata = us.gdp[105:114]) 

# Model 2.3 - dummy
gdp.fit <- lm(y ~ ipr + su + sr + Dfincris + D2000s, data = us.gdp[1:104])
gdp.hat$ghat3 <- predict(gdp.fit, newdata = us.gdp[105:114]) 

gdp.plot <- data.table('yhat' = rbindlist(list(data.table(us.gdp[105:114, y]),
                                               data.table(gdp.hat$ghat),
                                               data.table(gdp.hat$ghat3))),
                       'label' = rep(c('Y', 'YFOREG2_3', 'YRFOREG2_3'),
                                     each = 10))

ggplot(gdp.plot, aes(x = rep(1:10, 3), y = yhat, linetype = label)) +
  geom_line() + xlab('') + ylab('') + theme(legend.title = element_blank()) 

# Recursive
for (i in 1:10) {
  ols.rec <- lm(y ~ ipr + su + sr, data = us.gdp[1:(103 + i)])
  gdp.hat$rec[i] <- predict(ols.rec, newdata = us.gdp[104 + i])
  
  olsD.rec <- lm(y ~ ipr + su + sr + Dfincris + D2000s,
                 data = us.gdp[1:(103 + i)])
  gdp.hat$rec3[i] <- predict(olsD.rec, newdata = us.gdp[104 + i])
}

# RMSE & MAE
gdp.hat$Y <- cbind(gdp.hat$ghat, gdp.hat$ghat3) # simple
RMSE <- sqrt(colSums((gdp.hat$Y - us.gdp[105:114, y])^2) / 10)
MAE <- colSums(abs(gdp.hat$Y - us.gdp[105:114, y])) / 10
error.mat <- rbind(RMSE, MAE)

# Recursive RMSE & MAE
gdp.hat$Yrec <- cbind(gdp.hat$rec, gdp.hat$rec3) # recursive
RMSE <- sqrt(colSums((gdp.hat$Yrec - us.gdp[105:114, y])^2) / 10)
MAE <- colSums(abs(gdp.hat$Yrec - us.gdp[105:114, y])) / 10
error.mat <- rbind(error.mat, RMSE, MAE)
rownames(error.mat) <- c('Simple RMSE', 'Simple MAE',
                         'Recursive RMSE', 'Recursive MAE')
colnames(error.mat) <- c('Model 2', 'Model 2.3')
print(xtable(error.mat), include.rownames = T, include.colnames = T)

### 2.9.3 Default Risk ###
default.risk <- fread('default_risk.csv')
default.risk[, Date := as.Date(Date, format = '%m/%d/%Y')] 
default.risk[, OAS := OAS[2:216]]
default.risk <- default.risk[1:215]

# Dummy and interaction term
default.risk[Date >= '2008-01-01' & Date < '2010-01-01', D := 1]
default.risk[Date < '2008-01-01' | Date >= '2010-01-01', D:= 0]
default.risk[, c('VIX.D', 'SENT.D', 'PMI.D', 'sp500.D') :=
               list(VIX * D, SENT * D, PMI * D, sp500 * D)]

# Dummy
default.D <- list('M1' = 'OAS ~ VIX + D', 'M2' = 'OAS ~ SENT + D',
                  'M3' = 'OAS ~ PMI + D', 'M4' = 'OAS ~ sp500 + D')

# Interaction
default.I <- list('M1' = 'OAS ~ VIX + D + VIX.D',
                  'M2' = 'OAS ~ SENT + D + SENT.D',
                  'M3' = 'OAS ~ PMI + D + PMI.D',
                  'M4' = 'OAS ~ sp500 + D + sp500.D')

for (m in c('M1', 'M2', 'M3', 'M4')) {
  fit.D <- lm(default.D[[m]], data = default.risk)
  print(summary(fit.D))
  print(coeftest(fit.D, vcov = NeweyWest(fit.D, lag = 12)))
}

for (m in c('M1', 'M2', 'M3', 'M4')) {
  fit.I <- lm(default.I[[m]], data = default.risk)
  print(summary(fit.I))
  print(coeftest(fit.I, vcov = NeweyWest(fit.I, lag = 12)))
}