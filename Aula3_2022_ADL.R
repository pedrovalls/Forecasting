

#'
#' title: "Cap 3 Dynamic Linear Model"
#' author: 
#'  - "Pedro Valls"
#' date: "28 de outubro de 2020"
#' ---
#+ warning = FALSE, message = FALSE


rm(list = ls())
setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time Series_Econometrics_2020/applied_economics_forecasting/appliedeconomicforecasting_R/cap3")


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

load_package("nlme")
load_package("data.table")
load_package("ggplot2")
load_package("lmtest")
load_package("stargazer")
load_package("tseries")
load_package("xtable")
load_package("forecast")
load_package("Metrics")


library(data.table)
library(ggplot2)
library(lmtest)
library(Metrics)
library(stargazer)
library(xtable)

#'
#' ## **3.6 Simulated Data**
#' 
sim.data <- fread('simulated_datac3.csv')
sim.est <- sim.data[102:301]

ic.table <- data.table()
formula.list <- paste0('y0 ~ D + x0 + D * x0 ',
                       c('+ y1', '+ y1 + y2', '+ x1', '+ x1 + y1',
                         '+ x1 + y1 + y2', '+ x1 + x2', '+ x1 + x2 + y1',
                         '+ x1 + x2 + y1 + y2'))

#'
#' ## **Identify ARDL(1, 1) as the best model**
#' 
#' 
for (f in 1:8) {
  ardl.fit <- lm(formula.list[f], data = sim.est)
  ic <- data.table(cbind(AIC(ardl.fit)/200, BIC(ardl.fit)/200))
  ic.table <- rbindlist(list(ic.table, ic))
}
ic.table
best_aic <- matrix(data=ic.table[1,1], nrow=8, ncol=1)
best_aic[1]=ic.table[1,1]
for (i in 2:8) {
  if(best_aic[i-1] >= ic.table[i,1]){best_aic[i]=ic.table[i,1]}
  else {best_aic[i]=best_aic[i-1]}
}

best_bic <- matrix(data=ic.table[1,2], nrow=8, ncol=1)
best_bic[1]=ic.table[1,2]

for (i in 2:8) {
  if(best_bic[i-1] >= ic.table[i,2]){best_bic[i]=ic.table[i,2]}
  else {best_bic[i]=best_bic[i-1]}  
}
best_aic
best_bic

ind_aic=1
for (i in 1:8){
  if(best_aic[8]== ic.table[i,1]){ind_aic=i}
  else{}
}

#'
#' ## **Best model using AIC is model 7**
#' 

ind_bic=1
for (i in 1:8){
  if(best_bic[8]== ic.table[i,2]){ind_bic=i}
  else{}
}

#'
#' ## **Best model using AIC is model 4**
#' 


#'
#' ## ** The model 4 will be used **
#' 

ardl.fit <- lm(formula.list[4], data = sim.est)
summary(ardl.fit)
AIC(ardl.fit)/200
BIC(ardl.fit)/200




#'
#' ##** Different forms of dynamic models**
#' 
#' 
# d1ipr=c(NA,diff(eu.gdp$ipr,differences=1)[1:length(eu.gdp$ipr)-1])
# d1su=c(NA,diff(eu.gdp$su,differences=1)[1:length(eu.gdp$su)-1])
# d1sr=c(NA,diff(eu.gdp$sr,differences=1)[1:length(eu.gdp$sr)-1])
sim.data$d1x0=c(NA,diff(sim.data$x0,differences=1)[1:length(sim.data$x0)-1])



#'
#' ## **ADL(1,1) irrestricted**
#' 
model_ardls = lm(y0 ~ y1 + x0 + x1, data=sim.data[2:501] )
summary(model_ardls)
aic_m_ardls = AIC(model_ardls)/model_ardls$df.residual
bic_m_ardls = BIC(model_ardls)/model_ardls$df.residual

#'
#' ## **Static Regression**
#' 
model_stat_regs = lm(y0 ~ x0, data=sim.data[2:501] )
summary(model_stat_regs)
aic_stat_regs = AIC(model_stat_regs)/model_stat_regs$df.residual
bic_stat_regs = BIC(model_stat_regs)/model_stat_regs$df.residual

#'
#' ## **AR(1)**
#' 
model_ar1s = lm(y0 ~ y1 , data=sim.data[2:501] )
summary(model_ar1s)
aic_ar1s = AIC(model_ar1s)/model_ar1s$df.residual
bic_ar1s = BIC(model_ar1s)/model_ar1s$df.residual

#' 
#' ## ** Differenced Model **
#' 

model_Difs = lm(y0 ~ offset(1*y1)+ d1x0, data=sim.data[2:501])
summary(model_Difs)
aic_Difs = AIC(model_Difs)/model_Difs$df.residual
bic_Difs = BIC(model_Difs)/model_Difs$df.residual


#' 
#' ## ** Leading Indicator **
#' 

model_lis = lm(y0 ~ y1+x1, data=sim.data[2:501])
summary(model_lis)
aic_lis = AIC(model_lis)/model_lis$df.residual
bic_lis = BIC(model_lis)/model_lis$df.residual


#' 
#' ## ** Distributed Lags **
#' 

model_dls = lm(y0 ~ x0+x1,  data=sim.data[2:501])
summary(model_dls)
aic_dls = AIC(model_dls)/model_dls$df.residual
bic_dls = BIC(model_dls)/model_dls$df.residual


#' 
#' ## ** Partial Adjustment **
#' 

model_padjs = lm(y0 ~ y1+x0, data=sim.data[2:501])
summary(model_padjs)
aic_padjs = AIC(model_padjs)/model_padjs$df.residual
bic_padjs = BIC(model_padjs)/model_padjs$df.residual


#' 
#' ## ** Dead Start **
#' 

model_dss = lm(y0 ~ y1+x1, data=sim.data[2:501])
summary(model_dss)
aic_dss = AIC(model_dss)/model_dss$df.residual
bic_dss = BIC(model_dss)/model_dss$df.residual

#' 
#' ## ** Static Regression with AR(1) error**
#' 
model_RegAR1s = gls(y0 ~ x0, correlation =corARMA(p=1,q=0), data=sim.data[2:501])
summary(model_RegAR1s)
aic_RegAR1s = AIC(model_RegAR1s)/model_RegAR1s$dims$N
bic_RegAR1s = BIC(model_RegAR1s)/model_RegAR1s$dims$N


#' 
#' ## ** EqCM**
RegStats=lm(y0 ~ x0, data=sim.data[1:501])
RegStats$residuals
sim.data$RegStats_res_1=c(NA,RegStats$residuals[1:length(RegStats$residuals)-1])

model_EqCMs = lm(y0 ~ offset(1*y1)+ d1x0 + RegStats_res_1, data=sim.data[2:501])
summary(model_EqCMs)
aic_EqCMs = AIC(model_EqCMs)/model_EqCMs$df.residual
bic_EqCMs = BIC(model_EqCMs)/model_EqCMs$df.residual



AIC_Modelss <- cbind(aic_m_ardls, aic_stat_regs , aic_ar1s, aic_Difs, aic_lis,
                    aic_dls, aic_padjs, aic_dss, aic_RegAR1s, aic_EqCMs)

BIC_Modelss <- cbind(bic_m_ardls, bic_stat_regs , bic_ar1s, bic_Difs, bic_lis,
                    bic_dls, bic_padjs, bic_dss, bic_RegAR1s, bic_EqCMs)

errordms.mat <- rbind(AIC_Modelss, BIC_Modelss)
colnames(errordms.mat) <- c('ADL(1,1,1,1) irrestricted','Static Regression', 'AR(1)', 'Differenced Model',
                           'Leading Indicator', 'Distributed Lags', 'Partial Adjustment',
                           'Dead Start', 'Static Regression with AR(1) error', 'EqCM')
rownames(errordms.mat) <- c('AIC', 'BIC')
print(xtable(errordms.mat), include.rownames = T, include.colnames = T)

errordms.mat

#'
#' ## ** Best model ADL(1,1) irrestricted**
#' 
#'
#' ## ** Test if the long run solution for EqCM is well defined**
#' 
summary(model_ardls)

linearHypothesis(model_ardls, c("y1 = 1","x0-x1=0"), test = "Chisq")
linearHypothesis(model_ardls, c("y1 = 1","x0-x1=0"), test = "F")


#'
#' ## ** Test omitted variables**
#' 
#'
#' ## ** Add Dummy for constant and for x0**
#' 

model_ardlsD = lm(y0 ~ y1 + x0 + x1+D+I(D*x0), data=sim.data[2:501] )
summary(model_ardlsD)
aic_m_ardlsD = AIC(model_ardlsD)/model_ardlsD$df.residual
bic_m_ardlsD = BIC(model_ardlsD)/model_ardlsD$df.residual


RSSADL= sum(resid(model_ardls)^2)
RSSADL_D= sum(resid(model_ardlsD)^2)

TF_O=length(resid(model_ardlsD))

F_Omitted = (RSSADL/RSSADL_D)*((TF_O-6)/(TF_O-4))

#'
#' ## **p-value**
#' 
p_value_Ommitted =  1-pf(F_Omitted,TF_O-4,TF_O-6)
