#'
#' title: "Cap 7  Spurious Regression"
#' author: 
#'  - "Pedro Valls"
#' date: "10 of november 2022"
#' ---
#+ warning = FALSE, message = FALSE


# Load package using a function load_package-----------------------------------------------------------------
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


#'
#'  ## **Fix the random seed**
#'  
set.seed(123456)
#'
#' ##**matrix A of coeff of VAR(1)**
#' 
p1 = matrix(c(1.0,0.0,
              0.0,1.0),2)
#'
#' ##**matrix SIGMA var-cov of disturbances**
#' 
sig=matrix(c(1.0, 0.0,
             0.0,1.0),2)
#'
#' ## simulate a VAR(1)**
#' 
m1=VARMAsim(2000,arlags=c(1),malags=NULL,phi=p1,theta=NULL,sigma=sig)
#'
#' ## **rename series to $y_{t}$**
yt=m1$series

#'
#' ## **Transform $y_{t}$ in time series**
#' 

yt_ts=ts(yt)

#'
#' ## *rename the first component of yt to y1 and the second to y2**
#' 
y1=yt_ts[,1]
y2=yt_ts[,2]
#'
#' ##**plot the two series**
#' 
par(mfrow=c(1,2))
plot(y1,type='l', col='blue', main = 'First RW Component')
plot(y2,type='l', col='red', main = 'Second RW Component')

#'
#' ## **scatter plot**
#' 

par(mfrow=c(1,1))
plot(y1,y2,pch='8', col='black', main = 'Scatter Plot of two RW')

par(mfrow=c(1,1))

#'
#' ## **Regression in levels**
#' 
regressao <- lm(y2 ~y1)
summary(regressao)
plot(y1,y2,pch='8', col='black', main = 'Scatter Plot of two RW')
abline(coef(regressao), col='red')

legend('topleft', legend=c('y2=26.97938 + 0.14072*y1'), col=c('red'), pch=15)



#'
#' ## **correlation between the two series**
#' 

cor(y1,y2)


#'
#' ## **define the lagged series**
#' 


y1_1=c(NA,y1[1:length(y1)-1])

#'
#' ##  **defined the difference series and called dy1**
#' 


dy1=y1-y1_1


#'
#' ## **define the lagged series**
#' 


y2_1=c(NA,y2[1:length(y2)-1])

#'
#' ## **defined the difference series and called dy2**
#' 


dy2=y2-y2_1



#'
#' ## **plot the first difference of the two series - dy1 and dy2**
#' 


par(mfrow=c(1,2))
plot(dy1[2:2000],type='l', ylab='dy1', col='blue', main = 'First Difference of y1')
plot(dy2[2:2000],type='l', ylab = 'dy2' ,col='red', main = 'First Difference of y2')

#'
#' ## **scatter plot**
#' 


par(mfrow=c(1,1))
plot(dy1[2:2000],dy2[2:2000],xlab='dy1', ylab='dy2' ,pch='8', col='black', main = 'Scatter Plot of dy1 and dy2')

#'
#' ## **Regression in first difference**
#' 
regressaod <- lm(dy2[2:2000] ~ dy1[2:2000])
summary(regressaod)
plot(dy1[2:2000],dy2[2:2000],xlab='dy1', ylab='dy2' ,pch='8', col='black', main = 'Scatter Plot of dy1 and dy2')
abline(coef(regressaod), col='red')

legend('topleft', legend=c('dy2=-0.0001568 + 0.013444*dy1'), col=c('red'), pch=15)




#'
#' ## **regression in levels**
#' 

eq1_level<- lm(y2 ~ y1)

#'
#' ## **summary of the regression in levels y2 = a +b*y1**
#' 


summary.lm(eq1_level)



#'
#' ## **regression in first difference **
#' 

eq1_diff<- lm(dy2 ~ dy1)

#'
#' ## **summary of the regression in difference dy2 = a +b*dy1**
#' 


summary.lm(eq1_diff)



