#'
#' title: "Cap 8  Forecasting with BVAR Models"
#' author: 
#'  - "Pedro Valls"
#' date: "25 of november 2022"
#' ---
#+ warning = FALSE, message = FALSE




#'
#' ## Clear Workspace
#' 
rm(list = ls())
#'
#' ## Change working drectory
#' 
setwd("C:/Users/Pedro/Dropbox/Special_Topics_in_Time_Series_Econometrics_2022/applied_economics_forecasting/appliedeconomicforecasting_R/cap8")


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
#load_package('BMR')
load_package('data.table')
load_package('ggplot2')
load_package('Metrics')
load_package('xtable')
load_package('stargazer')
load_package("BVAR")

library(devtools)
#install_github("kthohr/BMR", force = TRUE)
#install.packages("BVAR")

library(BMR)
library(data.table)
library(ggplot2)
library(Metrics)
library(vars)
library(BVAR)
library(xtable)
library(stargazer)




### 8.4 Simulated Data ###
sim.data <- fread('bvar_simulated_ch8_sec4.csv')
sim.data <- sim.data[101:600, .(x, y)]

sim.est <- sim.fore <- list()

# VAR(1)
sim.est$var1 <- VAR(sim.data[1:400], p = 1)

#stargazer(sim.est$var1, type="latex")


sim.fore$var1 <- predict(sim.est$var1, n.ahead = 100)
sim.fore$var1 <- data.table('x' = sim.fore$var1$fcst$x[, 1],
                            'y' = sim.fore$var1$fcst$y[, 1])

# VAR(4)
sim.est$var4 <- VAR(sim.data[1:400], p = 4)
sim.fore$var4 <- predict(sim.est$var4, n.ahead = 100)
sim.fore$var4 <- data.table('x' = sim.fore$var4$fcst$x[, 1],
                            'y' = sim.fore$var4$fcst$y[, 1])


#------------------------------------------------------------------------------------------
# BVAR(1) - Minnesota prior
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
# prior setup in BVAR 
#------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------
# minnesota prior setup
# $\lambda$ has a Gamma hyperprior and is handed upper and lower bounds
# for its Gaussian proposal distribution in the Metropolis-Hasting step
# $\alpha$ parameter is r-tteated as fixed via the mode argument.
# The prior variance on the cosntant term of the model (var) 
# is dealt a large value, for a diffuse prior.
# The prior mean for the var-cov matrix $\Sigma$, is $\Phi$ is left to be 
# set automatically - i,e., to the square root of the innovation
# variance,  after fitting AR(p) models to each of the variables.
#------------------------------------------------------------------------------------------
mn <- bv_minnesota( lambda = bv_lambda(mode=0.2, sd =0.4, min = 0.0001, max =5),
                   alpha = bv_alpha(mode=2), var = 1e07)
#------------------------------------------------------------------------------------------
# sum-of-coefficients (Doan et al 1984) and single-unit-roots priors (Sims and Zha 1998)
# are also considered.
# The hyperprior of their key hyperparameters are assigned Gamma distributions,
# with specification working in the same way as for $\lambda$.
#------------------------------------------------------------------------------------------

soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


#------------------------------------------------------------------------------------------
# Now define the bv_priors()
# Via hyper we choose which hyperparameters should ve treated hierarchically
# The default is "auto" which includes $\lambda$ and the key hyperparameters
# of all provided dummy-observation priors. 
# This is equivalent to providing the character vector c("lambda", "soc","sur")
# $\alpha$ are treated as fixed and set equal to their mode. 
#------------------------------------------------------------------------------------------

priors <- bv_priors(hyper="auto", mn = mn, soc = soc, sur = sur)


#------------------------------------------------------------------------------------------
# Adjust the MH algorithm via bv_metropolis()
# The primary argument is scale_hess, which allows scaling the inverse Hessian, 
# which is used as VCOV matrix of the Gaussian proposal distribution
# The scalling can use adjust_aa = TRUE which enables automatic scale adjustment.
# This is done in the initial share of the burn-in period via adjust_burn
# Automatic adjustment is performed iteratively by acc_change percent, 
# until an acceptance rate between acc_lower and acc_upper is reached.
#------------------------------------------------------------------------------------------

mh <- bv_metropolis(scale_hess=c(0.05, 0.0001, 0.0001),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)

#------------------------------------------------------------------------------------------
# Now bvar() is the BAVR specification
# \textbf{lags} is the lag order of the VAR
# The total mumber of iterations with \textbf{n_draw}, the number of initial iterations
# to discard with \textbf{n_burns}, and a fraction of drawns to be store 
# via \textbf{n_thin}.
# When estimating the model, \textbf{verbose - TRUE} prompts printing of intermediate results.
#------------------------------------------------------------------------------------------


sim.est$bvar1 <- bvar(sim.data[1:400], lags = 1, n_draw = 15000, n_burn = 5000, n_thin = 1,
                      priors = priors, mh = mh, verbose = TRUE)

#------------------------------------------------------------------------------------------
# print the output of the estimation of the BAVR
#------------------------------------------------------------------------------------------

print(sim.est$bvar1)

#------------------------------------------------------------------------------------------
# We use \textbf{plot()} to assess convergence of the MCMC algorithm
#------------------------------------------------------------------------------------------


plot(sim.est$bvar1)


#------------------------------------------------------------------------------------------
# plot the posterior density of the first lag of each variable in each equation
#------------------------------------------------------------------------------------------
plot(sim.est$bvar1, type = "dens",
     vars_response = "x", vars_impulse = "x")

plot(sim.est$bvar1, type = "dens",
     vars_response = "y", vars_impulse = "y")
#------------------------------------------------------------------------------------------
# fitted and residuals values can be reached using \textbf{fitted()} command
# with the argument \textbf{typ1="mean"} that gives the mean of the posterior draws.
# Could compute using the command \textbf{conf_bands()} confidence bands
#------------------------------------------------------------------------------------------

fitted(sim.est$bvar1, conf_bands = 0.10)

#------------------------------------------------------------------------------------------
# \textbf{plot(residuals())} plot the residuals 
#------------------------------------------------------------------------------------------

plot(residuals(sim.est$bvar1, type = "mean"), vars = c("y","x"))

#------------------------------------------------------------------------------------------
# Can construct IRF 
#------------------------------------------------------------------------------------------

sim_opt_irf <- bv_irf(horizon = 20, identification = TRUE)
irf(sim.est$bvar1) <- irf(sim.est$bvar1, sim_opt_irf, conf_bands =c(0.05,0.16))

plot(irf(sim.est$bvar1), area = TRUE,
     var_impulse = c("y","x"))

#------------------------------------------------------------------------------------------
# Can construct forecast using \textbf{predict()}  
#------------------------------------------------------------------------------------------

sim.fore$bvar1 <- predict(sim.est$bvar1, horizon = 100, conf_bands = c(0.05, 0.16))
#------------------------------------------------------------------------------------------
# retain only the median forecast  
#------------------------------------------------------------------------------------------
sim.fore$bvar1 <- data.table(sim.fore$bvar1$quants[3,1:100,1:2])
setnames(sim.fore$bvar1, c('x', 'y'))


#------------------------------------------------------------------------------------------
# BVAR(4)
#------------------------------------------------------------------------------------------

sim.est$bvar4 <- bvar(sim.data[1:400], lags = 4, n_draw = 15000, n_burn = 5000, n_thin = 1,
                      priors = priors, mh = mh, verbose = TRUE)

#------------------------------------------------------------------------------------------
# print the output of the estimation of the BAVR
#------------------------------------------------------------------------------------------

print(sim.est$bvar4)

#------------------------------------------------------------------------------------------
# We use \textbf{plot()} to assess convergence of the MCMC algorithm
#------------------------------------------------------------------------------------------


plot(sim.est$bvar4)


#------------------------------------------------------------------------------------------
# plot the posterior density of the first lag of each variable in each equation
#------------------------------------------------------------------------------------------
plot(sim.est$bvar4, type = "dens",
     vars_response = "x", vars_impulse = "x")

plot(sim.est$bvar4, type = "dens",
     vars_response = "y", vars_impulse = "y")
#------------------------------------------------------------------------------------------
# fitted and residuals values can be reached using \textbf{fitted()} command
# with the argument \textbf{typ1="mean"} that gives the mean of the posterior draws.
# Could compute using the command \textbf{conf_bands()} confidence bands
#------------------------------------------------------------------------------------------

fitted(sim.est$bvar4, conf_bands = 0.10)

#------------------------------------------------------------------------------------------
# \textbf{plot(residuals())} plot the residuals 
#------------------------------------------------------------------------------------------

plot(residuals(sim.est$bvar4, type = "mean"), vars = c("y","x"))

#------------------------------------------------------------------------------------------
# Can construct IRF 
#------------------------------------------------------------------------------------------

sim_opt_irf <- bv_irf(horizon = 20, identification = TRUE)
irf(sim.est$bvar4) <- irf(sim.est$bvar4, sim_opt_irf, conf_bands =c(0.05,0.16))

plot(irf(sim.est$bvar4), area = TRUE,
     var_impulse = c("y","x"))

#------------------------------------------------------------------------------------------
# Can construct forecast using \textbf{predict()}  
#------------------------------------------------------------------------------------------

sim.fore$bvar4 <- predict(sim.est$bvar4, horizon = 100, conf_bands = c(0.05, 0.16))
#------------------------------------------------------------------------------------------
# retain only the median forecast  
#------------------------------------------------------------------------------------------
sim.fore$bvar4 <- data.table(sim.fore$bvar4$quants[3,1:100,1:2])
setnames(sim.fore$bvar4, c('x', 'y'))












#sim.est$bvar4 <- BVARM(sim.data[1:400], c(0, 0), p = 4, HP1 = 0.2, HP2 = 0.99, HP3 = 1)
#sim.fore$bvar4 <- forecast(sim.est$bvar4, periods = 100)
#sim.fore$bvar4 <- data.table(sim.fore$bvar4$MeanForecast)
#setnames(sim.fore$bvar4, c('x', 'y'))
#------------------------------------------------------------------------------------------
# Evaluation
#------------------------------------------------------------------------------------------

sim.error <- list('x' = data.table('Model' = c('var1', 'var4',
                                               'bvar1', 'bvar4')),
                  'y' = data.table('Model' = c('var1', 'var4',
                                               'bvar1', 'bvar4')))
for (model in c('var1', 'var4', 'bvar1', 'bvar4')) {
  sim.error$x[Model == model,
              c('RMSE', 'MAE') :=list(rmse(sim.data[401:500, x],
                                           sim.fore[[model]][, x]),
                                       mae(sim.data[401:500, x],
                                           sim.fore[[model]][, x]))]
  sim.error$y[Model == model,
              c('RMSE', 'MAE') := list(rmse(sim.data[401:500, y],
                                            sim.fore[[model]][, y]),
                                       mae(sim.data[401:500, y],
                                           sim.fore[[model]][, y]))]
}


print(xtable(sim.error$x, type = "latex", digits = 5), file = "filenamex.tex")
print(xtable(sim.error$y, type = "latex", digits = 5), file = "filenamey.tex")

