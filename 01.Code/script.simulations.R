
##############################################################################|
#
# Stat 520C - Final Project
# - Author: Thu Nguyen
# - Topic:  Bayesian Model Evaluating & Comparing
#
##############################################################################|



# ============================================================================!
# 0) Libraries ----------------------------------------------------------------
# ----------------------------------------------------------------------------!
libs <- c('tidyverse', 'ggplot2', 'geoR', 'MASS')
lapply(libs, require, character.only = TRUE)
rm(libs)






# ============================================================================!
# 1) Helper fns ---------------------------------------------------------------
# ----------------------------------------------------------------------------!

# Generate random data
source('01.Code/fn.DGP.R')
# Fit model Bayesian way w/ uniform prior
source('01.Code/fn.fit.model.LR.R')
# Compute log Predictive Density by Simulation
source('01.Code/fn.log.Pred.Dens.Sim.R')
# Multiple functions to compute AIC, DIC, WAIC, LOO.CV
source('01.Code/fn.Information.Criteria.R')




# ============================================================================!
# 1) Data Simulation ----------------------------------------------------------
# ----------------------------------------------------------------------------!
set.seed(520)
betas <- list(
  b0 = .5,
  b1 =  4,
  b2 =  0
)
dgp <- fn.dgp(n = 15, betas, sigma.squared = 1.5)
X <- dgp$X
Y <- dgp$Y
n <- dgp$n
J <- dgp$J
rm(betas)





# ============================================================================!
# 2) Fit Model ----------------------------------------------------------------
# ----------------------------------------------------------------------------!

# Under 4 scenarios:
# 1) Intercept + X1 + X2
# 2) Intercept + X1
# 3)             X1 + X2
# 4)             X1
(mdl.1.Int.X12 <- fn.fit.model.LR(    dgp$X,        dgp$Y))
(mdl.2.Int.X1  <- fn.fit.model.LR(t(t(dgp$X[,-3])), dgp$Y))
(mdl.3.X12     <- fn.fit.model.LR(t(t(dgp$X[,-1])), dgp$Y))
(mdl.4.X1      <- fn.fit.model.LR(t(t(dgp$X[, 2])), dgp$Y))






# ============================================================================!
# 3) Log Predictive Density ---------------------------------------------------
# ----------------------------------------------------------------------------!

set.seed(520)
# Simulate log Predictive Density
results.log.Pred.Dens <- list(
  log.Pred.Dens.1 = fn.log.Pred.Dens.Sim(        X,        Y, n, mdl.list = mdl.1.Int.X12),
  log.Pred.Dens.2 = fn.log.Pred.Dens.Sim(t(t(dgp$X[,-3])), Y, n, mdl.list = mdl.2.Int.X1),
  log.Pred.Dens.3 = fn.log.Pred.Dens.Sim(t(t(dgp$X[,-1])), Y, n, mdl.list = mdl.3.X12),
  log.Pred.Dens.4 = fn.log.Pred.Dens.Sim(t(t(dgp$X[, 2])), Y, n, mdl.list = mdl.4.X1)  
)

## Histograms -----------------------------------
par(mfrow = c(2,2))
hist(results.log.Pred.Dens[[1]], 
     breaks = seq(floor(min(results.log.Pred.Dens[[1]])), ceiling(max(results.log.Pred.Dens[[1]])), .25),
     xlab = expression(log ~ p ~ '(' ~ Y ~ '|' ~ theta ~ ')'),
     main = expression('Intercept +' ~ X[1] ~ '+' ~ X[2]))
hist(results.log.Pred.Dens[[2]], 
     breaks = seq(floor(min(results.log.Pred.Dens[[2]])), ceiling(max(results.log.Pred.Dens[[2]])), .25),
     xlab = expression(log ~ p ~ '(' ~ Y ~ '|' ~ theta ~ ')'),
     main = expression('Intercept +' ~ X[1]))
hist(results.log.Pred.Dens[[3]], 
     breaks = seq(floor(min(results.log.Pred.Dens[[3]])), ceiling(max(results.log.Pred.Dens[[3]])), .25),
     xlab = expression(log ~ p ~ '(' ~ Y ~ '|' ~ theta ~ ')'),
     main = expression(X[1] ~ '+' ~ X[2]))
hist(results.log.Pred.Dens[[4]], 
     breaks = seq(floor(min(results.log.Pred.Dens[[4]])), ceiling(max(results.log.Pred.Dens[[4]])), .25),
     xlab = expression(log ~ p ~ '(' ~ Y ~ '|' ~ theta ~ ')'),
     main = expression(X[1]))






# ============================================================================!
# 4) Information Criteria -----------------------------------------------------
# ----------------------------------------------------------------------------!



## 4.1) AIC -------------------------------------
set.seed(520)
(results.AIC <- list(
  mdl.1.Int.X12.AIC = fn.AIC(    X,        Y, mdl.1.Int.X12),
  mdl.2.Int.X1.AIC  = fn.AIC(t(t(X[,-3])), Y, mdl.2.Int.X1),
  mdl.3.X12.AIC     = fn.AIC(t(t(X[,-1])), Y, mdl.3.X12),
  mdl.4.X1.AIC      = fn.AIC(t(t(X[, 2])), Y, mdl.4.X1)
))


## 4.2) DIC -------------------------------------
set.seed(520)
(results.DIC <- list(
  mdl.1.Int.X12.DIC = fn.DIC(    X,        Y, mdl.1.Int.X12),
  mdl.2.Int.X1.DIC  = fn.DIC(t(t(X[,-3])), Y, mdl.2.Int.X1),
  mdl.3.X12.DIC     = fn.DIC(t(t(X[,-1])), Y, mdl.3.X12),
  mdl.4.X1.DIC      = fn.DIC(t(t(X[, 2])), Y, mdl.4.X1)
))



## 4.3) WAIC ------------------------------------
set.seed(520)
(results.WAIC <- list(
  mdl.1.Int.X12.WAIC = fn.WAIC(    X,        Y, mdl.1.Int.X12),
  mdl.2.Int.X1.WAIC  = fn.WAIC(t(t(X[,-3])), Y, mdl.2.Int.X1),
  mdl.3.X12.WAIC     = fn.WAIC(t(t(X[,-1])), Y, mdl.3.X12),
  mdl.4.X1.WAIC      = fn.WAIC(t(t(X[, 2])), Y, mdl.4.X1)
))



## 4.4) LOO.CV ----------------------------------
set.seed(520)
(results.LOO.CV <- list(
  mdl.1.Int.X12.CV = fn.LOO.CV(    X,        Y),
  mdl.2.Int.X1.CV  = fn.LOO.CV(t(t(X[,-3])), Y),
  mdl.3.X12.CV     = fn.LOO.CV(t(t(X[,-1])), Y),
  mdl.4.X1.CV      = fn.LOO.CV(t(t(X[, 2])), Y)
))





# ============================================================================!
# 5) Results ------------------------------------------------------------------
# ----------------------------------------------------------------------------!

(results.Table <- cbind(
  Model = c('Intercept + X1 + X2', 'Intercept + X1', 'X1 + X2', 'X1'),
  round(data.frame(
    AIC =    unlist(results.AIC),
    DIC =    unlist(results.DIC),
    WAIC =   unlist(results.WAIC),
    LOO.CV = unlist(results.LOO.CV)
  ),2)
))
#                 Model   AIC   DIC  WAIC LOO.CV
# 1 Intercept + X1 + X2 56.85 56.84 55.54  57.09
# 2      Intercept + X1 54.67 54.51 53.79  54.92
# 3             X1 + X2 57.28 57.14 56.50  57.46
# 4                  X1 55.24 55.11 54.64  55.33


(results.df <- data.frame(
  Model = rep(c('Intercept + X1 + X2', 'Intercept + X1', 'X1 + X2', 'X1'), 4),
  Info.Criteria = rep(c('AIC', 'DIC', 'WAIC', 'LOO.CV'), each = 4),
  Values = c(
    unlist(results.AIC),
    unlist(results.DIC),
    unlist(results.WAIC),
    unlist(results.LOO.CV)
  )
))
results.df$Info.Criteria <- factor(
  results.df$Info.Criteria,
  levels = c('AIC', 'DIC', 'WAIC', 'LOO.CV')
)
results.df$Model <- factor(
  results.df$Model,
  levels = c('Intercept + X1 + X2', 'Intercept + X1', 'X1 + X2', 'X1')
)





# ============================================================================!
# 6) Save Simulations ---------------------------------------------------------
# ----------------------------------------------------------------------------!
# sim.n.50
sim.n.15 <- list(
  # Simulation data
  dgp = dgp,
  # Fitted models
  models = list(mdl.1.Int.X12 = mdl.1.Int.X12,
                mdl.2.Int.X1  = mdl.2.Int.X1,
                mdl.3.X12     = mdl.3.X12,
                mdl.4.X1      = mdl.4.X1),
  # Estimated log predictive density
  results.log.Pred.Dens = results.log.Pred.Dens,
  # Information Criteria
  results = list(results.AIC    = results.AIC,
                 results.DIC    = results.DIC,
                 results.WAIC   = results.WAIC,
                 results.LOO.CV = results.LOO.CV),
  # Result tables
  results.Table = list(results.Table = results.Table,
                       results.df    = results.df)
)

# save(sim.n.50, file = '01.Code/sim.n.50.RData')
save(sim.n.15, file = '01.Code/sim.n.15.RData')






