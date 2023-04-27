#' Fit Linear Regression Model the Bayesian way with Uniform Prior
#'
#' fn.fit.model.LR: to fit Linear Regression model the Bayesian way
#' Uniform prior: p(beta, log(sigma)) = 1
#' 
#' Parameters -------------------------
#' @param X         X
#' @param Y         Y
#' @param mdl.list  fitted model
#' @param S         #(simulations)

fn.log.Pred.Dens.Sim <- function(X, Y, n, mdl.list, S = 1e4) {
  # Extract fitted parameters
  Beta      <- mdl.list$Beta.Post
  V.Beta    <- mdl.list$V.Beta.Post
  s.squared <- mdl.list$s.squared.Post
  J <- nrow(Beta)
  # if (J == 1) {X <- X[,-1]}
  # Simulation
  log.Pred.Dens <- numeric(S)
  for (s in 1:S) {
    # Generate sigma
    sigma.squared.s <- rinvchisq(1, df = n-J, scale = s.squared)
    # Generate beta
    Beta.s <- t(t(mvrnorm(n=1, mu = Beta, Sigma = sigma.squared.s[1,1]*V.Beta)))
    # log p(y|theta)
    log.Pred.Dens.tmp <- sum(dnorm(Y, mean = X%*%Beta.s, sd = sqrt(sigma.squared.s), log = T))
    log.Pred.Dens[s]  <- log.Pred.Dens.tmp
  }
  return(log.Pred.Dens)
}
