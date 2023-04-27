#' (Missing) Data Generation Process
#'
#' fn.dgp: to generate simple linear regression data
#' 
#' Parameters -------------------------
#' @param n              #(obs)
#' @param betas          True betas, in list form
#' @param sigma.squared  True sigma^2

fn.dgp <- function(n, betas, sigma.squared) {
  
  J <- length(betas)
  # True Beta
  Beta.True <- t(t(unlist(betas)))
  # True sigma^2
  sigma.squared.True <- sigma.squared
  
  # Data Simulation
  (X <- cbind(1, matrix(rnorm(n*(J-1),0,1), nrow = n)))
  (Y <- X%*%Beta.True + rnorm(n, 0, sqrt(sigma.squared.True)))
  cbind(X,Y)
  
  # Return
  return(list(Beta.True = Beta.True,
              sigma.squared.True = sigma.squared.True,
              X = X,
              Y = Y,
              n = n,
              J = J))
  
}