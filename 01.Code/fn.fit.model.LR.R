#' Fit Linear Regression Model the Bayesian way with Uniform Prior
#'
#' fn.fit.model.LR: to fit Linear Regression model the Bayesian way
#' Uniform prior: p(beta, log(sigma)) = 1
#' 
#' Parameters -------------------------
#' @param X.train  X
#' @param Y.train  Y

fn.fit.model.LR <- function(X.train, Y.train) {
  # Hyper-parameters
  n <- nrow(X.train)
  J <- ncol(X.train)
  # Fit model
  (Beta.Post      <- solve(t(X.train)%*%X.train) %*% t(X.train) %*% Y.train)
  (V.Beta.Post    <- solve(t(X.train)%*%X.train))
  (s.squared.Post <- 1/(n-J) * t(Y.train - X.train%*%Beta.Post) %*% (Y.train - X.train%*%Beta.Post))
  # Return
  return(list(Beta.Post      = Beta.Post,
              V.Beta.Post    = V.Beta.Post,
              s.squared.Post = s.squared.Post))
}