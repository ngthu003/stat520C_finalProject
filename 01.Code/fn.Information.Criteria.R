#' Functions to compute Information Criteria given Fitted Models


#' fn.AIC: to compute AIC: Akaike Information Criteria
#' 
#' Parameters -------------------------
#' @param X         X
#' @param Y         Y
#' @param mdl.list  fitted model
#' @param S         #(simulations)
#' 
fn.AIC <- function(X, Y, mdl.list, S = 1e4) {
  # k: #(free parameters)
  k <- nrow(mdl.list$Beta.Post) + 1  
  # log Predictive Density given MLE
  log.Pred.Dens <- sum(dnorm(Y, mean = X%*%mdl.list$Beta.Post, sd = sqrt(mdl.list$s.squared.Post), log = T))
  # elpd: expected log predictive density
  elpd.AIC <- log.Pred.Dens - k
  # AIC
  AIC <- -2 * elpd.AIC
  # Return
  return(AIC)
}


#' fn.DIC: to compute DIC: Deviance Information Criteria
#' 
#' Parameters -------------------------
#' @param X         X
#' @param Y         Y
#' @param mdl.list  fitted model
#' @param S         #(simulations)
#' 
fn.DIC <- function(X, Y, mdl.list, S = 1e4) {
  
  # Extract fitted parameters
  Beta <- mdl.list$Beta.Post
  V.Beta <- mdl.list$V.Beta.Post
  s.squared <- mdl.list$s.squared.Post
  n <- nrow(X)
  J <- nrow(Beta)
  
  # Relevant Formula ===========================!
  # p.DIC = 2 ( log(p(Y|E.post[theta])) - E.post[log(p(Y|theta))] )
  
  # 2nd term in p.DIC
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
  term.2.E.post <- mean(log.Pred.Dens)
  
  # 1st term in p.DIC
  E.post.Beta <- Beta
  E.post.s.squared <- (n-J)*s.squared / (n-J-2)
  term.1.log <- sum(dnorm(Y, mean = X%*% E.post.Beta, sd = sqrt(E.post.s.squared), log = T))
  
  # p.DIC: Combine both terms
  p.DIC <- 2 * (term.1.log - term.2.E.post)
  # elpd: expected log predictive density
  elpd.DIC <- term.1.log - p.DIC
  # DIC
  DIC <- -2 * elpd.DIC
  # Return
  return(DIC)
  
}



#' fn.WAIC: to compute WAIC: Watanabe-Akaike Information Criteria
#' 
#' Parameters -------------------------
#' @param X         X
#' @param Y         Y
#' @param mdl.list  fitted model
#' @param S         #(simulations)
#' 
fn.WAIC <- function(X, Y, mdl.list, S = 1e4) {
  
  # Extract fitted parameters
  Beta <- mdl.list$Beta.Post
  V.Beta <- mdl.list$V.Beta.Post
  s.squared <- mdl.list$s.squared.Post
  n <- nrow(X)
  J <- nrow(Beta)
  
  # Relevant Formula ===========================!
  # p.WAIC = 2 sum (
  #   log E.post[ p(Yi|theta) ] - E.post[ log p(Yi|theta) ]
  # )
  
  # LPPD: log pointwise predictive density
  # 1st term: log E.post[ p(Yi|theta) ]
  lppd.WAIC <- 0
  # Loop for each data observation
  for (i in 1:n) {
    log.Pred.Dens <- numeric(S)
    # Draw S samples
    for (s in 1:S) {
      # Generate sigma
      sigma.squared.s <- rinvchisq(1, df = n-J, scale = s.squared)
      # Generate beta
      Beta.s <- t(t(mvrnorm(n=1, mu = Beta, Sigma = sigma.squared.s[1,1]*V.Beta)))
      # log p(y|theta)
      log.Pred.Dens.tmp <- sum(dnorm(Y[i], mean = X[i,]%*%Beta.s, sd = sqrt(sigma.squared.s)))
      log.Pred.Dens[s]  <- log.Pred.Dens.tmp
    }
    # Update lppd from current i-th obs.
    lppd.WAIC <- lppd.WAIC + log(mean(log.Pred.Dens))
  }
  
  # 2nd term: E.post[ log p(Yi|theta) ]
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
  term.2.E.post <- mean(log.Pred.Dens)
  
  # p.WAIC
  p.WAIC <- 2*(lppd.WAIC - term.2.E.post)
  # elppd: expected log pointwise predictive density for a new dataset
  elppd.WAIC <- lppd.WAIC - p.WAIC
  # WAIC
  WAIC <- -2*elppd.WAIC
  # Return
  return(WAIC)
  
}




#' fn.LOO.CV: to compute Leave One Out Cross-Validation
#' 
#' Parameters -------------------------
#' @param X         X
#' @param Y         Y
#' @param S         #(simulations)
#' 
fn.LOO.CV <- function(X, Y, S = 1e4) {
  
  n <- nrow(X)
  lppd.LOO <- 0
  
  # Loop for each data observation
  for (i in 1:n) {
    
    # Fit model on all but the i-th obs.
    X.train <- t(t(X[-i,]))
    Y.train <- Y[-i,] 
    n.LOO   <- nrow(X.train)
    J.LOO   <- ncol(X.train)
    X.test  <- X[ i,]
    Y.test  <- Y[ i,]
    # Fix dimension if J = 1
    if (J.LOO == 1) {X.test  <- t(t(X[ i,]))}
    # Fit model on all but i-th obs
    (mdl.fit.LOO       <- fn.fit.model.LR(X.train, Y.train))
    Beta.Post.LOO      <- mdl.fit.LOO$Beta.Post
    V.Beta.Post.LOO    <- mdl.fit.LOO$V.Beta.Post
    s.squared.Post.LOO <- mdl.fit.LOO$s.squared.Post
    
    # Compute log Pred. Dens. on that i-th obs
    log.Pred.Dens.LOO <- numeric(S)
    # Draw S samples
    for (s in 1:S) {
      # Generate sigma
      sigma.squared.s <- rinvchisq(1, df = n.LOO - J.LOO, scale = s.squared.Post.LOO)
      # Generate beta
      Beta.s <- t(t(mvrnorm(n=1, mu = Beta.Post.LOO, Sigma = sigma.squared.s[1,1]*V.Beta.Post.LOO)))
      # log p(y|theta)
      log.Pred.Dens.tmp <- sum(dnorm(Y.test, mean = X.test%*%Beta.s, sd = sqrt(sigma.squared.s)))
      log.Pred.Dens.LOO[s] <- log.Pred.Dens.tmp  
    }
    lppd.LOO <- lppd.LOO + log(mean(log.Pred.Dens.LOO))  
    
  }
  
  # LOO.CV
  LOO.CV <- -2 * lppd.LOO
  
  return(LOO.CV)
}