# Stat 520C - Final Project

# Model Evaluation and Selection the Bayesian Way

Date: 04/28/2023

This project focuses on the different ways fitted models can be evaluated and compared under the Bayesian framework. The reference text is *Bayesian Data Analysis* by Gelman et al.

This repo contains all the codes to:

1.  simulate a simple linear regression model 

$$
  Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \epsilon, 
  \ \ \ \ \ \ 
  \epsilon \sim {\cal N}(0, \sigma^2),
$$

2.  fit the various model choices under Bayesian framework with a non-informative prior

$$
  p(\beta_0, \beta_1, \beta_2, \log \sigma) \propto 1,
$$


3.  evaluate and compare those models with Information Criteria

$$
  \text{AIC,} \ \ \ 
  \text{DIC,} \ \ \ 
  \text{WAIC,} \ \ \ 
  \text{AIC.}
$$


