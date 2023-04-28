
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
libs <- c('tidyverse', 'ggplot2', 'ggpubr', 'xtable')
lapply(libs, require, character.only = TRUE)
rm(libs)




# ============================================================================!
# 1) Load Simulation Data -----------------------------------------------------
# ----------------------------------------------------------------------------!

load('01.Code/sim.n.15.RData')
load('01.Code/sim.n.50.RData')





# ============================================================================!
# 2) log Pred. Dens. ----------------------------------------------------------
# ----------------------------------------------------------------------------!


## 2.1) Max (MLE) & Mean ------------------------

log.Pred.Dens.df <- data.frame(
  Model = rep(c('Intercept + X1 + X2', 'Intercept + X1', 'X1 + X2', 'X1'), 2),
  n = rep(c(15,50), each = 4)
)

log.Pred.Dens.df$max <- c(
  max(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.1),
  max(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.2),
  max(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.3),
  max(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.4),
  max(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.1),
  max(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.2),
  max(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.3),
  max(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.4)
)

log.Pred.Dens.df$mean <- c(
  mean(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.1),
  mean(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.2),
  mean(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.3),
  mean(sim.n.15$results.log.Pred.Dens$log.Pred.Dens.4),
  mean(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.1),
  mean(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.2),
  mean(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.3),
  mean(sim.n.50$results.log.Pred.Dens$log.Pred.Dens.4)
)

log.Pred.Dens.df
#                 Model  n       max      mean
# 1 Intercept + X1 + X2 15 -24.26573 -26.60626
# 2      Intercept + X1 15 -24.26483 -25.92237
# 3             X1 + X2 15 -25.56858 -27.23222
# 4                  X1 15 -25.60333 -26.66581
# 5 Intercept + X1 + X2 50 -69.62063 -71.73823
# 6      Intercept + X1 50 -69.70965 -71.25221
# 7             X1 + X2 50 -73.78118 -75.32513
# 8                  X1 50 -73.80129 -74.81267

print(xtable(log.Pred.Dens.df))



## 2.2) Histograms ------------------------------
fn.hist <- function(log.Pred.Dens, title.text) {
  p <- data.frame(
    Values = log.Pred.Dens
  ) %>% 
  ggplot(aes(x = Values)) +
    geom_histogram(aes(y=..density..), color='black', fill="white", alpha=.2, binwidth=.25) +
    geom_density(alpha=.5, color="#FF6666", linewidth=1) +
    ylab('') +
    xlab(expression(log ~ p ~ '(' ~ Y ~ '|' ~ theta ~ ')')) +
    labs(title = title.text) +
    theme(plot.background    = element_rect(fill = 'white'),
          panel.background   = element_rect(fill = 'white'),
          panel.grid.major   = element_line(linetype = 'dashed',
                                            linewidth = .5, color = 'lightgray'),
          legend.background = element_rect(fill = "white"),
          legend.position   = 'top',
          # legend.title      = element_blank()
    )
  return(p)
}


fn.hist.Grid <- function(sim.n) {
  p.list <- list(
    p1 = fn.hist(sim.n$results.log.Pred.Dens$log.Pred.Dens.1, expression('Model 1: Intercept +' ~ X[1] ~ '+' ~ X[2])),
    p2 = fn.hist(sim.n$results.log.Pred.Dens$log.Pred.Dens.2, expression('Model 2: Intercept +' ~ X[1])),
    p11 = NULL, p12 = NULL,
    p3 = fn.hist(sim.n$results.log.Pred.Dens$log.Pred.Dens.3, expression('Model 3:'             ~ X[1] ~ '+' ~ X[2])),
    p4 = fn.hist(sim.n$results.log.Pred.Dens$log.Pred.Dens.4, expression('Model 4:' ~ X[1]))
  )
  p <- ggarrange(plotlist = p.list, ncol = 2, nrow = 3,
                 heights = c(1, 0.1, 1),
                 common.legend = TRUE, legend = 'bottom')
  return(p)
}


## 2.3) Plots -----------------------------------
p.log.Pred.Dens.15 <- fn.hist.Grid(sim.n.15)
p.log.Pred.Dens.15
png('02.Figures/log.Predictive.Density.n.15.png', 
    width = 1000, height = 600, pointsize = 15)
print(p.log.Pred.Dens.15)
dev.off()

p.log.Pred.Dens.50 <- fn.hist.Grid(sim.n.50)
p.log.Pred.Dens.50
png('02.Figures/log.Predictive.Density.n.50.png', 
    width = 1000, height = 600, pointsize = 15)
print(p.log.Pred.Dens.50)
dev.off()











# ============================================================================!
# 3) Information Criteria -----------------------------------------------------
# ----------------------------------------------------------------------------!

fn.plot.Info.Criteria <- function(results.df, ymin, ymax) {
  p <- results.df %>% 
    ggplot() +
    geom_bar(aes(x = Info.Criteria, y = Values, fill = Model),
             width = 0.75, position = 'dodge', stat = 'identity') +
    ylab('') +
    xlab('Information Criteria') +
    labs(title = 'Information Criteria from Various Model Choices') +
    coord_cartesian(ylim = c(ymin, ymax)) +
    scale_fill_brewer(palette = "Set2",
                      direction = -1,
                      name = 'Model Choices',
                      labels = c('(M1) Intercept + X1 + X2', '(M2) Intercept + X1', '(M3) X1 + X2', '(M4) X1')) +
    theme(plot.background    = element_rect(fill = 'white'),
          panel.background   = element_rect(fill = 'white'),
          panel.grid.major   = element_line(linetype = 'dashed',
                                            size = .5, color = 'lightgray'),
          # axis.text.x = element_blank(),
          # Legend info
          legend.background = element_rect(fill = "white"),
          legend.position   = 'top',
          # legend.title      = element_blank()
    )
  return(p)
}


## 3.1) Plots -----------------------------------

(p.Info.Crit.15 <- fn.plot.Info.Criteria(sim.n.15$results.Table$results.df, 50, 60))
(p.Info.Crit.50 <- fn.plot.Info.Criteria(sim.n.50$results.Table$results.df, 140, 160))

png('02.Figures/Info.Criteria.n.15.png', 
    width = 600, height = 400, pointsize = 15)
print(p.Info.Crit.15)
dev.off()

png('02.Figures/Info.Criteria.n.50.png', 
    width = 600, height = 400, pointsize = 15)
print(p.Info.Crit.50)
dev.off()
