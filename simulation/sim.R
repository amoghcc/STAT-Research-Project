
library(reshape2)
library(tidyverse)
library(spatialEco)

#' #################################################################################
#' Simulated data:
#' 
#' Assume there are 5 time-independent (exponential) covariates; X1, X2, X3, X4, X5
#' Assume X1 is the important covariate that we want to find a cutpoint of.
#' Think of this as the constriction velocity in the dataset.
#' Assume X2 and X3 have signficant effect while X4 and X5 are nuisance variables.
#'
#' @param n is the sample size
#' @param cp is the true cutpoint (we assume we know this in the simulation,
#' but is unknown in real life data)

simDat <- function(n, cp = 1) {
    dat <- data.frame(matrix(rexp(n * 5), n))
    xb <- (dat$X1 > cp) + dat$X2 - dat$X3
    dat$Y <- rbinom(n, 1, 1 / (1 + exp(-xb)))
    return(dat)
}

dat <- simDat(100)
cp.vec <- seq(0, 6, .1)[-1]
cons <- rep(NA, length(cp.vec))
for (i in 1:length(cp.vec)) {
    fit <- glm(Y ~ I(X1 < cp.vec[i]) + X2 + X3 + X4 + X5, binomial, dat)
    cons[i] <- concordance(dat$Y, predict(fit, type = "response"))$con
}
cp.vec[which.max(cons)]

do <- function() {
    dat <- simDat(100)
    cp.vec <- seq(0, 6, .1)[-1]
    cons <- rep(NA, length(cp.vec))
    for (i in 1:length(cp.vec)) {
        fit <- glm(Y ~ I(X1 < cp.vec[i]) + X2 + X3 + X4 + X5, binomial, dat)
        cons[i] <- concordance(dat$Y, predict(fit, type = "response"))$con
    }
    cp.vec[which.max(cons)]
}

foo <- replicate(1e3, do())
summary(foo)



## Tree method? 
library(partykit)
ctree(Y ~ X1 + X2 + X3 + X4 + X5, dat)
ctree(Y ~ X1 + X2 + X3 + X4 + X5, dat, control = ctree_control(stump = TRUE))

#' #################################################################################
#'
#' Generate data from different distributions given parameters
#'
#' @param n is the sample size
#' @param cp is the true cutpoint
#' @param p is the number of covaraites; this includes
#' X1 that we want to find a cutpoint of
#' X2 and X3 that related to the response
#' X4, X5, ... Xp are noise 
#' @param dist is the distribution where the data is generated from
#' @param ... are the distributaional parameters to pass to dist
#'
simDat <- function(n, cp = 3, p = 5, round = FALSE, dist = "exp", ...) {
    dat <- data.frame(matrix(get(paste0("r", dist))(n * p, ...), n))
    if (round) dat$X1 <- round(dat$X1, 1)
    if (dist == "beta") dat$X1 <- 4 * dat$X1
    xb <- (dat$X1 > cp) + dat$X2 - dat$X3 
    dat$Y <- rbinom(n, 1, 1 / (1 + exp(-xb)))
    return(dat)
}

#' half normal random generator
rhnorm <- function(n, mean = 0, sd = 1) {
    abs(rnorm(n, mean, sd))
}


#' Function to find cutpoint
do <- function(n = 100, cp = 3, p = 5, round = FALSE, dist = "exp", ...) {
    dat <- simDat(n, cp, p, round, dist, ...)
    dat2 <- simDat(n, cp, p, round, dist, ...)
    cp.vec <- seq(0, 6, .1)[-1]
    cons <- rep(NA, length(cp.vec))
    for (i in 1:length(cp.vec)) {
        fit <- glm(Y ~ I(X1 < cp.vec[i]) + . - X1, binomial, dat)
        ## With training data
        ## cons[i] <- concordance(dat$Y, predict(fit, type = "response"))$con
        ## With testing data
        cons[i] <- concordance(dat2$Y, predict(fit, type = "response", newdata = dat2))$con
    }
    cp.vec[which.max(cons)]
}

## Standard exponential
fit1 <- replicate(100, do())
## gamma
fit2 <- replicate(100, do(dist = "gamma", shape = 2, scale = .5))
## half normal
fit3 <- replicate(100, do(dist = "hnorm", mean = 1, sd = 1))
## beta
fit4 <- replicate(100, do(dist = "beta", shape1 = 2, shape2 = 2))
    
data.frame(fit1, fit2, fit3, fit4) %>% melt() %>%
    mutate(variable = rep(c("exp", "gamma", "half normal", "beta"), each = 100)) %>% 
    ggplot(aes(x = value, color = variable)) + geom_density()



## Standard exponential
fit1 <- replicate(100, do(n = 200))
## gamma
fit2 <- replicate(100, do(n = 200, dist = "gamma", shape = 2, scale = .5))
## half normal
fit3 <- replicate(100, do(n = 200, dist = "hnorm", mean = 1, sd = 1))
## beta
fit4 <- replicate(100, do(n = 200, dist = "beta", shape1 = 2, shape2 = 2))
    
data.frame(fit1, fit2, fit3, fit4) %>% melt() %>%
    mutate(variable = rep(c("exp", "gamma", "half normal", "beta"), each = 100)) %>% 
    ggplot(aes(x = value, color = variable)) + geom_density()

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
