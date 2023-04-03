#' The 'flps' package.
#'
#' @name FLPS-package
#' @title Fully latent principal stratification
#' @description The FLPS package conducts bayesian analysis for fully latent principal stratification via rstan.
#'
#' @docType package
#' @aliases flps
#' @author Sooyong Lee
#'
#' @references
#' Sales, A. C., & Pane, J. F. (2019). The role of mastery learning in an intelligent tutoring system: Principal stratification on a latent variable. The Annals of Applied Statistics, 13(1), 420-443.
#' @import Rcpp
#' @import rstan
#' @import methods
#' @importFrom stats runif rnorm rmultinom rlnorm quantile var cov cor
#' @importFrom utils read.csv packageVersion head tail
#' @importFrom MASS mvrnorm
#' @importFrom mvtnorm rmvnorm
#' @keywords package
NULL

# library(lavaan)
# library(mirt)
# library(poLCA)
# library(tidyLPA)
# library(flexmix)
# library(broom)
# library(lme4)
# library(mvtnorm)
# library(coda)
# library(rstan)
# library(tidyverse)
# library(ggmcmc)
# library(doSNOW)
# library(doParallel)
# library(foreach)
# library(bayesplot)
