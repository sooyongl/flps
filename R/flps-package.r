#' The 'flps' package.
#'
#' @name flps-package
#' @title Fully latent principal stratification
#' @description The FLPS package conducts Bayesian analysis for fully latent principal stratification via rstan.
#'
#' @docType package
#' @aliases flps
#' @author Sooyong Lee \email{sooyongl09@utexas.edu}
#'
#' @references
#' Sales, A. C., & Pane, J. F. (2019). The role of mastery learning in an intelligent tutoring system: Principal stratification on a latent variable. The Annals of Applied Statistics, 13(1), 420-443.
#' Lee, S., Adam, S., Kang, H.-A., & Whittaker, T. A. (2022). Fully latent principal
#' stratification: Combining ps with model-based measurement models. In The annual
#' meeting of the psychometric society (pp. 287–298).
#'
#' @import Rcpp
#' @import rstan
#' @import methods
#' @import ggplot2
#' @import glue
#' @importFrom stats runif rnorm rmultinom rlnorm quantile var cov cor
#' @importFrom utils read.csv packageVersion head tail
#' @importFrom MASS mvrnorm
#' @importFrom mvtnorm rmvnorm
#' @keywords package
NULL
