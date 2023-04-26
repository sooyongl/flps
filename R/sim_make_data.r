#' Generate Fully Latent Principal Stratification data for simulation
#'
#' @description
#' \code{\link{makeInpData}} is a function for generating a data based on
#' the given information.
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param R2eta a numeric indicating Predictive power of latent variable
#' @param omega a numeric indicating the size of effect of latent factor on
#' the outcome.
#' @param tau0 a numeric indicating the size of difference in the outcome
#'  between the treatment and the control.
#' @param tau1 a numeric indicating the principal effect
#' @param betaL a numeric vector indicating the effects of covariates on the latent factor
#' @param betaY a numeric vector indicating the effects of covariates on the outcome
#' @param linear a logical whether the relationship between the outcome and covariates is linear (default is \code{TRUE}).
#' @param ydist a character indicating the outcome distribution (default is \code{n}).
#' @param lambda a numeric indicating the mean of Worked problems/person.
#'  (extent to which covariates predict eta).
#' @param nitem a numeric indicating the number of maximum measurement items
#' given to students.
#' @param nfac a numeric indicating the number of latent factors
#' @param lvmodel a character specifying a type of latent variable model.
#' @param fcovmat a matrix indicating the variance-covariance matrix of latent
#'  factors when nfac > 1
#' @param item.missing a logical to make the measurement item data missing for
#' the control group (default is \code{TRUE}).
#' @param misspec a logical to allow cross-loadings across latent factors
#' when nfac > 1 (default is \code{FALSE}).
#' @param cov.res a logical to allow for residual correlations
#' (only for CFA model) (default is \code{0}).
#' @param relsize a numeric indicating the degree to which the latent factor explain the variances of continuous items (only for CFA model) (default is \code{0.6}).
#' @return a list containing all the data related to population values and running FLPS.
#'
#' @examples
#' sdat <- makeSimData(
#' N       = 200,  # sample size
#' R2Y     = 0.2,  # r^2 of outcome
#' R2eta   = 0.5,  # r^2 of eta by one covariates
#' omega   = 0.2,  # the effect of eta
#' tau0    = 0.13, # direct effect
#' tau1    = -0.06,# interaction effect between Z and eta
#' betaL   = 0.2,
#' betaY   = 0.4,
#' lambda  = 0.8,  # the proportion of administered items
#' nitem    = 10,   # the total number of items
#' nfac    = 1,    # the number of latent factors
#' lvmodel = '2pl' )
#'
#' @export
makeSimData <- function(N,R2Y,R2eta,omega,tau0,tau1,betaL, betaY,
                        linear,ydist,lambda,nitem,nfac,lvmodel, fcovmat, item.missing=T, misspec=F, cov.res=0, relsize=0.6){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc <- as.list(match.call()[-1])

  # set up S3 class
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info<-structure(sim_condition,class=tolower(sim_condition$lvmodel))
  # Data = sim_info

  validate_siminfo(sim_info)

  sim_info <- flps:::addDefault(sim_info)

  # Generate Latent Variable Model Information
  sim_info <- flps:::genLVinfo(sim_info = sim_info)
  # Generate Structural model part
  sim_info <- genStructure(sim_info)
  # Generate Measurement model part
  sim_info <- genMeasurement(sim_info)
  # Generate data structure for Stan
  sim_info <- genStanData(sim_info)

  return(sim_info)
}

#' Generate a matrix style data for simulation
#'
#' @description
#' \code{\link{makeInpData}} is a function for generating a data based on
#' the given information.
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param R2eta a numeric indicating Predictive power of latent variable
#' @param omega a numeric indicating the size of effect of latent factor on
#' the outcome.
#' @param tau0 a numeric indicating the size of difference in the outcome
#'  between the treatment and the control.
#' @param tau1 a numeric indicating the principal effect
#' @param betaL a numeric vector indicating the effects of covariates on the latent factor
#' @param betaY a numeric vector indicating the effects of covariates on the outcome
#' @param linear a logical whether the relationship between the outcome and covariates is linear (default is \code{TRUE}).
#' @param ydist a character indicating the outcome distribution (default is \code{n}).
#' @param lambda a numeric indicating the mean of Worked problems/person.
#'  (extent to which covariates predict eta).
#' @param nitem a numeric indicating the number of maximum measurement items
#' given to students.
#' @param nfac a numeric indicating the number of latent factors
#' @param lvmodel a character specifying a type of latent variable model.
#' @param fcovmat a matrix indicating the variance-covariance matrix of latent
#'  factors when nfac > 1
#' @param item.missing a logical to make the measurement item data missing for
#' the control group (default is \code{TRUE}).
#' @param misspec a logical to allow cross-loadings across latent factors
#' when nfac > 1 (default is \code{FALSE}).
#' @param cov.res a logical to allow for residual correlations
#' (only for CFA model) (default is \code{0}).
#' @param relsize a numeric indicating the degree to which the latent factor explain the variances of continuous items (only for CFA model) (default is \code{0.6}).
#' @return a list containing all the data related to population values and running FLPS.
#'
#' @examples
#' sdat <- makeInpData(
#' N       = 200,  # sample size
#' R2Y     = 0.2,  # r^2 of outcome
#' R2eta   = 0.5,  # r^2 of eta by one covariates
#' omega   = 0.2,  # the effect of eta
#' tau0    = 0.13, # direct effect
#' tau1    = -0.06,# interaction effect between Z and eta
#' betaL   = 0.2,
#' betaY   = 0.4,
#' lambda  = 0.8,  # the proportion of administered items
#' nitem    = 10,   # the total number of items
#' nfac    = 1,    # the number of latent factors
#' lvmodel = '2pl' )
#' @export
makeInpData <- function(N, R2Y, R2eta, omega, tau0, tau1, betaL, betaY,
                        linear=TRUE, ydist='n',lambda,
                        nitem, nfac = 1, lvmodel,
                        fcovmat, item.missing=TRUE, misspec=FALSE, cov.res=0, relsize=0.6){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc <- as.list(match.call()[-1])

  # set up S3 class
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info<-structure(sim_condition,class=tolower(sim_condition$lvmodel))
  # Data = sim_info

  validate_siminfo(sim_info)

  sim_info <- addDefault(sim_info)

  # Generate Latent Variable Model Information
  sim_info <- genLVinfo(sim_info = sim_info)
  # Generate Structural model part
  sim_info <- genStructure(sim_info)
  # Generate Measurement model part
  sim_info <- genMeasurement(sim_info)

  inp_data <-  cbind(sim_info$struc_data, sim_info$lv.resp)

  if(sim_info$item.missing) {
    inp_data[inp_data$Z == 0, names(sim_info$lv.resp)] <- NA
  }

  inp_data
}
