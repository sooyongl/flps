#' Generate Fully Latent Principal Stratification data for simulation
#'
#' @description
#' \code{\link{makeSimData}} is a function for generating a data based on
#' the given information.
#'
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param R2eta a numeric indicating Predictive power of latent variable
#' @param linear a logical
#' @param ydist a character
#' @param lambda a numeric indicating the mean of Worked problems/person.
#'  (extent to which covariates predict eta).
#' @param nitem a numeric indicating the number of maximum measurement items
#' given to students.
#' @param nfac a numeric indicating the number of latent factors
#' @param lvmodel a character specifying a type of latent variable model.
#' @param fcovmat a matrix indicating the variance-covariance matrix of latent
#'  factors when nfac > 1
#' @param item.missing a logical to make the measurement item data missing for
#' the control group. Default is TRUE
#' @param misspec a logical to allow cross-loadings across latent factors
#' when nfac > 1.
#' @param cov.res a logical to allow for residual correlations
#' (only for CFA model).
#' @param relsize a numeric indicating the degree to which the latent factor explain the variances of continous items (only for CFA model).
#' @return a list containing all the data related to population values and running FLPS.
#'
#' @examples
#' sdat <- makeSimData(
#'   N = 100,
#'   R2Y = 0.2,
#'   R2eta = 0.5,
#'   linear = T,
#'   ydist = "n",
#'   lambda = .6,
#'   nsec = 10,
#'   nfac = 1,
#'   lvmodel = "2PL"
#' )
#'
#' @export
makeSimData <- function(N,R2Y,R2eta,omega,tau0,tau1,linear,ydist,lambda,nitem,nfac,lvmodel, fcovmat, item.missing, misspec, cov.res, relsize){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc <- as.list(match.call()[-1])

  # set up S3 class
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info<-structure(sim_condition,class=tolower(sim_condition$lvmodel))
  # Data = sim_info

  if(!"fcovmat" %in% names(sim_info)) {
    sim_info$fcovmat <- NULL
  }

  if(!"item.missing" %in% names(sim_info)) {
    sim_info$item.missing <- T
  }

  # Generate Latent Variable Model Information
  sim_info <- genLVinfo(sim_info = sim_info)
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
#' \code{\link{makeInpDat}} is a function for generating a data based on
#' the given information.
#' @param N a numeric indicating sample size.
#' @param R2Y a numeric indicating predictive power of covariates.
#' @param R2eta a numeric indicating Predictive power of latent variable
#' @param linear a logical
#' @param ydist a character
#' @param lambda a numeric indicating the mean of Worked problems/person.
#'  (extent to which covariates predict eta).
#' @param nitem a numeric indicating the number of maximum measurement items
#' given to students.
#' @param nfac a numeric indicating the number of latent factors
#' @param lvmodel a character specifying a type of latent variable model.
#' @param fcovmat a matrix indicating the variance-covariance matrix of latent
#'  factors when nfac > 1
#' @param item.missing a logical to make the measurement item data missing for
#' the control group. Default is TRUE
#' @param misspec a logical to allow cross-loadings across latent factors
#' when nfac > 1.
#' @param cov.res a logical to allow for residual correlations
#' (only for CFA model).
#' @param relsize a numeric indicating the degree to which the latent factor explain the variances of continous items (only for CFA model).
#' @return a list containing all the data related to population values and running FLPS.
#'
#' @examples
#' sdat <- makeInpData(
#'   N = 100,
#'   R2Y = 0.2,
#'   omega = 0.2,
#'   tau0 = 0.13,
#'   tau1 = -0.06,
#'   lambda = 10,
#'   R2eta = 0.5,
#'   nsec = 10,
#'   linear = T,
#'   lvmodel = "sem"
#' )
#' @export
makeInpData <- function(N, R2Y, R2eta, omega, tau0, tau1,
                        linear=T, ydist='n',lambda,
                        nitem, nfac = 1, lvmodel,
                        fcovmat, item.missing, misspec, cov.res, relsize){

  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(list); # mc <- as.list(match.call()[-1])

  # set up S3 class
  sim_info <- structure(eval(mc), class = tolower(lvmodel))
  # sim_info<-structure(sim_condition,class=tolower(sim_condition$lvmodel))
  # Data = sim_info

  if(!"relsize" %in% names(sim_info)) {
    sim_info$relsize <- 0.6
  }

  if(!"cov.res" %in% names(sim_info)) {
    sim_info$cov.res <- 0
  }

  if(!"misspec" %in% names(sim_info)) {
    sim_info$misspec <- F
  }

  if(!"fcovmat" %in% names(sim_info)) {
    sim_info$fcovmat <- NULL
  }

  if(!"item.missing" %in% names(sim_info)) {
    sim_info$item.missing <- T
  }

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
