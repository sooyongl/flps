#' Default priors
#'
#' @noRd
defaultPriors <- function(nfac) {

  priors_set <- list()

  priors_set$ptau0 <- matrix(c(0, 5), nrow = 1)
  priors_set$ptau1 <- matrix(rep(c(0, 5),each=nfac), nfac)

  priors_set$ptau0W <- matrix(c(0, 5), nrow = 1)
  priors_set$ptau0B <- matrix(c(0, 5), nrow = 1)
  priors_set$ptau1W <- matrix(rep(c(0, 5),each=nfac), nfac)
  priors_set$ptau1B <- matrix(rep(c(0, 5),each=nfac), nfac)

  priors_set$pomega <- matrix(rep(c(0, 5),each=nfac), nfac)
  priors_set$pomegaW <- matrix(rep(c(0, 5),each=nfac), nfac)
  priors_set$pomegaB <- matrix(rep(c(0, 5),each=nfac), nfac)

  priors_set$pbetaU <- c(0, 5)
  priors_set$pbetaUW <- c(0, 5)
  priors_set$pbetaUB <- c(0, 5)
  priors_set$pbetaY <- c(0, 5)
  priors_set$pbetaYW <- c(0, 5)
  priors_set$pbetaYB <- c(0, 5)

  priors_set$ploading <- c(0, 5)
  priors_set$pintcpt <- c(0, 5)
  # priors_set$intcpt <- c(0,5)
  # priors_set$loading_free <- c(0,1)

  priors_set
}

#' Set up priors
#'
#' @noRd
setPriors <- function(priors_input, lv_model, stan_options) {

  nfac <- lengths(regmatches(lv_model, gregexpr("=~", lv_model)))

  given.priors <- priors_input

  priors_set <- defaultPriors(nfac)

  if(!is.null(given.priors)) {

    if(any(names(given.priors)%in%"tau0")) {

      priors_set$ptau0 <- matrix(
        unlist(given.priors[names(given.priors)%in%"tau0"]), 1)
    }

    if(any(names(given.priors)%in%"tau1")) {
      priors_set$ptau1 <- matrix(
        unlist(given.priors[names(given.priors)%in%"tau1"]), nfac)
    }

    if(any(names(given.priors)%in%"omega")) {
      priors_set$pomega <- matrix(
        unlist(given.priors[names(given.priors)%in%"omega"]), nfac)
    }

  }

  stan_options$data <- append(stan_options$data, priors_set)

  return(stan_options)
}

