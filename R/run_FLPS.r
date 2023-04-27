#' Conduct fully latent principal stratification
#'
#' @param inp_data A matrix or a data frame
#' @param custom_data A list. should be provided with \code{custom_stan}.
#' @param custom_stan A string. should be provided with \code{custom_data}.
#' @param outcome A character indicating the name of an outcome variable
#' @param group A character indicating the name of a treatment/control group variable
#' @param covariate A character indicating the names of covariate variables
#' @param lv_model A description of the latent variable model, which is similar
#' to the \pkg{lavaan} model syntax.
#'  \itemize{
#'    \item \code{=~} : Specify the association between factors and indicators (e.g., F1 =~ v1 + v2 + v3).
#'    \item \code{+} : Specify a series of indicators
#' }

#' @param lv_type  A character indicating the type of latent variable models
#' @param priors_input A list of priors. Otherwise, the default priors are used (N(0, 5). It takes three parameter names including \code{tau0}, \code{tau1}, and \code{omega}, which are the difference between groups, the principal effects, and the effect of latent factors on the outcome. If added, the length of \code{tau1} and \code{omega} must be matched with the number of factors.
#' Examples of How to specify priors as follows:
#'  \itemize{
#'    \item \code{list(tau0 = c(0, 1), tau1 = c(0.5, 1))} : The first element is the mean and the second is the variance of normal priors.
#'    \item \code{list(tau1 = list(c(0.5, 1), c(-0.4, 1))} : If there's two factors.
#' }
#'
#' @param stan_options A list containing [rstan::stan()] options, using 'name = value'.
#' @param ... Additional arguments for latent variable models information (e.g., nclass = 2).
#' @return an object of class \code{flps} which contains a \code{\link[rstan]{stanfit}} object.
#'
#'  \item{call}{argument calls}
#'  \item{inp_data}{A given data frame}
#'  \item{flps_model}{a Stan syntax used in [rstan::stan()]}
#'  \item{flps_data}{a list of data used in [rstan::stan()]}
#'  \item{flps_fit}{\code{\link[rstan]{stanfit}}}
#'  \item{time}{a numeric of timing}
#'
#' @examples
#' \dontrun{
#' inp_data <- flps::makeInpData(
#'   N       = 200,
#'   R2Y     = 0.2,
#'   R2eta   = 0.5,
#'   omega   = 0.2,
#'   tau0    = 0.23,
#'   tau1    = -0.16,
#'   betaL   = 0.1,
#'   betaY   = 0.2,
#'   lambda  = 0.8,
#'   nitem    = 10,
#'   nfac    = 1,
#'   lvmodel = 'rasch' )
#'
#' res <- runFLPS(
#'    inp_data = inp_data,
#'    outcome = "Y",
#'    group = "Z",
#'    covariate = c("X1"),
#'    lv_type = "rasch",
#'    lv_model = "F =~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10",
#'    stan_options = list(iter = 1000, warmup = 500, cores = 1, chains = 2)
#'    )
#' }
#'
#' @family rstan
#' @seealso [rstan::stan()]
#' @export
runFLPS <- function(inp_data = NULL,
                    custom_data = NULL,
                    custom_stan = NULL,
                    outcome = NULL,
                    group = NULL,
                    covariate = NULL,
                    lv_model = NULL,
                    lv_type = NULL,
                    priors_input = NULL,
                    stan_options = list(),
                    ...
) {

  # time record --------------------------------------------------------
  start.time <- proc.time()[3L]

  # call ---------------------------------------------------------------
  .call <- match.call()
  argslist <- as.list(.call[-1])

  # validate -----------------------------------------------------------
  validate_data(inp_data, custom_data, custom_stan)


  # data and code -------------------------------------------------------
  if(is.null(inp_data) && !is.null(custom_data) && !is.null(custom_stan)) {
    flps_data_class <- makeFLPSdata(custom_data, outcome, group, covariate,
                                    lv_model, lv_type, custom = T)

    flps_model <- custom_stan
  }

  if (!is.null(inp_data) && is.null(custom_data)) {
    flps_data_class <- makeFLPSdata(inp_data, outcome, group, covariate,
                                    lv_model, lv_type)

    flps_model <- loadRstan(lv_type = flps_data_class$lv_type)
    # flps_model <- paste(readLines("inst/stan/flps_IRT_multi.stan"), collapse = "\n")
    # flps_model <- mkStanModel(lv_type = flps_data_class$lv_type)
  }

  # flps_model
  # fit FLPS ----------------------------------------------------------------
  if(!inherits(flps_model, "stanmodel")) {

    message("Compiling Stan code...")

    flps_model <- rstan::stan_model(model_code = flps_model)
  }

  # STVAL
  # init.rlnorm <- function(n, m, v) {
  #   a1 <- rlnorm(n, m, v)
  #   a1[abs(a1) > 5] = 1
  #   a1
  # }
  #
  # initf1 <- function() {
  #   list(lambda_free =
  #          array(init.rlnorm(sdat$nsec, 0,1), dim = c(sdat$nsec,1)))
  # }
  # stan_options$init <- initf1

  ## S3
  stan_options <- stanOptions(stan_options,
                              data = flps_data_class$stan_data,
                              object = flps_model)

  # Prior setting
  # argslist$lv_model <- paste0("F =~ ", paste(paste0("v", 1:10), collapse = "+"))
  stan_options <- setPriors(priors_input, lv_model, stan_options)

  flps_fit <-  try(do.call(rstan::sampling, stan_options))

  if(inherits(flps_fit, "try-error")) {

    message("Initial run failed, and re-compile and run.")

    flps_model <- loadRstan(lv_type = flps_data_class$lv_type, T)
    flps_model <- rstan::stan_model(model_code = flps_model)
    stan_options <- stanOptions(stan_options,
                                data = flps_data_class$stan_data,
                                object = flps_model)

    flps_fit <-  try(do.call(rstan::sampling, stan_options))
  }




  # class output ---------------------------------------------------

  ## S3
  o <- S3class("flps")
  o$call       <- .call
  o$inp_data   <- inp_data
  o$flps_model <- flps_model
  o$flps_data  <- flps_data_class
  o$flps_fit   <- flps_fit

  o$time <- c("Timing:" = as.numeric(proc.time()[3L] - start.time))

  return(o)
}
