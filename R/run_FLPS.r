#' Conduct fully latent principal stratification
#'
#' @param inp_data A matrix or data frame containing the input data.
#' @param compiled_stan An object of S4 class stanmodel produced by the
#'  \code{modelBuilder} function.
#' @param outcome A character string specifying the outcome variable's name.
#' @param trt A character string specifying the treatment or control group variable's name.
#' @param covariate A character string specifying the covariate variable names.
#' @param lv_model A description of the latent variable model using syntax
#' akin to the \pkg{lavaan} package. Key operators include:
#'  \itemize{
#'    \item \code{=~} : Denotes associations between factors and indicators (e.g., F1 =~ v1 + v2 + v3).
#'    \item \code{+} : Specifies a series of indicators.
#'  }
#'
#' @param lv_type A character string indicating the type of latent variable models.
#' @param multilevel A logical indicating if a multilevel structure is present.
#' @param lv_randomeffect A logical indicating whether to estimate random effects for latent variables.
#' @param priors_input A list specifying the priors or defaults to N(0, 5) if not provided.
#' Relevant parameters: \code{tau0} (group difference), \code{tau1} (principal effects),
#' and \code{omega} (effect of latent factors on outcome).
#' Ensure that the lengths of \code{tau1} and \code{omega} match the number of factors.
#' Examples:
#'  \itemize{
#'    \item \code{list(tau0 = c(0, 1), tau1 = c(0.5, 1))} : Mean and variance for normal priors.
#'    \item \code{list(tau1 = list(c(0.5, 1), c(-0.4, 1)))} : For two factors.
#'  }
#'
#' @param stan_options A list of options for [rstan::stan()], specified as 'name = value'.
#' @param ... Additional parameters for the latent variable models, such as \code{nclass = 2}.
#' @return An object of class \code{flps} encompassing a \code{\link[rstan]{stanfit}} object.
#' Components include:
#'  \item{call}{Function call with arguments.}
#'  \item{inp_data}{The input data frame provided.}
#'  \item{flps_model}{The Stan syntax used in [rstan::stan()].}
#'  \item{flps_data}{Data list used for [rstan::stan()].}
#'  \item{flps_fit}{Resulting \code{\link[rstan]{stanfit}} object.}
#'  \item{time}{A numeric; Time taken for computation}
#'
#'
#' @examples
#' \donttest{
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
#'    trt = "Z",
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
                    compiled_stan = NULL,
                    outcome = NULL,
                    trt = NULL,
                    covariate = NULL,
                    lv_model = NULL,
                    lv_type = NULL,
                    multilevel = FALSE,
                    lv_randomeffect = FALSE,
                    priors_input = NULL,
                    stan_options = list(),
                    ...
) {

  # time record --------------------------------------------------------
  start.time <- proc.time()[3L]

  # call ---------------------------------------------------------------
  .call <- match.call()
  argslist <- as.list(.call[-1])
  all_args <- as.list(environment())
  all_args <- append(all_args, list(...))

  # validate -----------------------------------------------------------
  validate_data(all_args)

  # data and code -------------------------------------------------------
  flps_data_class <- makeFLPSdata(inp_data, outcome, trt, covariate,
                                  lv_model, lv_type, multilevel,
                                  nclass = all_args$nclass,
                                  group_id = all_args$group_id)

  if(is.null(compiled_stan)) {
    flps_model <- loadRstan(flps_data_class$lv_type, multilevel, lv_randomeffect)

  } else {
    flps_model <- compiled_stan
  }

  # flps_model
  # fit FLPS --------------------------------------------------------------
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


  if(!inherits(flps_model, "stanmodel")) {
    message("Compiling Stan code...")
    ## S3
    stan_options <- stanOptions(stan_options, model_code = flps_model,
                                data = flps_data_class$stan_data)

    # Prior setting
    stan_options <- setPriors(priors_input, lv_model, stan_options)
    flps_fit <-  try(do.call(rstan::stan, stan_options))

  } else {

    stan_options <- stanOptions(stan_options, object = flps_model,
                                data = flps_data_class$stan_data)

    # Prior setting
    stan_options <- setPriors(priors_input, lv_model, stan_options)
    flps_fit <-  try(do.call(rstan::sampling, stan_options))
  }


  if(inherits(flps_fit, "try-error")) {

    message("Initial run failed, and re-compile and run.")

    flps_model <- loadRstan(lv_type = flps_data_class$lv_type, TRUE)

    ## S3
    stan_options <- stanOptions(stan_options, model_code = flps_model,
                                data = flps_data_class$stan_data)

    # Prior setting
    stan_options <- setPriors(priors_input, lv_model, stan_options)
    flps_fit <-  try(do.call(rstan::stan, stan_options))
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
