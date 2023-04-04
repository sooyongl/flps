#' @noRd
.stanOptions <- list(
  file = NA,
  model_name = "flps_model",
  fit = NA,
  pars = NA,
  chains = 1,
  iter = 10000,
  warmup = 2000,
  thin = 1,
  init = "random",
  seed = sample.int(.Machine$integer.max, 1),
  algorithm = c("NUTS","HMC", "Fixed_param"),
  control = NULL,
  sample_file = NULL,
  diagnostic_file = NULL,
  save_dso = TRUE,
  verbose = FALSE,
  include = TRUE,
  cores = getOption("mc.cores", 1L),
  open_progress = interactive() &&!isatty(stdout()) && !identical(Sys.getenv("RSTUDIO"),"1"),
  boost_lib = NULL,
  eigen_lib = NULL
)

#' @noRd
.onAttach <- function(libname, pkgname) {

  if (packageVersion("rstan") < "2.8.0") {
    stop("Install the latest version of rstan")
  }

  packageStartupMessage("It is a demo")

}

#' #' @noRd
#' .onLoad <- function(...) {
#'   .S3method("summary","flps")
#'   .S3method("print","flps")
#' }
