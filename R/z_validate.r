#' @noRd
validate_data <- function(all_args) {

  inp_data <- all_args$inp_data
  custom_data <- all_args$custom_data
  custom_stan <- all_args$custom_stan

  covariate <- all_args$covariate
  multilevel <- all_args$multilevel

  # validate ----------------------------------------------------------------
  if(is.null(inp_data) && is.null(custom_data))
    stop("Data is not provided.")

  if((!is.null(custom_data) && is.null(custom_stan)) |
     (is.null(custom_data) && !is.null(custom_stan)))
    stop("Custom data and custome stan code must be provided at the same time!")


  if(is.null(covariate)) {
    stop("Covariates must be provided")
  }

  if(multilevel) {
    stop("Multilevel structure will be supported")
  }
}

#' @noRd
validate_input <- function(sim_info) {



}

#' @noRd
validate_siminfo <- function(sim_info) {

  # validate ----------------------------------------------------------------
  if(length(sim_info$R2eta) != sim_info$nfac )
    # message("Mismatch: R2eta length and nfac")
    stop("Mismatch: R2eta length and nfac")

  if(length(sim_info$tau1) != sim_info$nfac )
    stop("Mismatch: tau1 length and nfac")

  if(length(sim_info$omega) != sim_info$nfac )
    stop("Mismatch: omega length and nfac")

}
