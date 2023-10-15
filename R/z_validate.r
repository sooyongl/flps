#' @noRd
validate_data <- function(all_args) {

  inp_data <- all_args$inp_data
  custom_data <- all_args$custom_data
  custom_stan <- all_args$custom_stan

  covariate <- all_args$covariate
  multilevel <- all_args$multilevel

  lv_type <- all_args$lv_type
  lv_model <- all_args$lv_model
  nclass <- all_args$nclass
  # validate ----------------------------------------------------------------
  # Input data block----------------------------
  if(is.null(inp_data) && is.null(custom_data))
    stop("Data is not provided.")

  if((!is.null(custom_data) && is.null(custom_stan)) |
     (is.null(custom_data) && !is.null(custom_stan)))
    stop("Custom data and custome stan code must be provided at the same time!")

  # Covariate block----------------------------
  if(is.null(covariate)) {
    stop("Covariates must be provided")
  }

  # Multilevel block----------------------------
  if(multilevel) {
    stop("Multilevel structure will be supported")
  }

  # Mixture model block----------------------------
  if(is.null(nclass)) {
    if(tolower(lv_type) %in% c("lca","lpa")) {

      stop("For mixture models, nclass must be specified")
    }
  } else {
    if(nclass > 2) {

      stop("Number of latent classes is limited to two")
    }
  }

  # Latent model script block ----------------------------
  var_string <- gsub(".*=~\\s*", "", lv_model)
  vars <- unlist(strsplit(gsub("[^a-zA-Z0-9]", " ", var_string), "\\s+"))
  duplicates <- vars[duplicated(vars)]
  if(length(duplicates) > 0) {
    stop("The following variables are duplicated:", paste(duplicates, collapse = ", "))
  }

  # Others block ----------------------------
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
