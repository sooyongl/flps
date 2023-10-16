#' Convert a matrix to a FLPS data
#'
#' @param inp_data A matrix or a data frame
#' @param outcome A character indicating the name of an outcome variable
#' @param trt A character indicating the name of a treatment/control group variable
#' @param covariate A character indicating the names of covariates variables
#' @param lv_model A description of the latent variable model, which is similar to the lavaan model syntax.
#' @param lv_type  A character indicating the type of latent variable models
#' @param multilevel A logical specifying multilevel structure.
#' @param ... Additional arguments for latent variable models information (e.g., nclass = 2).
#'
#' @returns a S3 class for a corresponding measurement model
#' @noRd

# makeFLPSdata <- function(...) {
#   dotdotdot <- list(...)
#   print(dotdotdot$group_id)
# }
# makeFLPSdata(group_id = 1)

makeFLPSdata <- function(inp_data, outcome, trt, covariate, lv_model, lv_type, multilevel,
                         # nclass = NULL, group_id = NULL
                         ...
                         ) {
  # flps_data <- dataSetting() ; S3 class

  class_name_1 <- paste0('flps', toupper(lv_type))
  class_name_2 <- ifelse(multilevel, "Multi", "Single")

  #
  # dotdotdot <- list(nclass = nclass, group_id = group_id)
  dotdotdot <- list(...)

  #
  inp_data <- data.frame(inp_data)
  outcome.data <- unname(unlist(inp_data[outcome]))
  trt.data <- unname(unlist(inp_data[trt]))

  #
  lv_info <- getMeasurementItems(lv_model)

  obs.v.name <- lv_info$item_name[lv_info$item_name != ""]
  obs.v.matrix <- inp_data[obs.v.name]

  obs.v.partial <- obs.v.matrix # [trt.data == 1, ]

  nitem <- ncol(obs.v.partial)
  nstu <- nrow(obs.v.matrix)

  obs.v.idx <- which(!is.na(obs.v.partial), arr.ind = TRUE)

  obs.v.vector <- sapply(1:nrow(obs.v.idx),
                         function(n) obs.v.matrix[obs.v.idx[n,1], obs.v.idx[n,2]])

  a_idx <- gen_a_idx(lv_info$item_factor, nfac = lv_info$nfac) # temporary 1
  fi_idx <- detect_firstitem(a_idx)

  #
  if(TRUE) {
    obtain_prior <- match.fun("obv_lambda")
  } else {
    obtain_prior <- match.fun("latent_lambda")
  }


  #
  if(multilevel) {
    group_id <- dotdotdot$group_id

    group_id_data <- inp_data[[group_id]]
    covariate.data_1 <- inp_data[covariate[[1]]]
    covariate.data_2 <- inp_data[c(group_id,  covariate[[2]])]

    covariate.data_2 <- covariate.data_2[!duplicated(covariate.data_2), ]
    covariate.data_2[[group_id]] <- NULL

    # nrow(covariate.data_2)
    formula_obj <- as.formula(paste(trt, "~", group_id))
    cm_Z <- aggregate(formula_obj,data = inp_data, FUN = mean)
    cm_Z <- cm_Z[[2]]

    # inp_data[[group_id]] <- as.numeric(as.factor(inp_data[[group_id]]))
    # group_indices <- split(1:nrow(inp_data), inp_data[[group_id]])

    flps_data <- list(
      # Data info
      nitemWorked = length(obs.v.idx[,2]),
      nstud = nstu,
      nitem = nitem,
      nfac = lv_info$nfac,
      nclass = dotdotdot$nclass,
      min_k = min(obs.v.vector),
      max_k = max(obs.v.vector),
      ncov_lv1   = ncol(covariate.data_1),
      ncov_lv2   = ncol(covariate.data_2),

      # Data Index
      stud_idx = unname(obs.v.idx[,1]),
      item_idx = unname(obs.v.idx[,2]),

      # Group Index
      nsch = length(unique(group_id_data)),
      sch  = group_id_data,

      # index for factor loadings
      loading_prior = obtain_prior(obs.v.partial, a_idx),
      firstitem = fi_idx,
      factoridx = a_idx,

      # Data
      grad = obs.v.vector,
      X = covariate.data_1 %>% as.matrix(),
      cm_X = covariate.data_2 %>% as.matrix(),
      cm_Z = cm_Z,
      Z = trt.data,
      Y = outcome.data
    )

  } else {
    covariate.data <- inp_data[covariate]

    flps_data <- list(
      # Data info
      nitemWorked = length(obs.v.idx[,2]),
      nstud = nstu,
      nitem = nitem,
      nfac = lv_info$nfac,
      nclass = dotdotdot$nclass,
      min_k = min(obs.v.vector),
      max_k = max(obs.v.vector),
      ncov = ncol(covariate.data),

      # Data Index
      stud_idx = unname(obs.v.idx[,1]),
      item_idx = unname(obs.v.idx[,2]),

      # index for factor loadings
      loading_prior = obtain_prior(obs.v.partial, a_idx),
      firstitem = fi_idx,
      factoridx = a_idx,

      # Data
      grad = obs.v.vector,
      X = covariate.data,
      Z = trt.data,
      Y = outcome.data
    )
  }

  out <- S3class(class_name_1, class_name_2)
  ## S3
  out$outcome <- outcome
  out$trt <- trt
  out$covariate <- covariate
  out$lv_type <- lv_type
  out$lv_model <- lv_model
  out$lv_data <- obs.v.partial
  out$stan_data <- flps_data

  return(out)
}

# dataSetting <- function(x, ...) {
#   UseMethod("dataSetting", x)
# }
#
# dataSetting.Single <- function(x, ...) {
#
# }




