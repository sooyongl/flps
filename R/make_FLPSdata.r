#' Convert a matrix to a FLPS data
#'
#' @param inp_data A matrix or a data frame
#' @param outcome A character indicating the name of an outcome variable
#' @param trt A character indicating the name of a treatment/control group variable
#' @param covariate A character indicating the names of covariates variables
#' @param lv_model A description of the latent variable model, which is similar to the lavaan model syntax.
#' @param lv_type  A character indicating the type of latent variable models
#' @param ... Additional arguments for latent variable models information (e.g., nclass = 2).
#'
#' @returns a S3 class for a corresponding measurement model
#' @noRd
makeFLPSdata <- function(inp_data, outcome, trt, covariate, lv_model, lv_type, ...) {
  # flps_data <- dataSetting() ; S3 class
  dotdotdot <- list(...)

  inp_data <- data.frame(inp_data)

  outcome.data <- unname(unlist(inp_data[outcome]))
  trt.data <- unname(unlist(inp_data[trt]))
  covariate.data <- inp_data[covariate]


  lv_model1 <- unlist(strsplit(lv_model, "\n"))

  nfac <- length(grep("=~", lv_model1))

  lv_model2 <- do.call("rbind",strsplit(lv_model1, "=~"))
  lv_model2 <- gsub(' |[\t\n]','',lv_model2)

  item_factor <- strsplit(lv_model2[,2], "\\+")
  item_factor <- item_factor[sapply(item_factor, function(x) length(x)!=0)]

  lv_model3 <- unlist(strsplit(lv_model2[, 2], "\\+"))
  lv_model4 <- unlist(strsplit(lv_model3, " "))

  obs.v.name <- lv_model4[lv_model4 != ""]
  obs.v.matrix <- inp_data[obs.v.name]

  obs.v.partial <- obs.v.matrix[trt.data == 1, ]

  nitem <- ncol(obs.v.partial)
  nstu <- nrow(obs.v.matrix)

  obs.v.idx <- which(!is.na(obs.v.partial), arr.ind = TRUE)

  obs.v.vector <- sapply(1:nrow(obs.v.idx),
                         function(n) obs.v.matrix[obs.v.idx[n,1], obs.v.idx[n,2]])

  a_idx <- gen_a_idx(item_factor, nfac = nfac) # temporary 1
  fi_idx <- detect_firstitem(a_idx)

  flps_data <- list(
    nitemWorked = length(obs.v.idx[,2]),
    nstud = nstu,
    nitem = nitem,

    stud_idx = unname(obs.v.idx[,1]),
    item_idx = unname(obs.v.idx[,2]),

    grad = obs.v.vector,
    X = covariate.data,
    ncov = ncol(covariate.data),

    Z = trt.data,
    Y = outcome.data,

    firstitem = fi_idx,
    factoridx = a_idx,
    nfac = nfac
  )

  if(TRUE) {
    obtain_prior <- match.fun("obv_lambda")
  } else {
    obtain_prior <- match.fun("latent_lambda")
  }

  lv_type <- toupper(lv_type)
  if(lv_type %in% c("IRT","RASCH","2PL","3PL")) {
    flps_data$loading_prior <- obtain_prior(obs.v.partial, a_idx)
    flps_data$min_k <- min(obs.v.vector)
    flps_data$max_k <- max(obs.v.vector)
    ## S3
    out <- S3class("flpsIRT")

    ## S4
    # out <- new("flpsIRT")
  }

  if(lv_type %in% c("GGRM","GRM")) {
    flps_data$loading_prior <- obtain_prior(obs.v.partial, a_idx)
    flps_data$min_k <- min(obs.v.vector)
    flps_data$max_k <- max(obs.v.vector)

    ## S3
    out <- S3class("flpsGRM")
  }

  if(lv_type %in% c("GPCM","PCM","RSM")) {
    flps_data$loading_prior <- obtain_prior(obs.v.partial, a_idx)
    flps_data$min_k <- min(obs.v.vector)
    flps_data$max_k <- max(obs.v.vector)

    ## S3
    out <- S3class("flpsGPCM")
  }

  if(lv_type %in% c("SEM","CFA")) {
    flps_data$loading_prior <- obtain_prior(obs.v.partial, a_idx)

    ## S3
    out <- S3class("flpsSEM")
  }

  if(lv_type %in% c("LGM")) {
    flps_data$time_loading <- dotdotdot$time_loading

    ## S3
    out <- S3class("flpsLGM")

  }

  if(lv_type %in% c("LPA","LCA","MIXTURE","GMM")) {
    flps_data$nclass <- dotdotdot$nclass
    flps_data$min_k <- min(obs.v.vector)
    flps_data$max_k <- max(obs.v.vector)

    ## S3
    out <- S3class("flpsMixture")
    out$nclass <- dotdotdot$nclass
  }

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

