#' make NULL S3 class
#' @noRd
S3class <- function(...) {

  out <- structure(list(), class = unlist(list(...)))
  out
}

#' Obtain the name of items of measurement model
#' @noRd
getMeasurementItems <- function(lv_model) {
  lv_model1 <- unlist(strsplit(lv_model, "\n"))

  nfac <- length(grep("=~", lv_model1))

  lv_model2 <- do.call("rbind",strsplit(lv_model1, "=~"))
  lv_model2 <- gsub(' |[\t\n]','',lv_model2)

  fname <- lv_model2[,1]

  item_factor <- strsplit(lv_model2[,2], "\\+")
  item_factor <- item_factor[sapply(item_factor, function(x) length(x)!=0)]

  lv_model3 <- unlist(strsplit(lv_model2[, 2], "\\+"))
  lv_model4 <- unlist(strsplit(lv_model3, " "))

  return(list(nfac=nfac, item_name=lv_model4, item_factor = item_factor, fname = fname))
}

#' obtain the signs of factor loadings
#' @noRd
obv_lambda <- function(obs.v.partial, a_idx) {

  nitem <- nrow(a_idx)
  nfac <- ncol(a_idx)

  fs.prior.info <- apply(obs.v.partial, 2, function(x) {
    cor(x, rowMeans(obs.v.partial, na.rm = T), use = "pairwise.complete.obs")
  })

  fs.prior.info[which(fs.prior.info > 0)] <- 1
  fs.prior.info[which(fs.prior.info < 0)] <- -1
  fs.prior.info[which(is.na(fs.prior.info))] <- 0

  temp_idx <- lapply(1:ncol(a_idx),  function(x) which(a_idx[, x] == 1))

  a1 <- matrix(rep(0, nitem*nfac), ncol=nfac)
  for(x in 1:nfac) {
    a1[temp_idx[[x]],x] <- fs.prior.info[temp_idx[[x]]]

  }

  a1
}

#' @noRd
gen_a_idx <- function(item_factor, nfac) {

  if(length(item_factor) != nfac)
    stop("The number of factors inconsistent with the syntax")

  idx_ <- sapply(item_factor, length)
  nitem <- sum(idx_)
  idx_c <- c(0,cumsum(idx_))

  a    <- matrix(rep(0, nitem*nfac), ncol=nfac)
  a_idx <- matrix(rep(0, nitem*nfac), ncol=nfac)
  for(j in 1:nfac) { # j=1
    a_idx[(idx_c[j]+1):idx_c[(j+1)],j] <- 1
  }
  a_idx
}

#' @noRd
detect_firstitem <- function(lambda_idx) {
  first_item <- apply(lambda_idx, 2, function(x) {which(x == 1)[1]})
  first_item_idx <- rep(0,nrow(lambda_idx))
  first_item_idx[first_item] <- 1
  first_item_idx
}

#' @noRd
str_split_and <- function(x, char) {
  for(i in 1:length(char)) {
    x <- x[grepl(char[i], x)]
  }

  return(x)
}

#' Set the options of rstan
#' stanOptions(stan_options = list());
#' @noRd
stanOptions <- function(stan_options, ...) {

  # replace a missing value in a list -------------
  dots <- list(...)

  if(!"chains" %in% names(stan_options)) {
    stan_options$chains <- 1
  }

  if(!"iter" %in% names(stan_options)) {
    stan_options$iter <- 8000
    stan_options$warmup <- 4000

  } else {

    if(!"warmup" %in% names(stan_options)) {
      stan_options$warmup <- floor(stan_options$iter / 2)
    }
  }

  for(i in 1:length(dots)){
    stan_options[[names(dots)[i]]] <- dots[[i]]
  }

  return(stan_options)
}
