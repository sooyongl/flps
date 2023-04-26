#' make NULL S3 class
#' @noRd
S3class <- function(class) {
  out <- structure(list(), class = class)
  out
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
addDefault <- function(sim_info) {

  if(!"nfac" %in% names(sim_info)) {
    sim_info$nfac <- 1
  }

  if(!"linear" %in% names(sim_info)) {
    sim_info$linear <- T
  }

  if(!"yidst" %in% names(sim_info)) {
    sim_info$yidst <- 'n'
  }

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

    if(sim_info$nfac > 1) {
      sim_info$fcovmat <- diag(0, nfac)[lower.tri(diag(0, nfac))]
    } else {
      sim_info$fcovmat <- NULL
    }
  }

  if(!"item.missing" %in% names(sim_info)) {
    sim_info$item.missing <- T
  }


  sim_info
}

#' @noRd
gen_a <- function(nitem, nfac, misspec = F) {
  # nitem = 20
  # nfac = 2

  idx_ <- rep(floor(nitem / nfac),nfac)
  idx_[length(idx_)] <- nitem - sum(idx_[-length(idx_)])
  idx_c <- c(0,cumsum(idx_))
  a    <- matrix(rep(0, nitem*nfac), ncol=nfac)
  a_idx <- matrix(rep(0, nitem*nfac), ncol=nfac)
  for(j in 1:nfac) { # j=1
    a_idx[(idx_c[j]+1):idx_c[(j+1)],j] <- 1
    # The first 1 here is the recommended constraint
    a[(idx_c[j]+1):idx_c[(j+1)],j] <- c(1, matrix(rlnorm((idx_[(j)]-1), .1, .3)))

    if(misspec) {
      a[-c((idx_c[j]+1):idx_c[(j+1)]),j] <- runif(length(a[-c((idx_c[j]+1):idx_c[(j+1)]),j]), 0.2, 0.2)
    }

  }
  colnames(a) <- paste0("a",1:ncol(a))

  list(a_idx = a_idx, a = a)
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

  if(!"chain" %in% names(stan_options)) {
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

#' @noRd
genData <- function(N, BETA, PSI, ALPHA) {
  nr <- nrow(BETA)
  I <- diag(nr)
  I.BETA <- I-BETA
  IB.inv <- solve(I.BETA)

  COV = (IB.inv) %*% PSI %*% t(IB.inv)
  MU <- IB.inv %*% ALPHA

  data <- MASS::mvrnorm(n = N, mu = MU, Sigma = COV, empirical = T)
  data <- data.frame(data)

  names(data) <- c("Y","X",paste0("eta", 1:(ncol(data)-2)))

  # list(data, COV, MU)
  data
}

#' @noRd
makeStructureData <- function(N, YRes, tau0, omega, inteff, xtol, xtoy, EtaRes, fcov) {

  nfac = length(EtaRes)
  ncov = length(xtoy)

  xtol = matrix(xtol, ncol = ncov)

  acov <- rep(0, ncov)
  afac <- rep(0, nfac)

  Alpha1 = matrix(c(tau0,acov,afac), ncol = 1)
  Alpha2 = matrix(c(0,acov,afac), ncol = 1)

  ccov <- diag(1,ncov)

  Psi <- diag(0, 1+ncov+nfac)

  if(nfac == 1) {
    temp_m <- EtaRes

  } else {
    temp_m <- diag(EtaRes)
    temp_m[lower.tri(temp_m)] <- fcov
    temp_m[upper.tri(temp_m)] <- t(temp_m)[upper.tri(temp_m)]
  }


  Psi[1,1] <- YRes
  Psi[2:(2+ncov-1), 2:(2+ncov-1)] <- ccov
  Psi[(2+ncov):(1+ncov+nfac), (2+ncov):(1+ncov+nfac)] <- temp_m

  Beta1 <- diag(0, 1+ncov+nfac)

  Beta1[1,2:(2+ncov-1)] <- xtoy
  Beta1[1,(2+ncov):(1+ncov+nfac)] <- inteff
  Beta1[(2+ncov):(1+ncov+nfac), 2:(2+ncov-1)] <- xtol

  Beta2 <- diag(0, 1+ncov+nfac)

  Beta2[1,2:(2+ncov-1)] <- xtoy
  Beta2[1,(2+ncov):(1+ncov+nfac)] <- omega
  Beta2[(2+ncov):(1+ncov+nfac), 2:(2+ncov-1)] <- xtol

  d1 <- genData(N/2, Beta1, Psi, Alpha1)
  d1$Z <- 1
  d2 <- genData(N/2, Beta2, Psi, Alpha2)
  d2$Z <- 0
  data <- rbind(d1, d2)

  data <- data[, c(which(names(data)=="Y"),
                   which(names(data)=="Z"),
                   which(!names(data) %in% c("Y","Z")))]

  names(data) <- c("Y","Z", paste0("X", 1:ncov), paste0("eta", 1:nfac))

  data
}
