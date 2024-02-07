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
      sim_info$fcovmat <- diag(0, sim_info$nfac)[lower.tri(diag(0, sim_info$nfac))]
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
genData <- function(N, BETA, PSI, ALPHA) {
  nr <- nrow(BETA)
  I <- diag(nr)
  I.BETA <- I-BETA
  IB.inv <- solve(I.BETA)

  COV = (IB.inv) %*% PSI %*% t(IB.inv)
  MU <- IB.inv %*% ALPHA

  data <- MASS::mvrnorm(n = N, mu = MU, Sigma = COV, empirical = TRUE)
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
