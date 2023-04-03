#' @noRd
genStanData <- function(Data) {

  N      <- Data$N
  nitem   <- Data$nitem
  theta  <- Data$theta
  xdata  <- Data$x
  nfac   <- Data$nfac

  R2eta    <- Data$R2eta
  R2Y    <- Data$R2Y
  linear <- Data$linear
  ydist  <- Data$ydist

  omega  <- Data$omega # round(runif(nfac, 0.1, 0.3),3)
  tau0   <- Data$tau0  # round(runif(1, 0.2, 0.4),3)
  tau1   <- Data$tau1  # round(runif(nfac, -0.2, -0.1),3)

  section  <- Data$section
  studentM <- Data$studentM
  grad     <- Data$grad
  lv.resp  <- Data$lv.resp

  a_idx <- gen_a_idx(nitem, nfac)
  fi_idx <- detect_firstitem(a_idx)

  xdata <- Data$struc_data[, which(names(Data$struc_data)=="X")]
  xdata <- as.matrix(xdata)

  Z <- Data$struc_data[, which(names(Data$struc_data)=="Z")]
  Z <- c(Z)

  Y <- Data$struc_data[, which(names(Data$struc_data)=="Y")]
  Y <- c(Y)


  Data$stan_dt <- list(
    # data info
    nitemWorked = length(section),
    nstud      = N,
    nitem       = nitem,
    nfac       = nfac,
    min_k      = min(grad),
    max_k      = max(grad),
    ncov       = ncol(xdata),
    # index
    studentM     = studentM,
    section      = section,
    lambda_prior = obv_lambda(obs.v.partial = lv.resp[1:(N/2), ], a_idx),
    factoridx    = a_idx,
    firstitem    = fi_idx,
    # data
    grad   = grad,
    X      = xdata,
    Z      = Z,
    Y      = c(Y)
  )
  Data
}
