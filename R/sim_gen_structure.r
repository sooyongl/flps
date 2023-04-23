#' @noRd
genStructure <- function(sim_info) {

  N = sim_info$N
  tau0 = sim_info$tau0
  tau1 = sim_info$tau1
  omega = sim_info$omega
  xtoy = sim_info$betaY
  xtol = sim_info$betaL

  inteff = tau1 + omega
  YRes = 1 - sim_info$R2Y
  EtaRes = 1 - sim_info$R2eta
  fcov = sim_info$fcovmat

  xtol <- xtol
  xtoy <- xtoy

  data0 <- makeStructureData(N, YRes, tau0, omega, inteff, xtol, xtoy, EtaRes, fcov)

  sim_info$betaU <- xtol
  sim_info$betaY <- xtoy
  sim_info$struc_data <- data0

  sim_info$theta <- data0[, grepl("^eta", names(data0))]
  sim_info$theta <- as.matrix(sim_info$theta)


  sim_info
}

#' @noRd
genMeasurement <- function(sim_info) {

  struc_data <- sim_info$struc_data
  theta <- sim_info$theta

  N      <- sim_info$N
  nitem   <- sim_info$nitem
  nfac   <- sim_info$nfac
  lambda <- sim_info$lambda
  item.missing <- ifelse(is.null(sim_info$item.missing), T, sim_info$item.missing)

  lv.gen.dt <- generateLV(sim_info)

  lv.par <- lv.gen.dt$lv.par
  lv.resp <- lv.gen.dt$resp

  if(item.missing) {
    total_N <- N/2
  } else {
    total_N <- N
  }

  nworked <- rep(floor(nitem * lambda), total_N)

  studentM <- do.call("c", lapply(seq(total_N),
                                  function(n) rep(n,each=nworked[n])))
  section <- do.call("c", lapply(seq(total_N),
                                 function(n) {
                                   sort(sample(1:nitem, nworked[n],
                                               replace = FALSE))}))
  ss <- cbind(studentM, section)
  grad <- sapply(1:dim(ss)[1], function(n) lv.resp[ss[n,1], ss[n,2]] )

  colnames(lv.resp) <- paste0("v", 1:ncol(lv.resp))

  res <- list(
    lv.par   = lv.par,
    lv.resp  = lv.resp,
    grad     = grad,
    studentM = studentM,
    section  = section
  )

  sim_info <- structure(append(sim_info, res),
                        class = attr(sim_info, "class"))

  return(sim_info)

}
