#' Set latent variable model information
#' @noRd
genLVinfo <- function(sim_info) {

  nitem   <- sim_info$nitem
  nfac    <- sim_info$nfac
  lvmodel <- sim_info$lvmodel

  if(sim_info$lvmodel %in% c("rasch",'irt',"2pl","3pl","gpcm","pcm","grm","ggrm")){

    ipar <- genIRTpar(nitem, nfac, lvmodel, ncat = 4)

  } else if(sim_info$lvmodel %in% c("cfa","sem","lgm")) {

    ipar <- genSEMpar(nitem, nfac, lvmodel)
    sim_info$lvinfo$growth_mean <- c(3, 0.2)

  }  else if(sim_info$lvmodel %in% c("lpa","lca","mixture")) {}

  sim_info$lvinfo$ipar <- ipar

  return(sim_info)
}

#' Generate IRT parameters
#' @noRd
genIRTpar <- function(nitem=25, nfac=1, lvmodel, ncat = 4) {

  lvmodel <- tolower(lvmodel)

  if(ncat <= 1) {
    stop("the number of cateories should be at least 2")
  } else if(ncat == 2 & lvmodel %in% c("grm","gpcm")) {
    stop("For GRM and GPCM, cateories should be at least 3")
  }

  a_list <- gen_a(nitem, nfac)
  a <- a_list$a
  a_idx <- a_list$a_idx

  if(lvmodel %in% c("grm","gpcm","ggrm","pcm")) {
    # for the graded model, ensure that there is enough space between
    # the intercepts, otherwise closer categories will not be selected often
    # (minimum distance of 0.5 here)
    # nitem = 10;ncat = 4; a = 1
    if(lvmodel == "gpcm") {
      diffs <- t(apply(matrix(runif(nitem * (ncat-1), 0.5, 1), nitem), 1, cumsum))
      d <- -(diffs - rowMeans(diffs))
      d <- -1*d
    }
    if(lvmodel == "grm") {
      # diffs <- t(apply(matrix(runif(20*4, .3, 1), 20), 1, cumsum))
      # diffs <- -(diffs - rowMeans(diffs))
      # d <- diffs + rnorm(20)

      diffs <- t(apply(matrix(runif(nitem * (ncat-1), 0.5, 1), nitem), 1, cumsum))
      d <- -(diffs - rowMeans(diffs))

    }


    colnames(d) <- paste0("d",1:ncol(d))
    ipar <- data.frame(a, d)

  } else if(lvmodel %in% c("rasch","1pl","2pl","3pl","irt")) {

    g <- 0
    d <- c(0, rnorm(nitem-1))

    if(lvmodel %in% c("1pl","rasch")) {
      a[which(a_idx == 1)] <- 1
    } else if(lvmodel == "3pl") {
      g = runif(nitem, 0, 0.2)
    }

    ipar <- data.frame(a, d, g)
  }

  return(ipar)
}

#' Generate SEM parameters
#' @noRd
genSEMpar <- function(nitem=25, nfac=1, lvmodel, misspec = F) {

  if(lvmodel %in% c("sem","cfa")) {

    a_list <- gen_a(nitem, nfac, misspec)
    a_list$a <- unname(a_list$a)
    loading <- a_list$a
  }

  if(lvmodel %in% c("lgm")) {
    loading <- sapply(0:(nfac-1), function(i) {(0:(nitem-1))^i})
  }

  ipar <- data.frame(loading=loading)
}


#' Generate LV model data
#' @noRd
genLVM <- function(info) { # info = sim_info
  N      <- info$N
  nitem   <- info$nitem
  nfac   <- info$nfac
  lambda <- info$lambda
  item.missing <- ifelse(is.null(info$item.missing), T, info$item.missing)

  lv.gen.dt <- generateLV(info)

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

  res <- list(
    lv.par   = lv.par,
    lv.resp  = lv.resp,
    grad     = grad,
    studentM = studentM,
    section  = section
  )

  info <- structure(append(info, res), class = attr(info, "class"))

  return(info)
}

#' S3 generic for latent model data generation
#' @noRd
generateLV <- function(info, ...) {
  UseMethod("generateLV", info)
}

#' methods for rasch model
#' @noRd
generateLV.rasch <- function(.x, ...) {.Class <- "irt"; NextMethod()}

#' method for 1PL model
#' @noRd
generateLV.1pl   <- function(.x, ...) {.Class <- "irt"; NextMethod()}

#' method for 2PL model
#' @noRd
generateLV.2pl   <- function(.x, ...) {.Class <- "irt"; NextMethod()}

#' method for 3PL model
#' @noRd
generateLV.3pl   <- function(.x, ...) {.Class <- "irt"; NextMethod()}

#' method for GPCM model
#' @noRd
generateLV.gpcm   <- function(.x, ...) {.Class <- "irt"; NextMethod()}
generateLV.pcm   <- function(.x, ...) {.Class <- "irt"; NextMethod()}
#' method for GRM
#' @noRd
generateLV.ggrm   <- function(.x, ...) {.Class <- "irt"; NextMethod()}
generateLV.grm   <- function(.x, ...) {.Class <- "irt"; NextMethod()}

#' method for all IRT model
#'
#' @examples
#'
#' lvmodel <- "gpcm"
#' ipar <- genIRTpar(20, ncat = 3, 2, lvmodel)
#' eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
#' generateLV.irt(lvmodel, eta, ipar)
#' @noRd
generateLV.irt <- function(info) {

  theta <- as.matrix(info$theta, ncol = info$nfac);
  nitem <- info$nitem;
  lv_info <- info$lvinfo
  lvmodel <- tolower(info$lvmodel)
  ipar <- lv_info$ipar

  a <- ipar[grep("a",names(ipar))]
  d <- ipar[grep("d|b",names(ipar))]
  guess <- 0

  stopifnot(is.data.frame(ipar))
  stopifnot(ncol(a) == ncol(theta))

  N    <- nrow(theta)
  nfac <- ncol(theta)

  lvmodel <- switch(lvmodel,
                    "rasch" = "dich",
                    "1pl" = "dich",
                    "2pl" = "dich",
                    "3pl" = "dich",
                    "gpcm" = "gpcm",
                    "pcm" = "gpcm",
                    "grm" = "graded",
                    "ggrm" = "graded")

  if(lvmodel == "dich") { guess <- ipar[,grep("g",names(ipar))] }

    resp <- simIRTdata(a = as.matrix(a),
                       d = as.matrix(d),
                       guess = as.vector(guess),
                       N = N,
                       theta = theta,
                       itemtype = lvmodel)
  # }
  if(lvmodel != "dich")
    resp <- resp + 1


  return(list(resp = data.frame(resp), lv.par = ipar))
}

#' Generate IRT data
#' @noRd
simIRTdata <- function(a, d, guess, N, theta, itemtype) {

  if(itemtype != "gpcm"){
    resp <- mirt::simdata(
      a = a,
      d = d,
      guess = guess,
      N = N,
      Theta = theta,
      itemtype = itemtype)

  } else {
    resp <- simData.pcm(
      a = a,
      d = d,
      theta = theta
    )
  }

  resp
}

#' Generate GPCM data
#' @noRd
simData.pcm <- function(a,d,theta) {

  N     <- nrow(theta)
  nfac  <- ncol(a)
  nitem <- nrow(a)
  ncat <- ncol(d) + 1

  difficulty = rowMeans(d)
  steps <- apply(d, 2, function(x) x - difficulty)
  steps <- cbind(difficulty, steps)

  # Create an empty response matrix
  res <- matrix(NA, nrow = N, ncol = nitem)
  # for(h in 1:nfac) { # h = 1
  for(k in 1:N) { # k = 1
    for(i in 1:nitem) { # i = 1
      measure=0
      p <- vector()
      p[1] <- 1

      for(j in 2:ncat) {
        measure <- measure + a[i, 1]*theta[k, 1] - d[i, j-1]
        p[j] <- p[(j-1)] + exp(measure)
      }

      U <- runif(1, 0, 1)
      U = U * p[ncat]

      for(j in 1:ncat) {
        if(U <= p[j]) {
          res[k,i] <- (j-1)
          break
        }
      }
    }
  }
  # }
  return(res)
}


#' method for Polytomous Response: NRM
#' @noRd
generateLV.nominal <- function(info) {
  print("not yet")
}

#' method for Polytomous Response: Response Time (Lognormal)
#' @noRd
generateLV.ln <- function(info){
  # # set up for data generation
  # tau <- info$tau; ipar <- info$ipar
  #
  # # data generation
  # nexaminee <- length(tau)
  # nitem <- nrow(ipar)
  # alp <- ipar[,"alp"]; bet <- ipar[,"bet"]
  #
  # retime <- matrix(NA, nexaminee, nitem)
  # for (j in 1:nexaminee){
  #   retime[j,] <- rlnorm(nitem, bet-tau[j], 1/alp)
  #   #m <- bet - tau[j]
  #   #s <- 1/alp
  #   #logmu <- log(m^2 / sqrt(s^2 + m^2))
  #   #logsd <- sqrt(log(1 + (s^2 / m^2)))
  # }
  # return(retime)
  print("not yet")
}

#' method for generating sem data
#' @noRd
generateLV.sem <- function(info) {

  # set up for data generation
  nfac <- info$nfac
  theta <- info$theta; nitem <- info$nitem; lv_info <- info$lvinfo
  relsize = info$relsize
  cov.res = info$cov.res

  ipar <- as.matrix(lv_info$ipar)

  loadings <- matrix(ipar[, grep("loading", colnames(ipar))], ncol = nfac)
  residuals <- diag(loadings %*% cov(matrix(theta, ncol = nfac)) %*% t(loadings)) * .4

  if(is.null(dim(theta))) {
    n_sample <- length(theta)
    theta <- matrix(theta)

  } else {
    n_sample <- dim(theta)[1]
  }

  # data generation
  latent <- tcrossprod(theta, loadings)

  res.var = ((1 - relsize) / relsize) * diag(cov(latent))
  res.var = diag(res.var)
  if(cov.res) {

    a1 <- sample(1:nfac, 4)
    a2 <- sample(1:nfac[-a1], 4)

    a3 <- cbind(a1, a2)
    for(i in 1:nrow(a3)) {

      ai <- a3[i,1]
      aj <- a3[i,2]

      cr <- runif(1, -0.2, 0.2)
      res.var[ai,aj] <- cr
      res.var[aj,ai] <- cr
    }

  }

  residuals <- MASS::mvrnorm(n_sample,
                             rep(0, nitem),
                             res.var,
                             empirical = T)


  resp <- latent + residuals

  return(list(resp = resp, lv.par = loadings))
}

#' method for generating lgm data
#' @noRd
generateLV.lgm <- function(info) {

  # set up for data generation
  theta <- info$theta; ntp <- info$nitem; lv_info <- info$lvinfo

  loadings <- lv_info$time_loading
  # residuals <- diag(loadings %*% cov(theta) %*% t(loadings)) * .4
  residuals <- diag(var(theta[,1]) * .4, nrow(loadings))

  if(is.null(dim(theta))) {
    n_sample <- length(theta)
    theta <- matrix(theta)

  } else {
    n_sample <- dim(theta)[1]
  }

  residuals <- MASS::mvrnorm(n = n_sample,
                             mu = rep(0, nrow(loadings)),
                             Sigma = residuals,
                             empirical = T)

  # data generation
  latent <- tcrossprod(theta, loadings)
  resp <- latent + residuals

  # test -------------------------
  # library(lavaan)
  # cov(theta); colMeans(theta)
  # data.frame(resp,sim_info$x) %>% growth(model = "I =~ 1*X1+1*X2+1*X3+1*X4+1*X5+1*X6+1*X7+1*X8+1*X9+1*X10+1*X11+1*X12+1*X13+1*X14+1*X15+1*X16+1*X17+1*X18+1*X19+1*X20;S =~ 0*X1+1*X2+2*X3+3*X4+4*X5+5*X6+6*X7+7*X8+8*X9+9*X10+10*X11+11*X12+12*X13+13*X14+14*X15+15*X16+16*X17+17*X18+18*X19+19*X20;I ~ x1 + x2;S ~ x1 + x2", data = .) %>% summary()


  return(list(resp = resp, lv.par = loadings))
}

## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##
##                   Mixture model                 ##
## #~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~# ##
#' @noRd
class_assign <- function(...) {
  eta <- list(...)

  # eta$ref <- rep(0, length(eta[[1]]))
  # eta <- lapply(eta, exp)
  # eta <- do.call("cbind", eta)
  #
  # sum_eta <- apply(eta, 1, function(x) Reduce(sum, x))
  # clasS_prob <- apply(eta, 2, function(x) x / (sum_eta))
  #
  # assignment <- apply(clasS_prob, 1, function(x) { (dim(clasS_prob)[2] + 1) - which.max(x)})
  #
  # assignment

  # vector of probabilities
  vProb = cbind(exp(0), exp(eta[[1]]))

  # multinomial draws
  mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
  assignment = cbind.data.frame(class = apply(mChoices, 1, function(x) which(x==1)))

  assignment
}

# https://mc-stan.org/users/documentation/case-studies/Latent_class_case_study.html#data-generation-and-label-switching

#' generate lca data
#' @noRd
generateLV.lca <- function(info) {

  # n_class <- info$n_class

  n_indi <- info$nitem
  theta <- info$theta
  # seperation <- info$seperation
  n_class <- info$lvinfo$nclass
  seperation <- info$lvinfo$separation
  seperation <- c(seperation, 1- seperation)

  latent_class <- class_assign(theta)
  class_prop <- table(latent_class)

  # Measurement models within classes
  data <- lapply(1:length(class_prop), function(i) {

    class_n_size <- class_prop[i]

    row_data <- mvtnorm::rmvnorm(n = class_n_size,
                                 mean = rep(0, n_indi),
                                 sigma = diag(n_indi))

    continuousData <- row_data
    threshold = c(1 - seperation[i])

    quants <- sapply(1:(dim(continuousData)[2]), function(i) {
      quantile(continuousData[,i], probs = threshold[1])
    })

    ordinalData <- sapply(1:n_indi, function(i) {
      as.numeric(cut(as.vector(continuousData[,i]),c(-Inf,quants[i],Inf)))
    })

    useData <- data.frame(ordinalData)
    names(useData) <- paste0("x", 1:dim(useData)[2])

    useData$class <- i

    useData
  })
  resp <- do.call('rbind', data)
  resp <- resp - 1

  idx <- data.frame(latent_class, id = 1:nrow(latent_class))
  idx <- idx[order(idx$class),]

  resp$id <- idx$id
  resp <- resp[order(resp$id),]
  resp$id <- NULL

  return(list(lv.par = seperation, resp = as.matrix(resp)))
}

#' genLV lpa data
#' @noRd
generateLV.lpa <- function(info) {

  n_indi <- info$nitem
  theta <- info$theta
  n_class <- info$lvinfo$nclass
  separation <- info$lvinfo$separation # this is Mahalanobis Distance; James Peugh & Xitao Fan(2013)

  if(separation == 1) {
    mean_list <- list()

    mean_list[[1]] <- rep(1, n_indi)
    mean_list[[2]] <- rep(1.41, n_indi)

  }
  if(separation == 2) {
    mean_list <- list()

    mean_list[[1]] <- rep(1, n_indi)
    mean_list[[2]] <- rep(1.58, n_indi)

  }

  latent_class <- class_assign(theta)
  class_prop <- table(latent_class)
  #
  # # Measurement models within classes
  data <- lapply(1:length(class_prop), function(i) {

    class_n_size <- class_prop[i]

    row_data <- mvtnorm::rmvnorm(n = class_n_size,
                                 mean = mean_list[[i]],
                                 sigma = diag(n_indi))

    useData <- data.frame(row_data)

    useData$class <- i
    names(useData) <- paste0("x", 1:dim(useData)[2], "class")
    useData
  })
  #
  # data <- do.call('rbind', data)
  #
  # return(data)

  resp <- do.call('rbind', data)

  idx <- data.frame(latent_class, id = 1:nrow(latent_class))
  idx <- idx[order(idx$class),]

  resp$id <- idx$id
  resp <- resp[order(resp$id),]
  resp$id <- NULL

  return(list(lv.par = separation, resp = as.matrix(resp)))
}

#' generate general mixture data
#' @noRd
generateLV.mixture <- function(info) {

  # return(data)
  print("not yet")
}
