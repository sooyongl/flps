#' Summary print
print.summary.flps <- function(x, type = "structures", ...) {

  calls <- as.list(x$call)
  results = x$results
  cat(toupper(calls$lv_type), ' used as a measurement model\n')
  cat('\n')

  if(is.null(calls$nclass)) {
    lv_model <- getMeasurementItems(calls$lv_model)

    cat(lv_model$nfac, ' factor model was fitted\n')
    cat('-----------------------------------------------\n\n')

  } else {
    cat(calls$nclass, 'class model was fitted\n')
    cat('-----------------------------------------------\n\n')
  }

  a1 <- results

  for(i in 1:length(a1)) {
    cat(names(a1)[i],'\n')

    print.default(as.matrix(a1[[i]]),
                  quote = FALSE, right = TRUE, na.print = 'NA')

    cat('\n')
    cat('-----------------------------------------------\n\n')
  }
}

#' Print results
#'
#' @param x an object of class \code{\link[flps]{flps}}
#' @param ... additional options for future development
#'
#' @return Summary of FLPS model are printed.
#'
#' @method print flps
#' @rdname print
#' @export
print.flps <- function(x, ...) {
  # rstan::show(x$flps_fit, ...)

  calls <- as.list(x$call)

  cat(toupper(calls$lv_type), ' used as a measurement model\n')
  cat('\n')

  if(is.null(calls$nclass)) {
    cat(getMeasurementItems(calls$lv_model)$nfac, 'factor model was fitted\n')
    cat('-----------------------------------------------\n\n')

  } else {
    cat(calls$nclass, 'class model was fitted\n')
    cat('-----------------------------------------------\n\n')
  }
}

#' Summarize the results
#'
#' @param object an object of class \code{\link[flps]{flps}}
#' @param type a string for the part of FLPS model
#'  \itemize{
#'    \item \code{structures} : prints the results of structural parts.
#'    \item \code{measurement} : prints the results of measurement parts.
#'    \item \code{latent} : prints the information of individual latent scores
#'    \item \code{raw} : prints the results via the \code{summary} function of \pkg{rstan} package..
#'  }
#' @param ... additional options for future development
#'
#' @return Summary of FLPS model are printed.
#'
#' @method summary flps
#' @rdname summary.flps
#' @export
summary.flps <- function(object, type = "structures", ...) {
  type <- match.arg(type, c("structures","measurement", "raw"))

  out <- rstan::summary(object$flps_fit, ...)
  out1 <- out$summary

  out1 <- round(data.frame(out1),4)
  par_name <- rownames(out1)
  out1 <- out1[, c(1,3,4,5,6,7,8,10)]

  calls <- as.list(object$call)
  covariates <- as.list(calls$covariate)

  covariates <- unlist(covariates[-1])

  lv_model <- getMeasurementItems(lv_model = calls$lv_model)

  if(is.null(calls$nclass)) {
    nfac <- lv_model$nfac
    fname <- lv_model$fname

    latents <- fname
    covariates_f <- paste0(covariates,".", fname)
    covariates_y <- paste0(covariates,".Y", fname)

    itemslope_name <- paste0(lv_model$item_name)

    item_name <- unlist(lapply(1:length(fname), function(xf) {
      paste0(lv_model$item_factor[[xf]], ".",fname[xf])
    }))

    if(tolower(calls$lv_type) == "sem") {
      itemint_name <- item_name
    } else {
      itemint_name <- paste0(item_name,".", 1:object$flps_data$stan_data$max_k)
    }

  } else {
    nclass = calls$nclass

    latents <- paste0("C",2:nclass)
    covariates_f <- paste0(covariates,".C")
    covariates_y <- paste0(covariates,".Y")

    tau0s <- out1[grep('tau0', par_name), ]
    rownames(tau0s) <- paste0("tau0.C",1:(nclass))

    tau1s <- out1[grep('tau1', par_name), ]
    rownames(tau1s) <- paste0("tau1.C",1:(nclass))

    tau1 = out1[grep('b1', par_name), ]
    rownames(tau1) = paste0("tau1.",1:(nclass-1))

    omega = out1[grep('a1', par_name), ]
    rownames(omega) = paste0("omega.",1:(nclass-1))


    item_name <- unlist(lapply(1:nclass, function(xxx) {
      paste0(lv_model$item_name,".",xxx)
    }))
  }


  if(type == "structures") {

    if(!res$flps_data$lv_type %in% c("lca","lpa")) {

      fsc <- out1[grep("fsc", par_name),]
      fsc$trt <- res$flps_data$stan_data$Z

      tau1 <- out1[grep('tau1', par_name), ]
      rownames(tau1) <- paste0('tau1.',latents)

      omega = out1[grep('omega', par_name), ]
      rownames(omega) <- latents

      betaY <- out1[grep('betaY', par_name), ]
      rownames(betaY) <- covariates_y

      betaU <- out1[grep('betaU', par_name), ]
      rownames(betaU) <- covariates_f

      lmv <- data.frame(
        "Mean" = round(
          c(mean(fsc[fsc$trt == 1, "mean"]),mean(fsc[fsc$trt == 0, "mean"])),3
        ),
        "Var" = round(
          c(var(fsc[fsc$trt == 1, "mean"]),var(fsc[fsc$trt == 0, "mean"])),3
        )
      )
      rownames(lmv) <- c("Treatment", "Control")

      o <- list(
        'Causal effect (tau0)' = out1[grep('tau0', par_name), ],
        'Principal effect (tau1)' = tau1,
        "Latent variables' effects on Y (omega)" = omega,
        "Covariates' effects on Y (betaY)" = betaY,
        "Covariates' effects on latent variables  (betaU)" = betaU,
        "Latent means and variances for treatment and control group" = lmv
      )

    } else {

      nu <- out1[grep("nu", par_name),]
      nu$trt <- res$flps_data$stan_data$Z

      nu$trt[nu$trt==0] <- 'Control'
      nu$trt[nu$trt==1] <- 'Treatment'

      classp <- nu[, "mean"]
      classp[classp >= 0.5] <- "C1"
      classp[classp < 0.5] <- "C2"

      nu$class_mem <- classp

      memtab0 <- table(nu[,c("trt","class_mem")])
      # rownames(memtab0)
      memtab <- matrix(memtab0, ncol = 2, nrow = 2)
      rownames(memtab) <- rownames(memtab0)
      colnames(memtab) <- colnames(memtab0)

      betaY <- out1[grep('betaY', par_name), ]
      rownames(betaY) <- covariates_y

      betaU <- out1[grep('betaU', par_name), ]
      rownames(betaU) <- covariates_f

      o <- list(
        'Y means differ between classes' = tau0s,
        'Treatment assignemtn effects on Y differ between classes' = tau1s,
        'Principal effect (tau1)' = tau1,
        "Latent class' effects on Y (omega)" = omega,
        "Covariates' effects on Y (betaY)" = betaY,
        "Covariates' effects on latent classes  (betaU)" = betaU,
        "Class proportions for treatment and control group" = out1[grep('alpha', par_name), ],
        'Class memberships for treatment and control group' = memtab
      )

    }

  } else if(type == "measurement") {

    measurement <- out1[grepl("^(loading|intcpt|p)\\[",par_name),]


    if(!res$flps_data$lv_type %in% c("lca","lpa")) {

      itemintcpt <- out1[grep('intcpt\\[', par_name), ]
      try(rownames(itemintcpt) <- itemint_name, silent = T)

      itemslope <- out1[grep('loading\\[', par_name), ]
      try(rownames(itemslope) <- itemslope_name, silent = T)

      o <- list(
        'Item intercepts' = itemintcpt,
        'Item slopes' = itemslope
      )
    } else {

      item_est <- out1[grep('p\\[', par_name), ]
      rownames(item_est) <- item_name

      if(res$flps_data$lv_type %in% c("lca")) {
        o <- list(
          'Item probs' = item_est
        )

      } else if(res$flps_data$lv_type %in% c("lpa"))
        o <- list(
          'Item means' = item_est
        )
    }

  } else if(type == "raw") {
    return(out1)

  } else if(type == 'latent') {

    o <- out1[grep("nu|fsc", par_name),]

    if(any(tolower(object$call$lv_type) %in% c("lca","lpa"))) {

      classp <- o[, "mean"]
      classp[classp >= 0.5] <- "C1"
      classp[classp < 0.5] <- "C2"

      o$class_mem <- classp
    }

  }

  o = list(results=o, call = calls)
  class(o) = "summary.flps"
  return(o)
}
