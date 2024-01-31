#' Print results
#'
#' @param x an object of class \code{\link[flps]{flps}}
#' @param ... additional options for future development
#'
#' @return Results of FLPS model are printed via the \pkg{rstan} package.
#'
#' @method print flps
#' @rdname print
#' @export
print.flps <- function(x, ...) {
  rstan::show(x$flps_fit, ...)
}

#' Summarize the results
#'
#' @param object an object of class \code{\link[flps]{flps}}
#' @param type a string for the part of FLPS model
#' @param ... additional options for future development
#'
#' @return Summary of FLPS model are printed via the \pkg{rstan} package.
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

  if(type == "all") {
    if(!res$flps_data$lv_type %in% c("lca","lpa")) {

      fsc <- out1[grep("fsc", par_name),]
      fsc$trt <- res$flps_data$stan_data$Z

      o <- list(
        'Causal effect (tau0)' = out1[grep('tau0', par_name), ],
        'Principal effect (tau1)' = out1[grep('tau1', par_name), ],
        "Latent variables' effects on Y (omega)" = out1[grep('omega', par_name), ],
        "Covariates' effects on Y (betaY)" = out1[grep('betaY', par_name), ],
        "Covariates' effects on latent variables  (betaU)" = out1[grep('betaU', par_name), ],
        "Latent means and variances for treatment and control group" =
          data.frame(
            "." = c("Treatment", "Control"),
            "Mean" = round(
              c(mean(fsc[fsc$trt == 1, "mean"]),mean(fsc[fsc$trt == 0, "mean"])),3
            ),
            "Var" = round(
              c(var(fsc[fsc$trt == 1, "mean"]),var(fsc[fsc$trt == 0, "mean"])),3
            )

          )
      )

    } else {

      nu <- out1[grep("nu", par_name),]
      nu$trt <- res$flps_data$stan_data$Z

      classp <- nu[, "mean"]
      classp[classp >= 0.5] <- "C1"
      classp[classp < 0.5] <- "C2"

      o <- list(
        'Y means differ between classes' = out1[grep('tau0', par_name), ],
        'Treatment assignemtn effects on Y differ between classes' = out1[grep('tau1', par_name), ],
        'Principal effect (tau1)' = out1[grep('b1', par_name), ],
        "Latent variables' effects on Y (omega)" = out1[grep('a1', par_name), ],
        "Covariates' effects on Y (betaY)" = out1[grep('betaY', par_name), ],
        "Covariates' effects on latent variables  (betaU)" = out1[grep('betaU', par_name), ],
        "Class proportions for treatment and control group" = out1[grep('alpha', par_name), ]
        )
    }

  } else if(type == "measurement") {

    measurement <- out1[grepl("^(loading|intcpt|p)\\[",par_name),]

    if(!res$flps_data$lv_type %in% c("lca","lpa")) {
      o <- list(
        'Item intercepts' = out1[grep('intcpt\\[', par_name), ],
        'Item slopes' = out1[grep('loading', par_name), ]
      )
    } else {
      if(res$flps_data$lv_type %in% c("lca")) {
        o <- list(
          'Item probs' = out1[grep('p\\[', par_name), ]
        )

      } else if(res$flps_data$lv_type %in% c("lpa"))
        o <- list(
          'Item means' = out1[grep('p\\[', par_name), ]
        )
    }

  } else if(type == "raw") {
    o <- out1

  }

  return(o)
}
