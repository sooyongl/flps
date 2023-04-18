#' Make plots related to FLPS models
#'
#' @param object a flps object
#' @param type a character indicating the type of plots
#'
#' @return
#'   A \code{\link[ggplot2]{ggplot}} object that can be further customized
#'   using the \pkg{ggplot2} package.
#'
#' @export
flps_plot <- function(object, type = "latent") {

  if(tolower(type) == "latent") {

    flps_latent(object, "hist")
  }
  else if(tolower(type) == "causal") {

    flps_causal(object)
  }

}


#' Latent factor scores distribution by treatment assignment
#'
#' @param object a flps object
#' @param type a character indicating the type of plots
#' @noRd
flps_latent <- function(object, type = "hist") {

  inputs <- as.list(object$call)

  outcome <- inputs$outcome
  group <- inputs$group
  covariate <- unlist(as.list(inputs$covariate[-1]))

  inp_data <- object$inp_data

  out.val <- inp_data[outcome]
  group.val <- inp_data[group]
  cov.val <- inp_data[covariate]

  fit <- summary(object, type = "measurement")
  lat.val <- fit[grepl("fsc", rownames(fit)), "mean"]
  inp_data$lscores <- lat.val

  p <- ggplot(inp_data)

  if(type == "hist") {
    p + geom_histogram(aes(.data$lscores), color = "white") + theme_bw()
  }
}

#' Causal inference graphs
#'
#' @param object a flps object
#' @noRd
flps_causal <- function(object) {

  inputs <- as.list(object$call)

  outcome <- inputs$outcome
  group <- inputs$group
  covariate <- unlist(as.list(inputs$covariate[-1]))

  inp_data <- object$inp_data

  out.val <- unlist(inp_data[outcome])
  group.val <- unlist(inp_data[group])
  cov.val <- unlist(inp_data[covariate])

  fit <- summary(object)
  lat.val <- fit[grepl("fsc", rownames(fit)), "mean"]
  inp_data$lscores <- lat.val

  tau0 <- fit[grepl("tau0", rownames(fit)), "mean"]
  tau1 <- fit[grepl("tau1", rownames(fit)), "mean"]
  omega <- fit[grepl("omega", rownames(fit)), "mean"]

  inp_data$Control <- inp_data$lscores*tau0
  inp_data$Treatment <- inp_data$lscores*(tau0+tau1)

  p <- ggplot(inp_data, aes_string("lscores", outcome))

  yint <- mean(out.val, na.rm = T) -
    (mean(group.val, na.rm = T)*tau0 +
       mean(lat.val, na.rm = T)*omega + mean(group.val*lat.val, na.rm = T)*tau1)

  slp.data <- data.frame(trt = factor(c("Treatment", "Contrl"), c("Treatment", "Contrl")),
                         intercept = yint,
                         slope = c(tau0+tau1, tau0))

  p +
    geom_point() +
    geom_abline(data = slp.data,
                aes(intercept = .data$yint, slope = .data$slope,
                    color = .data$trt, linetype = .data$trt),
                linewidth = 1) +
    scale_x_continuous(name = "Factor Scores") +
    scale_linetype_discrete(name = "") +
    scale_color_brewer(name = "", type = "qual", palette = "Dark2") +
    theme_bw()
}

#' Plot
#'
#' @param object an object of class \code{flps}
#' @param type a string for the type of plot
#' @param pars a character vector indicating the target parameters
#' @param ... additional options for future development
#'
#' @method plot flps
#' @export
plot.flps <- function(object, type = NULL, pars = c("tau0","tau1"), ...) {

  args_ls <- mget(names(formals()),sys.frame(sys.nframe()))
  args_ls$`...` <- NULL
  args_ls$type <- NULL
  args_ls$object <- object$flps_fit


  if(is.null(type)) {
    do.call("stan_plot", args_ls)
  }
  else if(type == "hist") {
    do.call("stan_hist", args_ls)
  }
  else if(type == "trace") {
    do.call("stan_trace", args_ls)
  }
  else if(type == "density") {
    do.call("stan_dens", args_ls)
  }
  else if(type == "scatter") {
    do.call("stan_scat", args_ls)
  }
  else if(type == "rhat") {
    do.call("stan_rhat", args_ls)
  }
  else if(type == "par") {
    do.call("stan_par", args_ls)
  }
  else if(type == "ess") {
    do.call("stan_ess", args_ls)
  }
  else if(type == "diag") {
    do.call("stan_diag", args_ls)
  }
  else if(type == "mcse") {
    do.call("stan_mcse", args_ls)
  }
}
