#' Make plots related to FLPS models
#'
#' @param object a \code{\link[flps]{flps}} object
#' @param type a character indicating the type of plots
#' @param ... additional features related to plot
#'
#' @return
#'   A \code{\link[ggplot2]{ggplot}} object that can be further customized
#'   using the \pkg{ggplot2} package.
#'
#' @export
flps_plot <- function(object, type = "latent", ...) {

  type <- tolower(type)
  type <- match.arg(type, c("latent","hist","causal","profile"))

  if(type == "latent") {

    flps_latent(object, "hist", ...)

  } else if(type == "causal") {

    flps_causal(object, ...)

  } else if(type == "profile") {

    flps_profile(object, ...)

  }

}

#' Class profiles by latent classes
#'
#' @param object a \code{\link[flps]{flps}} object
#' @noRd
#'
flps_profile <- function(object, ...) { # object = res

  inputs <- as.list(object$call)

  out <- rstan::summary(object$flps_fit, ...)

  out1 <- out$summary[grepl("^(p)\\[",rownames(out$summary)), ]

  LatentClass <- paste0("C", gsub("p\\[(\\d+).*", "\\1",
                                  rownames(out1)))
  param <- gsub("p.*\\,\\s*|\\]", "", rownames(out1))

  levels = unique(param)

  out1 <- data.frame(out1, LatentClass, param)

  out2 <- out$summary[grepl("^(nu)\\[",rownames(out$summary)), ]

  LatentClass <- out2[, "mean"]
  LatentClass[LatentClass >= 0.5] <- "C1"
  LatentClass[LatentClass < 0.5] <- "C2"

  class_counts = table(LatentClass) / sum(table(LatentClass))
  class_probs <- data.frame(class_counts)

  probs <- data.frame(out1)
  merged_df <- merge(probs, class_probs, by = "LatentClass", all.x = TRUE)
  merged_df$LatentClass <- paste0(merged_df$LatentClass, ":", merged_df$Freq)

  merged_df$param <- factor(merged_df$param, levels)

  p <- ggplot(merged_df) +

    geom_line(aes(x = param, y = mean,
                  group = LatentClass,
                  color = LatentClass), linewidth = 1) +
    geom_point(aes(x = param, y = mean,
                   fill = LatentClass),
               colour = "white",
               shape=21, stroke = 1.5,
               size = 3) +

    labs(y = "Probs", x = "Items") +
    scale_color_brewer(name = "", type = "qual", palette = "Dark2") +
    scale_fill_brewer(name = "", type = "qual", palette = "Dark2") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position="bottom")

  if(tolower(inputs$lv_type) == "lca") {
    p <- p + coord_cartesian(ylim = c(0, 1))
  }

  p
}

#' Latent factor scores distribution by treatment assignment
#'
#' @param object a \code{\link[flps]{flps}} object
#' @param type a character indicating the type of plots
#' @noRd
flps_latent <- function(object, type = "hist", ...) {

  add_options <- list(...)
  inputs <- as.list(object$call)

  outcome <- inputs$outcome
  trt <- inputs$trt
  covariate <- unlist(as.list(inputs$covariate[-1]))

  inp_data <- object$inp_data

  out.val <- inp_data[outcome]
  trt.val <- inp_data[trt]
  cov.val <- inp_data[covariate]

  fit <- summary(object, type = "raw")

  if(any(object$flps_data$lv_type %in% c("lca","lpa"))) {
    lat.val <- fit[grepl("nu", rownames(fit)), "mean"]
  } else {
    lat.val <- fit[grepl("fsc", rownames(fit)), "mean"]
  }

  inp_data$lscores <- lat.val
  inp_data$trt <- as.factor(inp_data$trt)

  p <- ggplot(inp_data)

  if(type == "hist") {

    if(length(add_options)!=0) {
      if(add_options$group) {

        meandata <- aggregate(lscores ~ trt, data = inp_data, FUN = mean)
        names(meandata)[names(meandata) == "lscores"] <- "grp.mean"

        # meandata <- inp_data %>%
        #   group_by(trt) %>%
        #   summarize(grp.mean = mean(lscores))

        p <-
          p +
          geom_histogram(aes(.data$lscores, color = .data$trt), fill = "white") +
          geom_vline(data=meandata,
                     aes(xintercept=grp.mean, color=trt),
                     linetype="dashed") +
          scale_color_brewer(name = "", type = "qual", palette = "Dark2") +
          theme_bw()
      }

    } else {

      p <-
        p +
        geom_histogram(aes(.data$lscores), color = "white") +
        geom_vline(
          xintercept=mean(inp_data$lscores),
          color='red',
          linetype="dashed", linewidth = 1.2) +
        theme_bw()
    }
  }

  p
}

#' Causal inference graphs
#'
#' @param object a flps object
#' @noRd
flps_causal <- function(object, ...) {

  add_options <- list(...)
  inputs <- as.list(object$call)

  outcome <- inputs$outcome
  trt <- inputs$trt
  covariate <- unlist(as.list(inputs$covariate[-1]))

  inp_data <- object$inp_data

  out.val <- unlist(inp_data[outcome])
  trt.val <- unlist(inp_data[trt])
  cov.val <- unlist(inp_data[covariate])

  fit <- summary(object, type = 'raw')

  if(tolower(inputs$lv_type) %in% c("lca","lpa")) {

    # message("Plots for mixture models are soon ready")
    out2 <- fit[grepl("^(nu)\\[",rownames(fit)), ]

    LatentClass <- out2[, "mean"]
    LatentClass[LatentClass >= 0.5] <- "C1"
    LatentClass[LatentClass < 0.5] <- "C2"
    class_counts = table(LatentClass) / sum(table(LatentClass))
    class_probs <- data.frame(class_counts)

    tau0 <- fit[grepl("tau0", rownames(fit)), "mean"]
    tau1 <- fit[grepl("tau1", rownames(fit)), "mean"]

    cp <- round(class_probs$Freq, 2)

    TRT = rep(c(0,1), 1000)
    C1 = tau1[1]*TRT + tau0[1]
    C2 = tau1[2]*TRT + tau0[2]

    dt <- rbind(
      data.frame(Yfitted = C1, TRT, C = paste0("C1:", cp[1])),
      data.frame(Yfitted = C2, TRT, C = paste0("C2:", cp[2]))
    )

    dt$TRT <- factor(dt$TRT, labels = c("Control","Treatment"))

    ggplot(dt) +
      geom_bar(aes(TRT, Yfitted, fill = C),
               width = 0.6,
               color = "white",
               stat = 'summary', fun = mean,
               position = position_dodge()
      ) +
      scale_fill_brewer(name = "", type = "qual", palette = "Dark2") +
      theme_bw() + theme(legend.position="bottom") +
      labs(x = "", fill = "")


  } else {

    lat.val <- fit[grepl("fsc", rownames(fit)), "mean"]
    inp_data$lscores <- lat.val

    tau0 <- fit[grepl("tau0", rownames(fit)), "mean"]
    tau1 <- fit[grepl("tau1", rownames(fit)), "mean"]
    omega <- fit[grepl("omega", rownames(fit)), "mean"]

    inp_data$Control <- inp_data$lscores*tau0
    inp_data$Treatment <- inp_data$lscores*(tau0+tau1)

    p <- ggplot(inp_data, aes(.data$lscores, .data$Y))

    yint <- mean(out.val, na.rm = TRUE) -
      (mean(trt.val, na.rm = TRUE)*tau0 +
         mean(lat.val, na.rm = TRUE)*omega + mean(trt.val*lat.val, na.rm = TRUE)*tau1)

    slp.data <- data.frame(trt = factor(c("Treatment", "Contrl"),
                                        c("Treatment", "Contrl")),
                           intercept = yint,
                           slope = c(tau0+tau1, tau0))

    palpha = 0
    if(length(add_options)!=0) {
      if(add_options$keep.point) {
        palpha <- ifelse(is.null(add_options$alpha), 0.1, add_options$alpha)
        p <- p + geom_point(alpha = palpha)
      }
    }

    p <-
      p +
      geom_point(alpha = palpha) +
      geom_abline(data = slp.data,
                  aes(intercept = .data$intercept, slope = .data$slope,
                      color = .data$trt, linetype = .data$trt),
                  linewidth = 1.5) +
      scale_x_continuous(name = "Factor Scores") +
      scale_linetype_discrete(name = "") +
      scale_color_brewer(name = "", type = "qual", palette = "Dark2") +
      theme_bw() +
      theme(legend.position="bottom")


    p
  }
}

#' Plot
#'
#' @param x an object of class \code{\link[flps]{flps}}
#' @param type a string for the type of plot
#' @param pars a character vector indicating the target parameters
#' @param ... additional options for future development
#'
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further customized
#' using the \pkg{ggplot2} package.
#'
#' @method plot flps
#' @rdname plot
#' @export
plot.flps <- function(x, type = NULL, pars = c("tau0","tau1"), ...) {

  args_ls <- mget(names(formals()),sys.frame(sys.nframe()))
  args_ls$`...` <- NULL
  args_ls$type <- NULL
  args_ls$object <- x$flps_fit


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
