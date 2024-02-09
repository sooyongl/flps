#' Make plots related to FLPS models
#'
#' @param object a \code{\link[flps]{flps}} object
#' @param type a character indicating the type of plots
#' @param ... Additional features related to plots
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
flps_profile <- function(object, ...) { # object = res

  if(!object$flps_data$lv_type %in% c('lca',"lpa")) {
    stop("flps_profile() works with LCA or LPA")
  }

  add_options = list(...)
  inputs <- as.list(object$call)
  lv_model <- getMeasurementItems(lv_model = inputs$lv_model)

  fname <- lv_model$fname
  cname <- c(paste0(fname,"1"),paste0(fname,"2"))
  item_name <- lv_model$item_name

  yname <- ifelse(inputs$lv_type == "lpa", "Means", "Probs")

  out <- rstan::summary(object$flps_fit, ...)

  out1 <- out$summary[grepl("^(p)\\[",rownames(out$summary)), ]

  LatentClass <- paste0(fname, gsub("p\\[(\\d+).*", "\\1",
                                    rownames(out1)))
  param <- gsub("p.*\\,\\s*|\\]", "", rownames(out1))

  levels = unique(param)
  labels = item_name

  out1 <- data.frame(out1, LatentClass, param)

  out2 <- out$summary[grepl("^(nu)\\[",rownames(out$summary)), ]

  LatentClass <- out2[, "mean"]
  LatentClass[LatentClass >= 0.5] <- 1
  LatentClass[LatentClass < 0.5] <- 0

  LatentClass[LatentClass == 1] <- cname[1]
  LatentClass[LatentClass == 0] <- cname[2]

  class_counts = table(LatentClass) / sum(table(LatentClass))
  class_probs <- data.frame(class_counts)

  probs <- data.frame(out1)
  merged_df <- merge(probs, class_probs, by = "LatentClass", all.x = TRUE)
  merged_df$LatentClass <- paste0(merged_df$LatentClass, ":", merged_df$Freq)

  merged_df$param <- factor(merged_df$param, levels, labels)

  plinewidth <- ifelse(is.null(add_options$linewidth), 1.5, add_options$linewidth)
  psize <- ifelse(is.null(add_options$size), 2.5, add_options$size)

  ptextsize <- ifelse(is.null(add_options$textsize), 14, add_options$textsize)

  p <- ggplot(merged_df) +

    geom_line(aes(x = param, y = mean,
                  group = LatentClass,
                  color = LatentClass), linewidth = plinewidth) +
    geom_point(aes(x = param, y = mean,
                   fill = LatentClass),
               colour = "white",
               shape=21, stroke = 1.5,
               size = psize) +

    labs(y = yname, x = "Items") +
    scale_color_brewer(name = "", type = "qual", palette = "Dark2") +
    scale_fill_brewer(name = "", type = "qual", palette = "Dark2") +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw(base_size = ptextsize) +
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

  # if(!object$flps_data$lv_type %in% c('lca',"lpa")) {
  #   stop("flps_profile() works with LCA or LPA")
  # }

  add_options <- list(...)
  inputs <- as.list(object$call)

  lv_model <- getMeasurementItems(lv_model = inputs$lv_model)
  fname <- lv_model$fname

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
    xname <- "Class membership prob"


  } else {
    nfac <- lv_model$nfac

    lat.val <- fit[grepl("fsc", rownames(fit)), "mean"]
    xname <- "Factor scores"

    sepby <- ifelse(nfac > 1, 2, 1)
    findex <- lapply(1:nfac, function(fi) {
      seq(from = fi, to = length(lat.val), by = sepby)
    })

    fsc_list <- lapply(1:length(findex), function(findexed) {
      temp0 <- lat.val[findex[[findexed]]]

      temp1 <- data.frame(
        fname = fname[[findexed]],
        lscores = temp0,
        trt = object$flps_data$stan_data$Z)

      temp1
    })

    fvalues <- do.call('rbind', fsc_list)

  }

  # inp_data$lscores <- lat.val
  # inp_data$trt <- factor(inp_data$trt, labels = c("Control","Treatment"))

  fvalues$trt <- factor(fvalues$trt, labels = c("Control","Treatment"))

  ptextsize <- ifelse(is.null(add_options$textsize), 14, add_options$textsize)
  pgroup <- ifelse(is.null(add_options$group), F, add_options$group)

  # p <- ggplot(inp_data)
  p <- ggplot(fvalues)

  if(type == "hist") {

    if(pgroup) {

      meandata <- aggregate(lscores ~ trt + fname, data = fvalues, FUN = mean)
      names(meandata)[names(meandata) == "lscores"] <- "grp.mean"

      p <-
        p +
        geom_histogram(aes(.data$lscores, color = .data$trt), fill = "white") +
        geom_vline(data=meandata,
                   aes(xintercept=grp.mean, color=trt),
                   linetype="dashed") +
        scale_color_brewer(name = "",
                           type = "qual", palette = "Dark2") +
        facet_grid(. ~ fname) +
        labs(x = "Factor scores") +
        theme_bw(base_size = ptextsize)
    }

  } else {

    meandata <- aggregate(lscores ~ fname, data = fvalues, FUN = mean)
    names(meandata)[names(meandata) == "lscores"] <- "grp.mean"

    p <-
      p +
      geom_histogram(aes(.data$lscores), color = "white") +
      geom_vline(data=meandata,
                 aes(xintercept=grp.mean, color=trt),
                 linetype="dashed") +
      labs(x = xname) +
      facet_grid(. ~ fname) +
      theme_bw(base_size = ptextsize)
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

  lv_model <- getMeasurementItems(lv_model = inputs$lv_model)
  fname <- lv_model$fname

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

    pwidth <- ifelse(is.null(add_options$width), 0.6, add_options$width)
    ptextsize <- ifelse(is.null(add_options$textsize), 14, add_options$textsize)

    ggplot(dt) +
      geom_bar(aes(TRT, Yfitted, fill = C),
               width = pwidth,
               color = "white",
               stat = 'summary', fun = mean,
               position = position_dodge()
      ) +
      scale_fill_brewer(name = "", type = "qual", palette = "Dark2") +
      theme_bw(base_size = ptextsize) +
      theme(legend.position="bottom") +
      labs(x = "", fill = "")


  } else {

    lat.val <- fit[grepl("fsc", rownames(fit)), "mean"]

    nfac <- lv_model$nfac
    sepby <- ifelse(nfac > 1, 2, 1)
    findex <- lapply(1:nfac, function(fi) {
      seq(from = fi, to = length(lat.val), by = sepby)
    })

    fsc_list <- lapply(1:length(findex), function(findexed) {
      temp0 <- lat.val[findex[[findexed]]]
      temp0
      # temp1 <- data.frame(
      #   fname = fname[[findexed]],
      #   lscores = temp0,
      #   trt = object$flps_data$stan_data$Z)
      #
      # temp1
    })
    # fvalues <- do.call('rbind', fsc_list)

    plist <- vector('list', length(fsc_list))
    for(fl in 1:length(fsc_list)) {

      lat.val <- fsc_list[[fl]]; # fl = 1
      inp_data$lscores <- lat.val

      tau0 <- fit[grepl("tau0", rownames(fit)), "mean"]
      tau1 <- fit[grepl("tau1", rownames(fit)), "mean"][fl]
      omega <- fit[grepl("omega", rownames(fit)), "mean"][fl]

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


      ptextsize <- ifelse(is.null(add_options$textsize), 14, add_options$textsize)

      plinewidth <- ifelse(is.null(add_options$linewidth), 1.3, add_options$linewidth)

      keep.point <- ifelse(is.null(add_options$keep.point), F, add_options$keep.point)

      palpha = 0
      if(keep.point) {
        palpha <- ifelse(is.null(add_options$alpha), 0.1, add_options$alpha)
        # p <- p + geom_point(alpha = palpha)
      }

      plist[[fl]] <-
        p +
        geom_point(alpha = palpha) +
        geom_abline(data = slp.data,
                    aes(intercept = .data$intercept, slope = .data$slope,
                        color = .data$trt, linetype = .data$trt),
                    linewidth = plinewidth) +
        scale_x_continuous(name = paste(fname[fl], "Factor Scores")) +
        scale_linetype_discrete(name = "") +
        scale_color_brewer(name = "", type = "qual", palette = "Dark2") +
        theme_bw(base_size = ptextsize) +
        theme(legend.position="bottom")

    }

    return(plist)
  }
}

#' Plot
#'
#' @param x an object of class \code{\link[flps]{flps}}
#' @param type a string for the type of plot
#' @param pars a character vector indicating the target parameters
#' @param ... additional options for \code{stan_plot}
#'
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further customized
#' using the \pkg{ggplot2} package.
#'
#' @method plot flps
#' @rdname plot
#' @export
plot.flps <- function(x, type = NULL, pars = c("tau0","tau1"), ...) {

  args_ls <- mget(names(formals()),sys.frame(sys.nframe()))
  args_ls <- append(args_ls, list(...))
  args_ls$`...` <- NULL
  args_ls$type <- NULL
  args_ls$object <- x$flps_fit
  args_ls$x <- NULL


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
  else if(type == "autocor") {
    do.call("stan_ac", args_ls)
  }

}
