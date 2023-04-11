#' Plot
#'
#'
#' @param object an object of class \code{flps}
#' @param type a string for the type of plot
#' @param ... additional options for future development
#'
#' @method plot flps
#' @export
plot.flps <- function(object, type = "default", ...) {

  type <- match.arg(type, c("trace", "default"))

  out <- object$flps_fit



}
