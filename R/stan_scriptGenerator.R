#' #' S3 generic for Generate Stan Script
#' #' @noRd
#' stanScriptGenerator <- function(info, ...) {
#'   UseMethod("stanScriptGenerator", info)
#' }

#' @title Generate Stan Script
#'
#' @description
#' This function generates \code{Stan} script.
#'
#' @examples
#' stanScriptGenerator(type = "irt", cate = TRUE, level = 1)
#' stanScriptGenerator(type = "sem", cate = TRUE, level = 1)
#' @noRd
stanScriptGenerator <- function(type = c("irt","sem","rasch","grm0","grm","lca","lpa"),
                                cate = TRUE, level = 1) {

  type <- tolower(type)
  type <- match.arg(type)

  if((type %in% c("irt","rasch","grm","grm0") & !cate)) {
    stop("Cate must be TRUE")
  }

  inpargg <- c(as.list(environment()))

  inpargg$type <- type

  script <- paste(
    do.call(data_stan, inpargg),
    do.call(parameters_stan, inpargg),
    do.call(transparams_stan, inpargg),
    do.call(model_stan, inpargg),
    do.call(generatedparam_stan, inpargg),

    collapse = "\n"
  )
  return(glue("{script}"))

}
