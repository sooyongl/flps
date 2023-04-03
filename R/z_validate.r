#' @noRd
validate <- function(inp_data, custom_data) {

  # validate ----------------------------------------------------------------
  if(!is.null(inp_data) && !is.null(custom_data))
    stop("Data is not provided.")

  if((!is.null(custom_data) && is.null(custom_stan)) |
     (is.null(custom_data) && !is.null(custom_stan)))
    stop("Custom data and custome stan code must be provided at the same time!")

}
