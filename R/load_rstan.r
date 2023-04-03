#' Load rstan model
#'
#' @param lv_type a character specifying a latent model
#' @return A string for Stan syntax
#' @examples
#' stan_model <- rstan_path(lv_type = "rasch")
loadRstan <- function(lv_type = "2PL") {

  if(!dir.exists(system.file("stan", package = "flps")))
    stop("The stan code does not exist!")

  if(tolower(lv_type) %in% c("rasch","2pl", "3pl")) {
    lv_type <- "IRT"
  }

  stan_path <- system.file("stan", package = "flps")
  # stan_path <- "inst/stan"
  stan_list <- list.files(stan_path)

  if(tolower(lv_type) != "lca") {
    stan_list <- stan_list[grepl(toupper("flps"), toupper(stan_list))]
  }

  stan_picked <- grepl("\\.stan", stan_list)
  stan_picked1 <- stan_list[stan_picked]

  stan_picked <- grepl(toupper(lv_type), toupper(stan_picked1))
  stan_model <- stan_picked1[stan_picked]

  stan_file <- file.path(stan_path, stan_model)

  stan_model <- paste(readLines(stan_file), collapse = "\n")

  return(stan_model)
}


