#' Load rstan model
#'
#' @param lv_type a character specifying a latent model
#' @return A string for Stan syntax
loadRstan <- function(lv_type = "2PL") {

  if(!dir.exists(system.file("stan", package = "flps")))
    stop("The stan code does not exist!")

  if(tolower(lv_type) %in% c("2pl", "3pl")) {
    lv_type <- "IRT"
  }

  if(tolower(lv_type) %in% c("rasch")) {
    lv_type <- "RASCH"
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

#' Create Stanmodel class
#'
#' @param lv_type a character specifying a latent model
#' @return A stanmodel class
mkStanModel <- function(lv_type = '2pl') {

  if(!dir.exists(system.file("stan", package = "flps")))
    stop("The stan code does not exist!")

  if(tolower(lv_type) %in% c("2pl", "3pl")) {
    lv_type <- "IRT"
  }

  if(tolower(lv_type) %in% c("rasch")) {
    lv_type <- "RASCH"
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

  stanfit <- rstan::stanc_builder(stan_file,
                                  allow_undefined = TRUE,
                                  obfuscate_model_name = FALSE)
  stanfit$model_cpp <- list(model_cppname = stanfit$model_name,
                            model_cppcode = stanfit$cppcode)
  # create stanmodel object
  methods::new(Class = "stanmodel",
               model_name = stanfit$model_name,
               model_code = stanfit$model_code,
               model_cpp = stanfit$model_cpp,
               mk_cppmodule = function(x) get(paste0(stanfit$model_name))
               # mk_cppmodule = function(x) get(paste0("anon_model"))
               )
}
