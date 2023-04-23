#' Generate compiled Stan object to facilitate the analysis
#'
#' @param type a character indicating the type of FLPS model. The default is \code{NULL} to compile all available Stan syntax ('all' does the same thing).
#'
#' @return There's no return, but the compiled objects are saved in the package
#' root directory.
#'
#' @examples
#' \dontrun{
#' modelBuilder(type = "rasch")
#' }
#'
#' @export
modelBuilder <- function(type = 'all') {

  stan_path <- system.file("stan", package = "flps")
  stan_list <- list.files(stan_path,full.names = T)

  message("It will take a while....")

  if(tolower(type) == "all") {

    stanfiles <- stan_list[grepl("stan$", stan_list)]

    for(i in 1:length(stanfiles)) {

      stan_model <- paste(readLines(stanfiles[i]), collapse = "\n")
      # cat(stan_model)
      stanmodel_obj <- rstan::stan_model(model_code = stan_model)
      saveRDS(stanmodel_obj, gsub("\\.stan", "\\.rds", stanfiles[i] ))

      message(paste0("Complied Stan object saved as ", gsub("\\.stan", "\\.rds", stanfiles[i] )))

    }
  } else {

    type <- ifelse(toupper(type) == "2PL", "IRT", type)

    stanfiles <- stan_list[grepl("stan$", stan_list)]
    stanfiles <- stanfiles[grepl(toupper(type), stanfiles)]

    stan_model <- paste(readLines(stanfiles), collapse = "\n")
    # cat(stan_model)

    stanmodel_obj <- rstan::stan_model(model_code = stan_model)
    saveRDS(stanmodel_obj, gsub("\\.stan", "\\.rds", stanfiles ))

    message(paste0(type, " model saved as ", gsub("\\.stan", "\\.rds", stanfiles )))
  }

  # message("It will take a while....")
}

#' Load rstan model
#'
#' @param lv_type a character specifying a latent model
#' @param force.string a logical indicating forcing the string stan code
#' @return A string for Stan syntax
#' @noRd
loadRstan <- function(lv_type = "2PL", force.string = F) {

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

  # stan_picked <- grepl("\\.stan", stan_list)
  stan_picked1 <- stan_list #[stan_picked]

  stan_picked <- grepl(toupper(lv_type), toupper(stan_picked1))
  stan_model <- stan_picked1[stan_picked]


  if(force.string) {
    stan_picked2 <- grepl(".stan$", stan_model)
    stan_model <- stan_model[stan_picked2]

    stan_file <- file.path(stan_path, stan_model)
    stan_model <- paste(readLines(stan_file), collapse = "\n")

  } else {

    if(any(grepl(".rds$", stan_model))) {

      stan_picked2 <- grepl(".rds$", stan_model)
      stan_model <- stan_model[stan_picked2]

      stan_file <- file.path(stan_path, stan_model)
      stan_model <- readRDS(stan_file)

    } else {

      stan_picked2 <- grepl(".stan$", stan_model)
      stan_model <- stan_model[stan_picked2]

      stan_file <- file.path(stan_path, stan_model)
      stan_model <- paste(readLines(stan_file), collapse = "\n")
    }
  }

  return(stan_model)
}

#' Create Stanmodel class
#'
#' @param lv_type a character specifying a latent model
#' @return A stanmodel class
#' @noRd
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
