#' Import complied Stan object
#'
#' @param type a character indicating the type of FLPS model.
#' @param multilevel a logical indicating multilevel Stan model.
#'
#' @return a Stan complied object generated by \code{modelBuilder}
#'
#' @export
importModel <- function(type = c("rasch","irt","grm","sem","lca","lpa"), multilevel = FALSE) {

  if(tolower(type) == "2pl") {
    type = 'irt'
  }

  type <- match.arg(tolower(type), c("rasch","irt","grm","sem","lca","lpa"))

  stan_path <- system.file("stan", package = "flps")
  stan_list <- list.files(stan_path, pattern = "RDS$",full.names = TRUE)

  if(multilevel) {
    level = "Multi"
  } else {
    level = "Single"
  }

  stan_list <- stan_list[grepl(toupper(level), toupper(stan_list))]
  stan_list <- stan_list[grepl(toupper(type), toupper(stan_list))]

  readRDS(stan_list)
}

#' Generate compiled Stan object to facilitate the analysis
#'
#' @param type a character indicating the type of FLPS model.
#' The default is \code{all} to compile all available Stan syntax.
#' @param multilevel a logical indicating multilevel Stan model.
#'
#' @return There's no return, but the compiled objects are saved in the package
#' root directory.
#'
#' @export
modelBuilder <- function(type = 'all', multilevel = FALSE) {

  type <- match.arg(tolower(type), c("all","rasch","irt","2pl","grm","sem","lca","lpa"))

  stan_path <- system.file("stan", package = "flps")

  message("It will take a while....")

  if(tolower(type) == "all") {

    type = c("irt","rasch","grm","lca","sem","lpa")
    cate = T
    level = c(1,2)
    models <- expand.grid(type, cate, level)

    for(i in 1:nrow(models)) { # i = 9

      itype  = models[[1]][i]
      ilevel = models[[2]][i]
      icate  = models[[3]][i]

      if(itype %in% c("sem","lpa")) {
        icate = F
      }

      stanscript <- stanScriptGenerator(itype, icate, ilevel)

      if(ilevel == 1) {
        ilevel = "Single"
      } else {
        ilevel = "Multi"
      }

      writeLines(stanscript, glue("{stan_path}/{itype}_{ilevel}.stan"))

      mod <- stan_model(model_name = glue("{itype}_{ilevel}"),model_code = stanscript)
      saveRDS(mod, glue("{stan_path}/{itype}_{ilevel}.RDS"))
      message(glue("Complied Stan object ({toupper(itype)}) saved as {stan_path}/{itype}_{ilevel}.RDS"))
    }

  } else {

    type <- ifelse(toupper(type) == "2PL", "irt", tolower(type))

    cate = TRUE
    if(tolower(type) %in% c("sem","lpa")) {
      cate = FALSE
    }

    stanscript <- stanScriptGenerator(type, cate, as.numeric(multilevel)+1)

    if(multilevel) {
      level = "Multi"
    } else {
      level = "Single"
    }

    writeLines(stanscript, glue("{stan_path}/{type}_{level}.stan"))

    mod <- stan_model(model_name = glue("{type}_{level}"),model_code = stanscript)
    saveRDS(mod, glue("{stan_path}/{type}_{level}.RDS"))

    message(paste0(toupper(type), " model saved as ",
                   glue("{stan_path}/{type}_{level}.RDS")))

  }
}

#' Load rstan model
#'
#' @param lv_type a character specifying a latent model
#' @param multilevel a logical specifying multilevel structure
#' @return A string for Stan syntax
#' @noRd
loadRstan <- function(lv_type = "2PL", multilevel = FALSE) {

  if(!dir.exists(system.file("stan", package = "flps")))
    stop("The stan code does not exist!")

  if(tolower(lv_type) %in% c("2pl", "3pl")) {
    lv_type <- "IRT"
  }

  if(tolower(lv_type) %in% c("rasch")) {
    lv_type <- "RASCH"
  }

  cate = TRUE
  if(tolower(lv_type) %in% c("sem","lpa")) {
    cate = FALSE
  }

  given_stan_model <- stanScriptGenerator(lv_type, cate, as.numeric(multilevel) + 1)

  return(given_stan_model)
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
  given_stan_model <- stan_picked1[stan_picked]

  stan_file <- file.path(stan_path, given_stan_model)

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
