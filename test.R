# PAckage submission -----------------------------------------------------
library(devtools)
library(pkgdown)
library(testthat)

pkgdown::build_articles()
pkgdown::build_site()

# devtools::test()
# devtools::check()
# check_rhub()
#
# base::summary

# https://cran.r-project.org/submit.html
# submit_cran(pkg = ".", args = NULL)
# release()

# devtools::build_vignettes()
spell_check()
devtools::test()

devtools::check()
devtools::check_rhub()

# devtools::check(vignettes = FALSE)
# devtools::check_rhub(check_args = "--no-build-vignettes")


devtools::check_win_devel()

obj <- as.package(".")
release(pkg = obj, check = F, args = NULL)


# library( rhub )
# rhub::validate_email(email = "sooyongl09@gmail.com")
# list_validated_emails()
#
# list_my_checks(email = "sooyongl09@gmail.com", package = "flps")
#
#
# rhub::check(email = "sooyongl09@gmail.com")


# PAckage test ------------------------------------------------------------
library(tidyverse)
library(rstan)
library(glue)
#
# for(i in fs::dir_ls("R", regexp = "(r|R)$")) { source(i) }

# load("data/binary.rda")
# load("data/graded.rda")
# load("data/continuous.rda")
# library(flps)
#
binary <- binary %>%
  group_by(schid) %>%
  mutate(cm_sex = mean(sex),
         cm_race = mean(race),
         cm_pretest = mean(pretest),
         cm_stdscore = mean(stdscore)
  ) %>%
  relocate(cm_sex,cm_race,cm_pretest,cm_stdscore, .before = "trt") %>%
  data.frame()
#
save(binary, file = "data/binary.rda", compress = "xz")
# # #
continuous <- continuous %>%
  group_by(schid) %>%
  mutate(cm_sex = mean(sex),
         cm_race = mean(race),
         cm_pretest = mean(pretest),
         cm_stdscore = mean(stdscore)
  ) %>%
  relocate(cm_sex,cm_race,cm_pretest,cm_stdscore, .before = "trt") %>%
  data.frame()
save(continuous, file = "data/continuous.rda", compress = "xz")
# # #
graded <- graded %>%
  group_by(schid) %>%
  mutate(cm_sex = mean(sex),
         cm_race = mean(race),
         cm_pretest = mean(pretest),
         cm_stdscore = mean(stdscore)
  ) %>%
  relocate(cm_sex,cm_race,cm_pretest,cm_stdscore, .before = "trt") %>%
  data.frame()
save(graded, file = "data/graded.rda", compress = "xz")

library(flps)

modelBuilder(lv_type = "irt")
modelBuilder(lv_type = "irt", T)
modelBuilder(lv_type = "irt", T, T)
modelBuilder(lv_type = "sem")
modelBuilder(lv_type = "rasch")
modelBuilder(lv_type = "lca")
modelBuilder(lv_type = "lca", T)
modelBuilder(lv_type = "lca", T, T)
modelBuilder(lv_type = "lpa")

compiled_stan <- importModel(lv_type = "irt")
compiled_stan <- importModel(lv_type = "irt", T)
#
# binary <- read.csv("data/binary.csv") %>%
#   select(-year, -lagread) %>%
#   rename("pretest" = "t1score",
#          "stdscore" = "lagmath")
# graded <- read.csv("data/graded.csv")%>%
#   select(-year, -lagread) %>%
#   rename("pretest" = "t1score",
#          "stdscore" = "lagmath")
# continuous <- read.csv("data/continous.csv")%>%
#   select(-year, -lagread) %>%
#   rename("pretest" = "t1score",
#          "stdscore" = "lagmath")
#
# save(binary, file = "data/binary.rda")
# save(graded, file = "data/graded.rda")
# save(continuous, file = "data/continuous.rda")

all_args <- list(
  inp_data = flps::binary,
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "2pl",

  lv_model = "F1 =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  F2 =~ q11 + q12 + q13 + q14 + q15 + q16 + q17 + q18 + q19 + q20",
  stan_options = list(iter = 100, cores = 2, chains = 2)
)

# single level ------------------------------------------------------------
res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('irt'),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "2pl",

  lv_model = "F1 =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  F2 =~ q11 + q12 + q13 + q14 + q15 + q16 + q17 + q18 + q19 + q20",
  stan_options = list(iter = 100, cores = 2, chains = 2)
)

library(dplyr); library(purrr)
testdata <- data.table::fread("C:/Users/lee/Desktop/test_mixture.csv")
names(testdata) <- c(paste0("q",1:10), "Y","trt","X1","X2", "cm")
table(testdata$trt)
testdata <- testdata %>%
  mutate_at(vars(matches("^q")),
    ~ if_else(trt == 0, NA, .x)
  )

res <- flps::runFLPS(
  inp_data = testdata,
  # compiled_stan = importModel('rasch'),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("X1","X2"),
  lv_type = "rasch",
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 10, cores = 2, chains = 2)
)
summary(res)
res <- flps::runFLPS(
  inp_data = testdata,
  # compiled_stan = importModel('rasch'),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("X1","X2"),
  lv_type = "lca",
  nclass = 2,
  lv_model = "C =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 10, cores = 2, chains = 2)
)
summary(res)


all_args <- list(
  inp_data = flps::binary,
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "rasch",

  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 10, cores = 2, chains = 2)
)

res <- flps::runFLPS(
  inp_data = flps::binary,
  # compiled_stan = importModel('rasch'),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "rasch",
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 10, cores = 2, chains = 2)
)

res <- runFLPS(
  inp_data = continuous,
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "sem",
  multilevel = F,
  lv_model = "
  F1 =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
  F2 =~ q11 + q12 + q13 + q14 + q15 + q16 + q17 + q18 + q19 + q20",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)


res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('irt'),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "lca",
  nclass = 2,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = continuous,
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "lpa",
  multilevel = F,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  nclass = 2,
  stan_options = list(iter = 10, cores = 1, chains = 1)
)


# multilevel model --------------------------------------------------------

set.seed(1000)

binary <- binary %>%
  group_split(trt, schid) %>%
  map_df(., ~ .x %>% sample_frac(0.5)) %>%
  arrange(schid, id)

binary %>% count(trt)

for(i in fs::dir_ls("R", regexp = "(r|R)$")) { source(i) }

res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('lca', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "lca",
  multilevel = T,
  lv_randomeffect = F,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  nclass = 2,
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('lca', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "lca",
  multilevel = T,
  lv_randomeffect = T,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  nclass = 2,
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)



res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('irt', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "irt",
  multilevel = T,
  lv_randomeffect = F,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

flps_data_class <- makeFLPSdata(inp_data, outcome, trt, covariate,
                                lv_model, lv_type, multilevel,
                                nclass = NULL,
                                group_id = 'schid')

# flps_model <- loadRstan(flps_data_class$lv_type, multilevel, lv_randomeffect)
# cat(flps_model)
str(flps_data_class$stan_data)

unique(flps_data_class$stan_data$sch)

rstan::stan(
  file = "inst/stan/multilevel/multilevel_IRT_noRandom.stan",
  data = flps_data_class$stan_data
)

res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('lca', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "irt",
  multilevel = T,
  lv_randomeffect = T,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('lca', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "rasch",
  multilevel = T,
  lv_randomeffect = F,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  nclass = 2,
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = binary,
  # compiled_stan = importModel('lca', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "rasch",
  multilevel = T,
  lv_randomeffect = T,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  nclass = 2,
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = continuous,
  # complied_stan = importModel('sem', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "sem",
  multilevel = T,
  lv_randomeffect = F,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = continuous,
  # complied_stan = importModel('sem', T, T),
  outcome  = "Y",
  trt      = "trt",
  covariate =
    list(c("sex","race","pretest","stdscore"),
         c("cm_sex","cm_race")),
  lv_type = "sem",
  multilevel = T,
  lv_randomeffect = T,
  lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  group_id = "schid",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = flps::continuous,
  # complied_stan = importModel('sem', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "sem",
  lv_model = "F1 =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = flps::continuous,
  # complied_stan = importModel('sem', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "sem",
  lv_model = "F2 =~ q5 + q6 + q7 + q8 + q9 + q10 + q1 + q2 + q3 + q4",
  # lv_model = "F1 =~ q1 + q2 + q3 + q4
  # F2 =~ q5 + q6 + q7 + q8 + q9 + q10",
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

res <- runFLPS(
  inp_data = flps::continuous,
  # complied_stan = importModel('sem', T, F),
  outcome  = "Y",
  trt      = "trt",
  covariate = c("sex","race","pretest","stdscore"),
  lv_type = "lpa",
  lv_model = "C =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
  nclass = 2,
  stan_options = list(iter = 10, cores = 1, chains = 1)
)

# Print test--------------------------------------------------------------
library(ggplot2); library(rstan)
for(i in fs::dir_ls("R")) { source(i)}

library(flps)
res <- readRDS("G:\\My Drive\\project\\1ing_FLPS_package\\examples\\stanres\\single_irt.rds")

res <- readRDS("G:\\My Drive\\project\\1ing_FLPS_package\\examples\\stanres\\single_lca.rds")

res <- readRDS("G:\\My Drive\\project\\1ing_FLPS_package\\examples\\stanres\\single_lca_iter10000.rds")

class(res)

summary(res, "structures")
summary(res, "measurement")

aa <- summary(res, "structures")

flps_plot(object = res, type = 'causal', width = 1, textsize = 20)
flps_plot(res, 'causal', keep.point = T, alpha = 1)
flps_plot(res, 'causal', keep.point = F)
flps_plot(res, 'causal', keep.point = T)

flps_plot(res, 'profile', size = 1.2, linewidth = 0.1)

flps_plot(object = res, type = 'latent', group = T, textsize = 20)

flps_causal(res)
flps_latent(res)
flps_profile(res)

a1 <- summary(res, 'raw')

a1 <- data.frame(a1)

tail(a1)

object$call

calls <- as.list(object$call)

cat(toupper(calls$lv_type), 'used as a measurement model')

plot(res, type = 'trace', pars = c("tau0", "tau1", "omega"),
     nrow = 3, ncol = 1)

plot(res, type = 'autocor', pars = c("tau1"),
     fill = "red", width = 0.5,alpha = 0.3, color = 'transparent',
     lags = 100
     )

stan_trace(res$flps_fit, pars = c("tau0", "tau1", "omega"))


stan_ac(res$flps_fit, pars = c("tau1"), fill = 'red')

covariates <- as.list(calls$covariate)

unlist(covariates[-1])



0.2139 * 1.7

















