test_that("runFLPS run unidimensional FLPS model", {
  set.seed(1000)

  inp_data <- makeInpData(
    N       = 100,
    R2Y     = 0.2,
    R2eta   = 0.5,
    omega   = 0.2,
    tau0    = 0.13,
    tau1    = -0.06,
    betaY   = 0.1,
    betaL   = 0.2,
    lambda  = 0.8,
    nitem    = 5,
    nfac    = 1,
    lvmodel = 'sem'
  )

  res <- suppressWarnings(runFLPS(
    inp_data = inp_data,
    outcome = "Y",
    group = "Z",
    covariate = c("X1"),
    lv_type = "sem",
    lv_model = "F =~ v1 + v2 + v3 + v4 + v5",
    stan_options = list(iter = 10, warmup = 5, cores = 1, chains = 1)
  )
  )

  res <- class(res$flps_fit)

  expect_equal(res[1], "stanfit")
})
