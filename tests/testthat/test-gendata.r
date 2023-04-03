test_that("makeInpData generates matrix input data", {
  set.seed(1000)

  inp_data <- makeInpData(
    N       = 200,
    R2Y     = 0.2,
    R2eta   = 0.5,
    omega   = 0.2,
    tau0    = 0.13,
    tau1    = -0.06,
    lambda  = 0.8,
    nitem    = 10,
    nfac    = 1,
    lvmodel = 'rasch'
  )

  res <- is.data.frame(inp_data)

  expect_true(res)
})


test_that("makeSimData generates stan input data", {
  set.seed(1000)

  inp_data <- makeSimData(
    N       = 200,
    R2Y     = 0.2,
    R2eta   = 0.5,
    omega   = 0.2,
    tau0    = 0.13,
    tau1    = -0.06,
    lambda  = 0.8,
    nitem    = 10,
    nfac    = 1,
    lvmodel = 'rasch'
  )

  res <- class(inp_data)

  expect_equal(res, "rasch")
})

test_that("runFLPS run FLPS model", {
  set.seed(1000)

  inp_data <- makeInpData(
    N       = 100,
    R2Y     = 0.2,
    R2eta   = 0.5,
    omega   = 0.2,
    tau0    = 0.13,
    tau1    = -0.06,
    lambda  = 0.8,
    nitem    = 5,
    nfac    = 1,
    lvmodel = 'rasch'
  )

  res <- suppressWarnings(runFLPS(
    inp_data = inp_data,
    outcome = "Y",
    group = "Z",
    covariate = c("X"),
    lv_type = "rasch",
    lv_model = "F =~ v1 + v2 + v3 + v4 + v5",
    stan_options = list(iter = 500, warmup = 250, cores = 1, chains = 1)
  ))

  res <- class(res$flps_fit)

  expect_equal(res[1], "stanfit")
})

