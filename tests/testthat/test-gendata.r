test_that("makeInpData generates matrix input data for rasch", {
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


test_that("makeSimData generates stan input data for rasch", {
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

test_that("makeInpData generates matrix input data for 2pl", {
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
    lvmodel = '2pl'
  )

  res <- is.data.frame(inp_data)

  expect_true(res)
})


test_that("makeSimData generates stan input data for 2pl", {
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
    lvmodel = '2pl'
  )

  res <- class(inp_data)

  expect_equal(res, "2pl")
})

test_that("makeInpData generates matrix input data for grm", {
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
    lvmodel = 'grm'
  )

  res <- is.data.frame(inp_data)

  expect_true(res)
})


test_that("makeSimData generates stan input data for grm", {
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
    lvmodel = 'grm'
  )

  res <- class(inp_data)

  expect_equal(res, "grm")
})

test_that("makeInpData generates matrix input data for sem", {
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
    lvmodel = 'sem'
  )

  res <- is.data.frame(inp_data)

  expect_true(res)
})


test_that("makeSimData generates stan input data for sem", {
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
    lvmodel = 'sem'
  )

  res <- class(inp_data)

  expect_equal(res, "sem")
})

