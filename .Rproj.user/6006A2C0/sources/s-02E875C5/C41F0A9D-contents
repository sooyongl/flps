test_that("genIpar generates item parameters", {
  set.seed(1000)
  ipar <- round(genIRTpar(2, 3, 1),1)
  ex.ipar <- data.frame(a1 = c(1.0,1.1),
                        d1 = c(0.3,0.2),
                        d2 = c(-0.3,-0.2))
  expect_equal(ipar, ex.ipar)
})


test_that("genTheta generates individual scores", {
  set.seed(1000)
  indi.score <- round(genTheta(1, 1),1)
  ex.score <- -0.4
  expect_equal(indi.score, ex.score)
})


test_that("genData generates item response data", {
  set.seed(1000)
  ipar <- genIRTpar(2, ncat = 3, 1)
  eta <- genTheta(1, 1)
  res.data <- genData(eta, ipar)
  ex.data <- data.frame(y1 = 0, y2 = 0)
  expect_equal(res.data, ex.data)
})

test_that("genLavSyn generates lavaan syntax", {
  set.seed(1000)
  ipar <- genIRTpar(2, ncat = 3, 1)
  eta <- genTheta(1, 1)
  res.data <- genData(eta, ipar)
  lavaan.syntax <- genLavSyn(res.data, 1)
  res <- is.character(lavaan.syntax)
  expect_true(res)
})

test_that("runGRM runs GRM", {
  set.seed(1000)
  ipar <- genIRTpar(6, ncat = 3, 1)
  eta <- genTheta(100, 1)
  res.data <- genData(eta, ipar)
  lavaan.syntax <- genLavSyn(res.data, 1)
  grm.fit <- runGRM(res.data, lavaan.syntax, "WL")
  res <- class(grm.fit$lav.fit) == "lavaan"
  expect_true(res)
})

test_that("extract_ tidies restuls", {
  set.seed(1000)
  ipar <- genIRTpar(6, ncat = 3, 1)
  eta <- genTheta(100, 1)
  res.data <- genData(eta, ipar)
  lavaan.syntax <- genLavSyn(res.data, 1)
  grm.fit <- runGRM(res.data, lavaan.syntax, "WL")
  res1 <- is.data.frame(extract_est(grm.fit))
  res2 <- is.data.frame(extract_fit(grm.fit))
  expect_true(res1 & res2)
})

test_that("Plotting functions work", {
  set.seed(1000)
  ipar <- genIRTpar(6, ncat = 3, 1)
  eta <- genTheta(100, 1)
  res.data <- genData(eta, ipar)
  lavaan.syntax <- genLavSyn(res.data, 1)
  grm.fit <- runGRM(res.data, lavaan.syntax, "WL")
  p1 <- FSplot(grm.fit)
  p2 <- ICCplot(grm.fit, 1)
  p3 <- ESplot(grm.fit , 1)
  p4 <- infoPlot(grm.fit, 1)
  res <- all(c(class(p1)[1],class(p2)[1],class(p3)[1],class(p4)[1]) %in% "gg")
  expect_true(res)
})
