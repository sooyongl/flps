test_that("runFLPS generates a flps class", {

  binary <- binary[c(sample(which(binary$trt == 1), 50),
                     sample(which(binary$trt == 0), 50)),]

  res <-
    suppressWarnings(runFLPS(
      inp_data = binary,
      outcome = "Y",
      trt = "trt",
      covariate = c("sex","race","pretest","stdscore"),
      lv_type = "rasch",
      lv_model = "F =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10",
      stan_options = list(iter = 10, cores = 1, chains = 1)
    ))

  res <- inherits(res, "flps")

  expect_true(res)
})
