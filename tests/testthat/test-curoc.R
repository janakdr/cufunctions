library(testthat)
library(cufunctions)

test_setup <- function() {
  data(Met, envir = environment())
  return(Met)
}

test_that("curoc emf=TRUE writes EMF file without error", {
  skip_if_not_installed("devEMF")

  Met <- test_setup()
  logfit <- glm(MetSyn ~ TG + HDL, data = Met, family = binomial)

  tmpdir <- withr::local_tempdir()
  old_wd <- getwd()
  withr::defer(setwd(old_wd))
  setwd(tmpdir)

  output <- capture.output({
    curoc(logfit, Met$MetSyn, emf = TRUE)
  })

  # The emf=TRUE path produces a "culogist.emf" file (line 116 of curoc.R)
  expect_true(file.exists(file.path(tmpdir, "culogist.emf")))
  # Verify file has some content.
  expect_gt(file.size(file.path(tmpdir, "culogist.emf")), 1000)

  # Verify ROC output values appear correctly in console output
  expect_format_match(
    output, 
    "Accuracy %n%; Sensitivity %n%; Specificity %n%", 
    c(84.7, 45, 96.9), 
  )
  expect_format_match(
    output, 
    "AUC =  %n", 
    c(0.799), 
  )
})

test_that("curoc emf=<name> writes named EMF file without error", {
  skip_if_not_installed("devEMF")

  Met <- test_setup()
  logfit <- glm(MetSyn ~ TG + HDL, data = Met, family = binomial)

  tmpdir <- withr::local_tempdir()
  old_wd <- getwd()
  withr::defer(setwd(old_wd))
  setwd(tmpdir)

  output <- capture.output({
    curoc(logfit, Met$MetSyn, emf = "my_roc")
  })

  expect_true(file.exists(file.path(tmpdir, "my_roc.emf")))
  # Verify file has some content.
  expect_gt(file.size(file.path(tmpdir, "my_roc.emf")), 1000)
})
