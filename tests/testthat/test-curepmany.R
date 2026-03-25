library(testthat)
library(cufunctions)

# Tests for curepmany based on cufuncs-tests.docx
# Golden values are in tests/testthat/golden/*.csv

test_setup <- function() {
  data(delta, envir = environment())
  return(delta)
}

test_that("curepmany(delta, TC, TG, Diet) generates matching csv", {
  delta <- test_setup()
  golden_path <- test_path("golden/curepnos-curepmany.csv")
  
  fname <- tempfile("curepnos")
  actual_file <- paste0(fname, "-curepmany.csv")
  
  # Execute the function, which generates curepnos-curepmany.csv
  capture.output(suppressWarnings(curepmany(delta, "TC", "TG", "Diet", fname=fname)))
  
  # Verify file exists
  expect_true(file.exists(actual_file))
  
  # Verify table match
  actual <- read.csv(actual_file, stringsAsFactors = FALSE, check.names = FALSE)
  golden <- read.csv(golden_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  expect_equal(actual, golden, tolerance = 0.01, ignore_attr = TRUE)
  
  # Cleanup
  unlink(actual_file)
})

test_that("curepmany(delta, TC, TG, Diet, sex) generates matching csv", {
  delta <- test_setup()
  golden_path <- test_path("golden/curepnew-curepmany.csv")
  
  fname <- tempfile("curepnew")
  actual_file <- paste0(fname, "-curepmany.csv")
  
  # Execute function
  capture.output(suppressWarnings(curepmany(delta, "TC", "TG", "Diet", "sex", fname=fname)))
  
  # Verify file exists
  expect_true(file.exists(actual_file))
  
  # Verify match
  actual <- read.csv(actual_file, stringsAsFactors = FALSE, check.names = FALSE)
  golden <- read.csv(golden_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  expect_equal(actual, golden, tolerance = 0.01, ignore_attr = TRUE)
  
  # Cleanup
  unlink(actual_file)
})
