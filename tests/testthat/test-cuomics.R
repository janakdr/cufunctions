library(testthat)
library(cufunctions)

# Tests for cuomics based on cufuncs-tests.docx
# Golden values are in tests/testthat/golden/*.csv

test_setup <- function() {
  data(NEJM, envir = environment())
  return(NEJM)
}

test_that("cuomics 1way generates matching csv", {
  NEJM <- test_setup()
  golden_path <- test_path("golden/tchc1w-cuomics.csv")
  
  # Generate an absolute path in the OS temp directory
  fname <- tempfile("tchc1w")
  actual_file <- paste0(fname, "-cuomics.csv")
  
  # Execute the function using the temp file prefix
  capture.output(suppressWarnings(suppressMessages(with(NEJM, cuomics("1way", NEJM, "tcstudy", "hcchange", fname, Diet)))))
  
  # Verify file exists
  expect_true(file.exists(actual_file))
  
  # Verify table match
  actual <- read.csv(actual_file, stringsAsFactors = FALSE, check.names = FALSE)
  golden <- read.csv(golden_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  expect_equal(actual, golden, tolerance = 0.01, ignore_attr = TRUE)
  
  # Cleanup
  unlink(actual_file)
})

test_that("cuomics 2way generates matching csv", {
  NEJM <- test_setup()
  golden_path <- test_path("golden/tchc2w-cuomics.csv")
  
  fname <- tempfile("tchc2w")
  actual_file <- paste0(fname, "-cuomics.csv")
  
  # Execute function, avoiding attach namespace collision by passing bare environment
  capture.output(suppressWarnings(suppressMessages(with(NEJM, cuomics("2way", NEJM, "tcstudy", "hcchange", fname, Diet, sex, pnorm=0.05)))))
  # TODO (Janak): get rid of pnorm=0.05
  
  # Verify file exists
  expect_true(file.exists(actual_file))
  
  # Verify match
  actual <- read.csv(actual_file, stringsAsFactors = FALSE, check.names = FALSE)
  golden <- read.csv(golden_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  expect_equal(actual, golden, tolerance = 0.01, ignore_attr = TRUE)
  
  # Cleanup
  unlink(actual_file)
})

test_that("cuomics repmeasw runs without normvisit crash", {
  data(delta, envir = environment())

  fname <- tempfile("tctgrep")
  actual_file <- paste0(fname, "-cuomics.csv")

  # This would crash with "object 'normvisit' not found" before the fix
  capture.output(suppressWarnings(suppressMessages(
    cuomics("repmeasw", delta, "TC", "TG", fname, "Diet", "sex")
  )))

  expect_true(file.exists(actual_file))

  actual <- read.csv(actual_file, stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(nrow(actual) > 0)
  expect_true("Variable" %in% colnames(actual))

  unlink(actual_file)
})
