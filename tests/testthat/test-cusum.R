test_that("cusum works on numeric vector", {
  x <- c(1, 2, 3, 4, 5, NA)
  result <- cusum(x)
  expect_true(!is.null(result))
})

test_that("cusum works on factor", {
  f <- factor(c("a", "b", "a", "c", "b"))
  result <- cusum(f)
  expect_true(!is.null(result))
})

test_that("cusum works on data frame", {
  df <- data.frame(x = 1:5, y = factor(c("a", "b", "a", "b", "a")))
  result <- cusum(df)
  expect_true(!is.null(result))
})
