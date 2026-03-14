test_that("cupartialF returns correct p-value", {
  # Known result: if models are identical, p should be 1
  pval <- cupartialF(10, 30, 10, 30)
  expect_equal(pval, 1.0)
})

test_that("cupartialF returns small p for significant improvement", {
  # Bigger model has much lower RSE
  pval <- cupartialF(12.39, 33, 7.56, 30)
  expect_true(pval < 0.05)
})

test_that("cupartialF handles zero RSE in full model", {
  pval <- cupartialF(10, 30, 0, 28)
  expect_equal(pval, 0.0)
})
