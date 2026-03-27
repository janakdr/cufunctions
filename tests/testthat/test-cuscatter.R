# Tests for cuscatter

test_that("cuscatter(tcstudy, tcpre) correlation and regression output", {
  out <- capture.output(with(NEJM, cuscatter(tcstudy, tcpre, remove = c("x.ticks", "y.ticks"))))
  out_text <- paste(trimws(out, "right"), collapse = "\n")

  # Pearson correlation
  expect_match(out_text, "Pearson's product-moment correlation", fixed = TRUE)
  expect_format_match(out, "t = %n, df = %n, p-value = %n",
                      c(7.2818, 34, 1.97e-08), tol = 0.001)

  # Regression coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cuscatter_tcstudy_tcpre_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term",
                     label = "cuscatter(tcstudy, tcpre) coefficients")
})
