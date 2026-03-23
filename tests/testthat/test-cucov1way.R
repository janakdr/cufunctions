# Tests for cucov1way based on cufunctions-tests-with-output.txt
# Golden values are in tests/testthat/golden/*.csv

attach_NEJM()
withr::defer(detach_NEJM(), teardown_env())

# --- cucov1way(tcstudy, tcpre, Diet) — default ---

test_that("cucov1way(tcstudy, tcpre, Diet) summary, coef, posthoc, p-matrix, and F-tests match golden", {
  out <- capture.output(cucov1way(tcstudy, tcpre, Diet))

  # Summary table
  actual_summary <- parse_summary_table(out)
  golden_summary <- load_golden("cucov1way_tcstudy_tcpre_Diet_summary")
  expect_table_match(actual_summary, golden_summary,
                     label = "cucov1way summary")

  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cucov1way_tcstudy_tcpre_Diet_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term",
                     label = "cucov1way coefficients")

  # Posthoc comparisons
  expect_posthoc_match(out, "cucov1way_tcstudy_tcpre_Diet_posthoc")

  # P-value matrix
  actual_pmat <- parse_p_matrix(out, "Summary of p-values")
  golden_pmat <- load_golden("cucov1way_tcstudy_tcpre_Diet_p_matrix")
  expect_table_match(actual_pmat, golden_pmat,
                     label = "cucov1way p-matrix")

  # F-tests
  expect_partial_f_match(out, "cucov1way_tcstudy_tcpre_Diet_f_tests")
})

# --- cucov1way(tcstudy, tcpre, Diet, g1order=...) — reordered groups ---

test_that("cucov1way with g1order reorders summary and coefficients", {
  out <- capture.output(cucov1way(tcstudy, tcpre, Diet,
                                  g1order = c("Step1", "Mono", "AAD")))

  # Summary table (columns reordered)
  actual_summary <- parse_summary_table(out)
  golden_summary <- load_golden("cucov1way_tcstudy_tcpre_Diet_g1order_summary")
  expect_table_match(actual_summary, golden_summary,
                     label = "cucov1way g1order summary")

  # Coefficients (reference level changes)
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cucov1way_tcstudy_tcpre_Diet_g1order_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term",
                     label = "cucov1way g1order coefficients")

  # P-value matrix (reordered)
  actual_pmat <- parse_p_matrix(out, "Summary of p-values")
  golden_pmat <- load_golden("cucov1way_tcstudy_tcpre_Diet_g1order_p_matrix")
  expect_table_match(actual_pmat, golden_pmat,
                     label = "cucov1way g1order p-matrix")
})
