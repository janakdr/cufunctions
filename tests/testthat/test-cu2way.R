# Tests for cu2way based on cufunctions-tests-with-output.txt
# Golden values are in tests/testthat/golden/*.csv

attach_NEJM()
withr::defer(detach_NEJM(), teardown_env())

# --- cu2way(tcchange, sex, Diet) — full interaction model ---

test_that("cu2way(tcchange, sex, Diet) summary, pairwise, coef, posthoc, and F-tests match golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "no"))

  # Summary table
  golden <- load_golden("cu2way_tcchange_sex_Diet_summary")
  hdr <- grep("All", out, fixed = TRUE)[1]
  end <- hdr + 1
  while (end <= length(out) && trimws(out[end]) != "") end <- end + 1
  actual <- parse_fixed_width_table(out[hdr], out[(hdr + 1):(end - 1)],
                                    row_labels = golden$stat)
  expect_table_match(actual, golden, label = "cu2way summary", tol = 0.01)

  # Pairwise matrix
  actual_pw <- parse_pairwise_matrix(out)
  golden_pw <- load_golden("cu2way_tcchange_sex_Diet_pairwise")
  expect_table_match(actual_pw, golden_pw, label = "cu2way pairwise")

  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cu2way_tcchange_sex_Diet_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term",
                     label = "cu2way coefficients")

  # sex posthoc
  expect_grouped_posthoc_match(out, "cu2way_tcchange_sex_Diet_sex_posthoc")

  # diet posthoc
  expect_grouped_posthoc_match(out, "cu2way_tcchange_sex_Diet_diet_posthoc")

  # F-tests
  expect_partial_f_match(out, "cu2way_tcchange_sex_Diet_f_tests")
})

# --- cu2way(tcchange, sex, Diet, plot="violin") — same stats ---

test_that("cu2way violin plot produces same coefficients", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "violin", dots = 1))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu2way_tcchange_sex_Diet_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu2way violin coefficients")
})

# --- cu2way(tcchange, sex, Diet, interact=F) — additive model ---

test_that("cu2way(interact=F) coef, posthoc, and F-tests match golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, interact = FALSE, plot = "no"))

  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cu2way_nointeract_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term",
                     label = "cu2way nointeract coefficients")

  # sex posthoc
  expect_posthoc_match(out, "cu2way_nointeract_sex_posthoc")

  # diet posthoc
  expect_posthoc_match(out, "cu2way_nointeract_diet_posthoc")

  # F-tests
  expect_partial_f_match(out, "cu2way_nointeract_f_tests")
})
