# Tests for cu2way based on cufunctions-tests-with-output.txt
# Golden values are in tests/testthat/golden/*.csv

attach_NEJM()
withr::defer(detach_NEJM(), teardown_env())

# --- cu2way(tcchange, sex, Diet) — full interaction model ---

test_that("cu2way(tcchange, sex, Diet) summary table matches golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "no"))
  golden <- load_golden("cu2way_tcchange_sex_Diet_summary")
  # Find header line containing column names (skip title line)
  hdr <- grep("All", out, fixed = TRUE)[1]
  end <- hdr + 1
  while (end <= length(out) && trimws(out[end]) != "") end <- end + 1
  actual <- parse_fixed_width_table(out[hdr], out[(hdr + 1):(end - 1)],
                                    row_labels = golden$stat)
  expect_table_match(actual, golden, label = "cu2way summary", tol = 0.01)
})

test_that("cu2way(tcchange, sex, Diet) pairwise matrix matches golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "no"))
  actual <- parse_pairwise_matrix(out)
  golden <- load_golden("cu2way_tcchange_sex_Diet_pairwise")
  expect_table_match(actual, golden, label = "cu2way pairwise")
})

test_that("cu2way(tcchange, sex, Diet) coefficients match golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "no"))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu2way_tcchange_sex_Diet_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu2way coefficients")
})

test_that("cu2way(tcchange, sex, Diet) sex posthoc matches golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "no"))
  expect_grouped_posthoc_match(out, "cu2way_tcchange_sex_Diet_sex_posthoc")
})

test_that("cu2way(tcchange, sex, Diet) diet posthoc matches golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "no"))
  expect_grouped_posthoc_match(out, "cu2way_tcchange_sex_Diet_diet_posthoc")
})

test_that("cu2way partial F-test output present", {
  out <- capture.output(cu2way(tcchange, sex, Diet, plot = "no"))
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expect_match(out_text, "vs model with no interaction", fixed = TRUE)
  expect_match(out_text, "vs model with just sex", fixed = TRUE)
  expect_match(out_text, "vs model with just Diet", fixed = TRUE)
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

test_that("cu2way(interact=F) coefficients match golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, interact = FALSE, plot = "no"))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu2way_nointeract_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu2way nointeract coefficients")
})

test_that("cu2way(interact=F) sex posthoc matches golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, interact = FALSE, plot = "no"))
  expect_posthoc_match(out, "cu2way_nointeract_sex_posthoc")
})

test_that("cu2way(interact=F) diet posthoc matches golden", {
  out <- capture.output(cu2way(tcchange, sex, Diet, interact = FALSE, plot = "no"))
  expect_posthoc_match(out, "cu2way_nointeract_diet_posthoc")
})

test_that("cu2way(interact=F) partial F-test output present", {
  out <- capture.output(cu2way(tcchange, sex, Diet, interact = FALSE, plot = "no"))
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expect_match(out_text, "vs model with just sex", fixed = TRUE)
  expect_match(out_text, "vs model with just Diet", fixed = TRUE)
})
