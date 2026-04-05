# Tests for cu2way based on cufunctions-tests-with-output.txt
# Golden values are in tests/testthat/golden/*.csv


# --- cu2way(tcchange, sex, Diet) — full interaction model ---

test_that("cu2way(tcchange, sex, Diet) summary, pairwise, coef, posthoc, and F-tests match golden", {
  out <- capture.output(with(NEJM, cu2way(tcchange, sex, Diet, plot = "no",ebars=1)))

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
  expect_grouped_posthoc_match(out, "cu2way_tcchange_sex_Diet_sex_posthoc", tol = 0.01)

  # diet posthoc
  expect_grouped_posthoc_match(out, "cu2way_tcchange_sex_Diet_diet_posthoc")

  # F-tests
  expect_partial_f_match(out, "cu2way_tcchange_sex_Diet_f_tests")
})

# --- cu2way(tcchange, sex, Diet, plot="violin") — same stats ---

test_that("cu2way violin plot produces same coefficients", {
  out <- capture.output(with(NEJM, cu2way(tcchange, sex, Diet, plot = "violin", dots = 1, ebars=1)))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu2way_tcchange_sex_Diet_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu2way violin coefficients")
})

# --- cu2way(tcchange, sex, Diet, interact=F) — additive model ---

test_that("cu2way(interact=F) coef, posthoc, and F-tests match golden", {
  out <- capture.output(with(NEJM, cu2way(tcchange, sex, Diet, interact = FALSE, plot = "no", ebars=1)))

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

# --- cu2way with Ordinal Outcomes ---

test_that("cu2way(feel, WTCAT, Sex, ordinal) counts match golden", {
  data(Met, envir = environment())
  out <- capture.output(with(Met, cu2way(feel, WTCAT, Sex, g1order=c("lean","overwt","obese"), ordinal=c("bad","ok","good"))))
  
  counts_lines <- extract_section_lines(out, "     lean&F")
  expect_true(!is.null(counts_lines))
  
  data_lines <- counts_lines[2:length(counts_lines)]
  # Remove descriptive footnote if it got appended to output lines
  data_lines <- data_lines[!grepl("^\\[Note that outcome", data_lines)]
  
  actual <- parse_fixed_width_table("     lean&F lean&M overwt&F overwt&M obese&F obese&M", 
                                    data_lines, id_col = "stat")
  
  golden <- load_golden("cu2way_ordinal_counts")
  expect_table_match(actual, golden, id_col = "stat", label = "cu2way ordinal counts", tol = 0.02)
})

test_that("cu2way(feel, WTCAT, Sex, ordinal) posthoc matches golden", {
  data(Met, envir = environment())
  out <- capture.output(with(Met, cu2way(feel, WTCAT, Sex, g1order=c("lean","overwt","obese"), ordinal=c("bad","ok","good"))))
  expect_ordinal_posthoc_match(out, "cu2way_ordinal_posthoc", tol = 0.02)

  expect_format_match(out, "Overall Fisher's Exact p-value = %n", c(5.00e-04), tol = 0.02)

  golden_log <- load_golden("cu2way_log_ratios")

  # Group 1: bad vs > bad
  rows_bad <- golden_log[golden_log$group == "bad vs > bad in:", ]
  idx_bad <- which(grepl("bad vs > bad in:", out))
  sub_bad <- out[idx_bad:length(out)]
  for (i in seq_len(nrow(rows_bad))) {
    row <- rows_bad[i, ]
    pattern <- paste0(row$comparison, ": log(%n)±%n vs log(%n)±%n p=%n")
    gold_nums <- c(row$v1, row$se1, row$v2, row$se2, row$p)
    expect_format_match(sub_bad, pattern, gold_nums, tol = 0.02)
  }

  # Group 2: <good vs good
  rows_good <- golden_log[golden_log$group == "<good vs good in:", ]
  idx_good <- which(grepl("<good vs good in:", out))
  sub_good <- out[idx_good:length(out)]
  for (i in seq_len(nrow(rows_good))) {
    row <- rows_good[i, ]
    pattern <- paste0(row$comparison, ": log(%n)±%n vs log(%n)±%n p=%n")
    gold_nums <- c(row$v1, row$se1, row$v2, row$se2, row$p)
    expect_format_match(sub_good, pattern, gold_nums, tol = 0.02)
  }
})

test_that("cu2way(feel, WTCAT, Sex, ordinal, scale='percent') produces same results", {
  data(Met, envir = environment())
  out <- capture.output(with(Met, cu2way(feel, WTCAT, Sex,
      g1order=c("lean","overwt","obese"),
      ordinal=c("bad","ok","good"), scale="percent")))

  # The statistical output is identical to the non-percent variant;
  # scale="percent" only affects the plot. Verify posthoc matches same golden.
  expect_ordinal_posthoc_match(out, "cu2way_ordinal_posthoc", tol = 0.02)
})

# --- cu2way with missing factor-level combinations ---

test_that("cu2way handles missing group1*group2 combinations", {
  # Create data where group1 has levels A, B, C and group2 has levels X, Y,
  # but the C&Y combination has no observations. This exercises the macmap()
  # machinery that maps between all possible combinations (from interaction())
  # and the actually-present combinations (from factor(paste())).
  set.seed(42)
  depvar <- c(rnorm(5, 10, 2),   # A&X
              rnorm(5, 12, 2),   # A&Y
              rnorm(5, 11, 2),   # B&X
              rnorm(5, 13, 2),   # B&Y
              rnorm(5, 10, 2))   # C&X only — no C&Y
  group1 <- factor(c(rep("A", 10), rep("B", 10), rep("C", 5)))
  group2 <- factor(c(rep("X", 5), rep("Y", 5),
                      rep("X", 5), rep("Y", 5),
                      rep("X", 5)))

  out <- capture.output(cu2way(depvar, group1, group2, plot = "no", ebars = 1))
  full_out <- paste(out, collapse = "\n")

  # --- Core behavioral checks for missing combinations ---

  # Should report 5 combination levels (not 6)
  expect_match(full_out, "5 group1&group2 groups")

  # C&Y should not appear anywhere in output
  expect_no_match(full_out, "C&Y")

  # Y-group comparisons should only show B minus A (C has no Y data)
  expect_match(full_out, "Y \\n.*B minus A")

  # --- Exact golden-value assertions ---

  # Summary table
  golden_summary <- load_golden("cu2way_missing_combo_summary")
  hdr <- grep("All", out, fixed = TRUE)[1]
  end <- hdr + 1
  while (end <= length(out) && trimws(out[end]) != "") end <- end + 1
  actual_summary <- parse_fixed_width_table(out[hdr], out[(hdr + 1):(end - 1)],
                                             row_labels = golden_summary$stat)
  expect_table_match(actual_summary, golden_summary,
                     label = "missing-combo summary", tol = 0.01)

  # Coefficients (5 terms: intercept=A&X, then A&Y, B&X, B&Y, C&X — no C&Y)
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cu2way_missing_combo_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term",
                     label = "missing-combo coefficients", tol = 0.01)

  # Verify only 4 non-intercept coefficients (5 groups - 1 baseline)
  expect_equal(nrow(actual_coef), nrow(golden_coef),
    info = "Should have 5 coefficient rows (intercept + 4 groups)")

  # Pairwise matrix should have 5 groups (C&X present, C&Y absent)
  actual_pw <- parse_pairwise_matrix(out)
  expect_true(!is.null(actual_pw))
  expect_true("C&X" %in% actual_pw$stat,
    info = "C&X should appear in pairwise matrix rows")
  expect_true("C&X" %in% colnames(actual_pw) || "C&X" %in% actual_pw$stat,
    info = "C&X should be in pairwise matrix")

  # Partial F-tests should be present
  expect_match(full_out, "Partial F-test vs simpler models")
})
