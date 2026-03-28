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
  expect_grouped_posthoc_match(out, "cu2way_tcchange_sex_Diet_sex_posthoc")

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
  data(AJCN, envir = environment())
  out <- capture.output(with(AJCN, cu2way(feel, WTCAT, Sex, g1order=c("lean","overwt","obese"), ordinal=c("bad","ok","good"))))
  
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
  data(AJCN, envir = environment())
  out <- capture.output(with(AJCN, cu2way(feel, WTCAT, Sex, g1order=c("lean","overwt","obese"), ordinal=c("bad","ok","good"))))
  expect_ordinal_posthoc_match(out, "cu2way_ordinal_posthoc", tol = 0.02)
})

test_that("cu2way(feel, WTCAT, Sex, ordinal, scale='percent') produces same results", {
  data(AJCN, envir = environment())
  out <- capture.output(with(AJCN, cu2way(feel, WTCAT, Sex,
      g1order=c("lean","overwt","obese"),
      ordinal=c("bad","ok","good"), scale="percent")))

  # The statistical output is identical to the non-percent variant;
  # scale="percent" only affects the plot. Verify posthoc matches same golden.
  expect_ordinal_posthoc_match(out, "cu2way_ordinal_posthoc", tol = 0.02)
})


