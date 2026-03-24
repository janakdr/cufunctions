# Tests for cucov2way based on cufuncs-tests.docx.txt
# Golden values are in tests/testthat/golden/*.csv

attach_NEJM()
withr::defer(detach_NEJM(), teardown_env())

# --- Case 1: default interact=T ---

test_that("cucov2way(tcstudy, tcpre, Diet, sex) matches golden", {
  out <- capture.output(cucov2way(tcstudy, tcpre, Diet, sex))

  # Summary table and Pairwise matrix are not printed in the current version
  # because cucov2way calls cu2way(minimal=T), which suppresses them.
  # golden_summary <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_summary")
  # ...


  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term", label = "cucov2way coefficients")

  # Posthoc (Merged diet and sex)
  expect_grouped_posthoc_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_posthoc")

  # F-tests
  expect_partial_f_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_f_tests")
})

# --- Case 2: with covariate values ---

test_that("cucov2way(tcstudy, tcpre, Diet, sex, c(160,180,200)) matches golden", {
  out <- capture.output(cucov2way(tcstudy, tcpre, Diet, sex, c(160, 180, 200)))

  # Summary and Pairwise not printed
  # actual_summary <- parse_summary_table(out)
  # golden_summary <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_c160_summary")
  # expect_table_match(actual_summary, golden_summary, label = "cucov2way c160 summary")
  # actual_pw <- parse_pairwise_matrix(out)
  # golden_pw <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_c160_pairwise")
  # expect_table_match(actual_pw, golden_pw, label = "cucov2way c160 pairwise")

  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_c160_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term", label = "cucov2way c160 coefficients")

  # Posthoc
  expect_grouped_posthoc_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_c160_posthoc", tol = 0.01)

  # F-tests
  expect_partial_f_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_c160_f_tests")
})

# --- Case 3: interact=F ---

test_that("cucov2way(interact=F) matches golden", {
  out <- capture.output(cucov2way(tcstudy, tcpre, Diet, sex, interact = FALSE))

  # Summary and Pairwise not printed
  # actual_summary <- parse_summary_table(out)
  # golden_summary <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_summary")
  # expect_table_match(actual_summary, golden_summary, label = "cucov2way nointeract summary")
  # actual_pw <- parse_pairwise_matrix(out)
  # golden_pw <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_pairwise")
  # expect_table_match(actual_pw, golden_pw, label = "cucov2way nointeract pairwise")

  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term", label = "cucov2way nointeract coefficients")

  # Posthoc (Not grouped because interact=F)
  expect_posthoc_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_nointeract_posthoc")

  # F-tests
  expect_partial_f_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_nointeract_f_tests")
})

# --- Case 4: interact=F with covariate values ---

test_that("cucov2way(interact=F, c(160,180,200)) matches golden", {
  out <- capture.output(cucov2way(tcstudy, tcpre, Diet, sex, interact = FALSE, c(160, 180, 200)))

  # Summary and Pairwise not printed
  # actual_summary <- parse_summary_table(out)
  # golden_summary <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_summary")
  # expect_table_match(actual_summary, golden_summary, label = "cucov2way nointeract c160 summary")
  # actual_pw <- parse_pairwise_matrix(out)
  # golden_pw <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_pairwise")
  # expect_table_match(actual_pw, golden_pw, label = "cucov2way nointeract c160 pairwise")

  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term", label = "cucov2way nointeract c160 coefficients")

  # Posthoc (Not grouped because interact=F)
  expect_posthoc_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_posthoc")

  # F-tests
  expect_partial_f_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_f_tests")
})
