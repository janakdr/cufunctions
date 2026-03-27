# Tests for cucov2way based on cufuncs-tests.docx.txt
# Golden values are in tests/testthat/golden/*.csv


# --- Case 1: default interact=T ---

test_that("cucov2way(tcstudy, tcpre, Diet, sex) matches golden", {
  out <- capture.output(with(NEJM, cucov2way(tcstudy, tcpre, Diet, sex)))

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
  out <- capture.output(with(NEJM, cucov2way(tcstudy, tcpre, Diet, sex, c(160, 180, 200))))

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
  out <- capture.output(with(NEJM, cucov2way(tcstudy, tcpre, Diet, sex, interact = FALSE)))

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
  out <- capture.output(with(NEJM, cucov2way(tcstudy, tcpre, Diet, sex, interact = FALSE, c(160, 180, 200))))

  # Coefficients
  actual_coef <- parse_coef_table(out)
  golden_coef <- load_golden("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_coef")
  expect_table_match(actual_coef, golden_coef, id_col = "term", label = "cucov2way nointeract c160 coefficients")

  # Posthoc (Not grouped because interact=F)
  expect_posthoc_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_posthoc")

  # Slopes within Diet levels
  expect_format_match(
    out,
    "AAD: %n ± %n, p=%n, CL=[%n,%n]",
    c(1.03, 0.19, 8.71e-06, 0.636, 1.41),
    tol = 0.02
  )
  expect_format_match(
    out,
    "Mono: %n ± %n, p=%n, CL=[%n,%n]",
    c(0.963, 0.171, 4.5e-06, 0.613, 1.31),
    tol = 0.02
  )
  expect_format_match(
    out,
    "Step1: %n ± %n, p=%n, CL=[%n,%n]",
    c(0.79, 0.224, 0.00141, 0.332, 1.25),
    tol = 0.02
  )

  # Slope comparisons between Diet levels
  expect_format_match(
    out,
    "Mono minus AAD: %n ± %n, p=%n, CL=[%n,%n]",
    c(-0.0622, 0.255, 0.809, -0.583, 0.459),
    tol = 0.02
  )

  # F-tests
  expect_partial_f_match(out, "cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160_f_tests")
})
