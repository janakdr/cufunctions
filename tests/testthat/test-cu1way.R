# Tests for cu1way based on cufuncs-tests.docx
# Golden values are in tests/testthat/golden/*.csv

attach_NEJM()
withr::defer(detach_NEJM(), teardown_env())

# --- cu1way(tcchange, Diet) — standard ANOVA path ---

test_that("cu1way(tcchange, Diet) summary table matches golden", {
  out <- capture.output(cu1way(tcchange, Diet, plot = "no"))
  actual <- parse_summary_table(out)
  golden <- load_golden("cutable1_tcchange_Diet")
  expect_table_match(actual, golden, label = "cu1way(tcchange, Diet) summary",
                     tol = 0.01)
})

test_that("cu1way(tcchange, Diet) ANOVA table matches golden", {
  out <- capture.output(cu1way(tcchange, Diet, plot = "no"))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(tcchange, Diet) ANOVA")
})

test_that("cu1way(tcchange, Diet) coefficients match golden", {
  out <- capture.output(cu1way(tcchange, Diet, plot = "no"))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu1way(tcchange, Diet) coefficients")
})

test_that("cu1way(tcchange, Diet) post-hoc comparisons match golden", {
  out <- capture.output(cu1way(tcchange, Diet, plot = "no"))
  expect_posthoc_match(out, "cu1way_tcchange_Diet_posthoc")
  # Pairwise method description
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expected_method <- paste(
    "Pairwise comparisons using t tests with pooled SD 12.4",
    "",
    "data:  tcchange compared across 3 Diet groups using Fisher's LSD",
    sep = "\n"
  )
  expect_match(out_text, expected_method, fixed = TRUE)
})

# --- cu1way(tcchange, Diet) — plot variants produce identical stats ---

test_that("cu1way(tcchange, Diet, plot='box') produces same stats", {
  out <- capture.output(cu1way(tcchange, Diet, plot = "box"))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(tcchange, Diet, plot=box) ANOVA")
})

test_that("cu1way(tcchange, Diet, plot='violin', dots=1) produces same stats", {
  out <- capture.output(cu1way(tcchange, Diet, plot = "violin", dots = 1))
  expect_posthoc_match(out, "cu1way_tcchange_Diet_posthoc")
})

# --- cu1way plot rendering (catches ggpubr function resolution issues) ---

test_that("cu1way(tcchange, Diet) default bar plot renders without error", {
  # Default plot="bar" uses ggbarplot(add="ggpubr::mean_sd") which requires
  # the summary function to be resolvable. This test catches issues where
  # ggpubr functions aren't findable because the package is only imported,
  # not attached.
  expect_no_error(capture.output(cu1way(tcchange, Diet)))
})

# --- cu1way(tcchange, Diet, ebars=4) — nonparametric Kruskal-Wallis/Dunn ---

test_that("cu1way(tcchange, Diet, ebars=4) summary table matches golden", {
  out <- capture.output(cu1way(tcchange, Diet, ebars = 4, plot = "no"))
  actual <- parse_summary_table(out)
  golden <- load_golden("cutable1_tcchange_Diet")
  expect_table_match(actual, golden, label = "cu1way(tcchange, Diet, ebars=4) summary",
                     tol = 0.01)
})

test_that("cu1way(tcchange, Diet, ebars=4) Kruskal-Wallis matches golden", {
  out <- capture.output(cu1way(tcchange, Diet, ebars = 4, plot = "no"))
  expect_kw_match(out, "cu1way_tcchange_Diet_ebars4_kw")
})

test_that("cu1way(tcchange, Diet, ebars=4) Dunn pairwise matches golden", {
  out <- capture.output(cu1way(tcchange, Diet, ebars = 4, plot = "no"))
  actual <- parse_dunn_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_ebars4_dunn")
  expect_table_match(actual, golden,
                     label = "cu1way(tcchange, Diet, ebars=4) Dunn")
})

# --- cu1way(hcstudy, Diet) — normality failure, still does ANOVA ---

test_that("cu1way(hcstudy, Diet) summary table matches golden", {
  out <- capture.output(cu1way(hcstudy, Diet, plot = "no"))
  actual <- parse_summary_table(out)
  golden <- load_golden("cu1way_hcstudy_Diet")
  expect_table_match(actual, golden, label = "cu1way(hcstudy, Diet) summary")
})

test_that("cu1way(hcstudy, Diet) ANOVA matches golden", {
  out <- capture.output(cu1way(hcstudy, Diet, plot = "no"))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_hcstudy_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(hcstudy, Diet) ANOVA")
})

test_that("cu1way(hcstudy, Diet) coefficients match golden", {
  out <- capture.output(cu1way(hcstudy, Diet, plot = "no"))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu1way_hcstudy_Diet_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu1way(hcstudy, Diet) coefficients")
})

test_that("cu1way(hcstudy, Diet) post-hoc matches golden", {
  out <- capture.output(cu1way(hcstudy, Diet, plot = "no"))
  expect_posthoc_match(out, "cu1way_hcstudy_Diet_posthoc", tol = 0.01)
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expected_method <- paste(
    "Pairwise comparisons using t tests with pooled SD 7.38",
    "",
    "data:  hcstudy compared across 3 Diet groups using Fisher's LSD",
    sep = "\n"
  )
  expect_match(out_text, expected_method, fixed = TRUE)
})

test_that("cu1way(hcstudy, Diet) warns about normality failure", {
  out <- capture.output(cu1way(hcstudy, Diet, plot = "no"))
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expected_warning <- paste(
    "DATA FAIL NORMALITY TEST IN 1 OF 3 GROUPs. SMALLEST P-VALUE 0.027",
    "LOOK FOR DATA ERRORS IN 'Min' AND 'Max' VALUES.",
    "IF DATA ARE NOT NORMAL,",
    "YOU SHOULD USE THE NONPARAMETRIC DUNN TEST (ebars=4)",
    sep = "\n"
  )
  expect_match(out_text, expected_warning, fixed = TRUE)
})

# --- cu1way(diffvar, Diet) — Bartlett failure, unequal variances ---

test_that("cu1way(diffvar, Diet) summary table matches golden", {
  out <- capture.output(cu1way(diffvar, Diet, plot = "no"))
  actual <- parse_summary_table(out)
  golden <- load_golden("cu1way_diffvar_Diet")
  expect_table_match(actual, golden, label = "cu1way(diffvar, Diet) summary")
})

test_that("cu1way(diffvar, Diet) ANOVA matches golden", {
  out <- capture.output(cu1way(diffvar, Diet, plot = "no"))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_diffvar_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(diffvar, Diet) ANOVA")
})

test_that("cu1way(diffvar, Diet) post-hoc matches golden", {
  out <- capture.output(cu1way(diffvar, Diet, plot = "no"))
  expect_posthoc_match(out, "cu1way_diffvar_Diet_posthoc")
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expected_method <- paste(
    "Pairwise comparisons using t tests with non-pooled SD",
    "",
    "data:  diffvar compared across 3 Diet groups by Welch's t (unequal variances)",
    sep = "\n"
  )
  expect_match(out_text, expected_method, fixed = TRUE)
})

test_that("cu1way(diffvar, Diet) reports Bartlett failure", {
  out <- capture.output(cu1way(diffvar, Diet, plot = "no"))
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expected_bartlett <- paste(
    "Data fail the Bartlett test for homogeneity of variances (p=0.000272 for chi-sq=16.4 with 2 df)",
    "COULD NOT USE POOLED SD DUE TO UNEQUAL VARIANCES.",
    "LOOK FOR DATA ERRORS, FOCUSING ON GROUP(S) WITH LARGE SD AND UNEXPECTED MIN/MAX IN SUMMARY ABOVE.",
    "If you wish to pool variances despite failing Bartlett, redo cu1way adding pbart=x, where x< 0.000272",
    sep = "\n"
  )
  expect_match(out_text, expected_bartlett, fixed = TRUE)
})

# --- cu1way(badtcp, Diet) — normality + Bartlett failure ---

test_that("cu1way(badtcp, Diet) summary table matches golden", {
  out <- capture.output(cu1way(badtcp, Diet, plot = "no"))
  actual <- parse_summary_table(out)
  golden <- load_golden("cu1way_badtcp_Diet")
  expect_table_match(actual, golden, label = "cu1way(badtcp, Diet) summary")
})

test_that("cu1way(badtcp, Diet) ANOVA matches golden", {
  out <- capture.output(cu1way(badtcp, Diet, plot = "no"))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_badtcp_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(badtcp, Diet) ANOVA", tol = 0.01)
})

test_that("cu1way(badtcp, Diet) post-hoc matches golden", {
  out <- capture.output(cu1way(badtcp, Diet, plot = "no"))
  expect_posthoc_match(out, "cu1way_badtcp_Diet_posthoc")
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expected_method <- paste(
    "Pairwise comparisons using t tests with non-pooled SD",
    "",
    "data:  badtcp compared across 3 Diet groups by Welch's t (unequal variances)",
    sep = "\n"
  )
  expect_match(out_text, expected_method, fixed = TRUE)
})

test_that("cu1way(badtcp, Diet) warns about normality and Bartlett failure", {
  out <- capture.output(cu1way(badtcp, Diet, plot = "no"))
  out_text <- paste(trimws(out, "right"), collapse = "\n")
  expected_normality <- paste(
    "DATA FAIL NORMALITY TEST IN 1 OF 3 GROUPs. SMALLEST P-VALUE <0.001",
    "LOOK FOR DATA ERRORS IN 'Min' AND 'Max' VALUES.",
    "IF DATA ARE NOT NORMAL,",
    "YOU SHOULD USE THE NONPARAMETRIC DUNN TEST (ebars=4)",
    sep = "\n"
  )
  expect_match(out_text, expected_normality, fixed = TRUE)

  expected_bartlett <- paste(
    "Data fail the Bartlett test for homogeneity of variances (p=0.00176 for chi-sq=12.7 with 2 df)",
    "COULD NOT USE POOLED SD DUE TO UNEQUAL VARIANCES.",
    sep = "\n"
  )
  expect_match(out_text, expected_bartlett, fixed = TRUE)
})

# --- cu1way with Ordinal Outcomes ---

test_that("cu1way(WTCAT, MetSyn, ordinal) summary matches golden", {
  data(AJCN, envir = environment())
  out <- capture.output(with(AJCN, cu1way(WTCAT, MetSyn, ordinal=c("lean","overwt","obese"), plot="no")))
  
  actual <- parse_summary_table(out)

  golden <- load_golden("cu1way_ordinal_summary")
  expect_table_match(actual, golden, label = "cu1way ordinal summary", tol = 0.02)
})

