# Tests for cu1way
# Golden values are in tests/testthat/golden/*.csv


# --- cu1way(tcchange, Diet) — standard ANOVA path ---

test_that("cu1way(tcchange, Diet) summary table matches golden", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, plot = "no")))
  actual <- parse_summary_table(out)
  golden <- load_golden("cutable1_tcchange_Diet")
  expect_table_match(actual, golden, label = "cu1way(tcchange, Diet) summary",
                     tol = 0.01)
})

test_that("cu1way(tcchange, Diet) ANOVA table matches golden", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, plot = "no")))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(tcchange, Diet) ANOVA")
})

test_that("cu1way(tcchange, Diet) coefficients match golden", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, plot = "no")))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu1way(tcchange, Diet) coefficients")
})

test_that("cu1way(tcchange, Diet) post-hoc comparisons match golden", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, plot = "no")))
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
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, plot = "box")))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(tcchange, Diet, plot=box) ANOVA")
})

test_that("cu1way(tcchange, Diet, plot='violin', dots=1) produces same stats", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, plot = "violin", dots = 1)))
  expect_posthoc_match(out, "cu1way_tcchange_Diet_posthoc")
})

# --- cu1way plot rendering (catches ggpubr function resolution issues) ---

test_that("cu1way(tcchange, Diet) default bar plot renders without error", {
  # Default plot="bar" uses ggbarplot(add="mean_sd") which requires
  # the summary function to be resolvable. This test catches issues where
  # ggpubr functions aren't findable because the package is only imported,
  # not attached.
  expect_no_error(capture.output(with(NEJM, cu1way(tcchange, Diet))))
})

test_that("cu1way with ebars=2 (mean_se) renders without error", {
  # ebars=2 triggers ggpubr's add_summary to look up "mean_se_" (with
  # trailing underscore). Because get() searches the cufunctions namespace
  # environment for it during plot rendering, we must explicitly import and
  # re-export mean_se_ from ggpubr.
  expect_no_error(capture.output(with(NEJM, cu1way(tcchange, Diet, ebars = 2))))
})

test_that("cu1way with ebars=3 (mean_ci) renders without error", {
  expect_no_error(capture.output(with(NEJM, cu1way(tcchange, Diet, ebars = 3))))
})

# --- cu1way(tcchange, Diet, ebars=4) — nonparametric Kruskal-Wallis/Dunn ---

test_that("cu1way(tcchange, Diet, ebars=4) summary table matches golden", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, ebars = 4, plot = "no")))
  actual <- parse_summary_table(out)
  golden <- load_golden("cutable1_tcchange_Diet")
  expect_table_match(actual, golden, label = "cu1way(tcchange, Diet, ebars=4) summary",
                     tol = 0.01)
})

test_that("cu1way(tcchange, Diet, ebars=4) Kruskal-Wallis matches golden", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, ebars = 4, plot = "no")))
  expect_kw_match(out, "cu1way_tcchange_Diet_ebars4_kw")
})

test_that("cu1way(tcchange, Diet, ebars=4) Dunn pairwise matches golden", {
  out <- capture.output(with(NEJM, cu1way(tcchange, Diet, ebars = 4, plot = "no")))
  actual <- parse_dunn_table(out)
  golden <- load_golden("cu1way_tcchange_Diet_ebars4_dunn")
  expect_table_match(actual, golden,
                     label = "cu1way(tcchange, Diet, ebars=4) Dunn")
})

test_that("cu1way(tcchange, Diet, ebars=4) does not leak dunn.test messages to stderr", {
  # dunn.test emits "α = 0.05 / Reject Ho if p ≤ α" via message();
  # these must be suppressed so they don't pollute test output.
  msgs <- capture.output(
    capture.output(with(NEJM, cu1way(tcchange, Diet, ebars = 4, plot = "no"))),
    type = "message"
  )
  # Filter out any empty strings — only real messages matter
  msgs <- msgs[nzchar(trimws(msgs))]
  expect_length(msgs, 0)
})

# --- cu1way(hcstudy, Diet) — normality failure, still does ANOVA ---

test_that("cu1way(hcstudy, Diet) summary table matches golden", {
  out <- capture.output(with(NEJM, cu1way(hcstudy, Diet, plot = "no")))
  actual <- parse_summary_table(out)
  golden <- load_golden("cu1way_hcstudy_Diet")
  expect_table_match(actual, golden, label = "cu1way(hcstudy, Diet) summary")
})

test_that("cu1way(hcstudy, Diet) ANOVA matches golden", {
  out <- capture.output(with(NEJM, cu1way(hcstudy, Diet, plot = "no")))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_hcstudy_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(hcstudy, Diet) ANOVA")
})

test_that("cu1way(hcstudy, Diet) coefficients match golden", {
  out <- capture.output(with(NEJM, cu1way(hcstudy, Diet, plot = "no")))
  actual <- parse_coef_table(out)
  golden <- load_golden("cu1way_hcstudy_Diet_coef")
  expect_table_match(actual, golden, id_col = "term",
                     label = "cu1way(hcstudy, Diet) coefficients")
})

test_that("cu1way(hcstudy, Diet) post-hoc matches golden", {
  out <- capture.output(with(NEJM, cu1way(hcstudy, Diet, plot = "no",pnorm=0.01)))
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
  out <- capture.output(with(NEJM, cu1way(hcstudy, Diet, plot = "no",ebars=1,pnorm=0.05)))
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
  out <- capture.output(with(NEJM, cu1way(diffvar, Diet, plot = "no")))
  actual <- parse_summary_table(out)
  golden <- load_golden("cu1way_diffvar_Diet")
  expect_table_match(actual, golden, label = "cu1way(diffvar, Diet) summary")
})

test_that("cu1way(diffvar, Diet) ANOVA matches golden", {
  out <- capture.output(with(NEJM, cu1way(diffvar, Diet, plot = "no")))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_diffvar_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(diffvar, Diet) ANOVA")
})

test_that("cu1way(diffvar, Diet) post-hoc matches golden", {
  out <- capture.output(with(NEJM, cu1way(diffvar, Diet, plot = "no")))
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
  out <- capture.output(with(NEJM, cu1way(diffvar, Diet, plot = "no")))
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
# to do: Janak to add ebars=0 default case

test_that("cu1way(badtcp, Diet) summary table matches golden", {
  out <- capture.output(with(NEJM, cu1way(badtcp, Diet, plot = "no")))
  actual <- parse_summary_table(out)
  golden <- load_golden("cu1way_badtcp_Diet")
  expect_table_match(actual, golden, label = "cu1way(badtcp, Diet) summary")
})

test_that("cu1way(badtcp, Diet) ANOVA matches golden", {
  out <- capture.output(with(NEJM, cu1way(badtcp, Diet, plot = "no",ebars=1)))
  actual <- parse_anova_table(out)
  golden <- load_golden("cu1way_badtcp_Diet_anova")
  expect_table_match(actual, golden, id_col = "source",
                     label = "cu1way(badtcp, Diet) ANOVA", tol = 0.01)
})

test_that("cu1way(badtcp, Diet) post-hoc matches golden", {
  out <- capture.output(with(NEJM, cu1way(badtcp, Diet, plot = "no",ebars=1)))
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
  out <- capture.output(with(NEJM, cu1way(badtcp, Diet, plot = "no",ebars=1)))
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
  data(Met, envir = environment())
  out <- capture.output(with(Met, cu1way(WTCAT, MetSyn, ordinal=c("lean","overwt","obese"), plot="no")))
  
  actual <- parse_summary_table(out)

  golden <- load_golden("cu1way_ordinal_summary")
  expect_table_match(actual, golden, label = "cu1way ordinal summary", tol = 0.02)

  expect_format_match(out, "Overall Pearson's χ² p-value = %n", c(0.0107), tol = 0.02)

  expect_format_match(out, "Overall Fisher's Exact p-value = %n", c(0.0055), tol = 0.01)

  expect_format_match(out, "RR=%n, CL=[%n,%n] Fisher's Exact p=%n", c(1.54, 1.24, 1.92, 0.0046), tol = 0.02)

  rr_obese_section <- out[grep("obese vs obese", out)[1]:length(out)]
  expect_format_match(rr_obese_section, "RR=%n, CL=[%n,%n] Fisher's Exact p=%n", c(2.09, 1.07, 4.09, 0.0483), tol = 0.02)
})

