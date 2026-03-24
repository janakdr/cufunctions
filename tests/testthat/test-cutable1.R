# Tests for cutable1 based on cufuncs-tests.docx
# Golden values are in tests/testthat/golden/*.csv

attach_NEJM()
withr::defer(detach_NEJM(), teardown_env())

test_that("cutable1(tcchange, Diet) summary table matches golden", {
  out <- capture.output(cutable1(tcchange, Diet, plot = "no"))
  actual <- parse_summary_table(out)
  golden <- load_golden("cutable1_tcchange_Diet")
  expect_table_match(actual, golden, label = "cutable1(tcchange, Diet)",
                     tol = 0.01)
})

test_that("cutable1(NEJM) summary table matches golden", {
  out <- capture.output(cutable1(NEJM, plot = "no"))
  golden <- load_golden("cutable1_NEJM")
  actual <- parse_multi_chunk_table(out, row_labels = golden$stat)
  expect_table_match(actual, golden, label = "cutable1(NEJM)", tol = 0.01)
})

test_that("cutable1(NEJM, Diet, brief=T) matches golden", {
  out <- capture.output(cutable1(NEJM, Diet, brief = TRUE, plot = "no"))
  golden <- load_golden("cutable1_NEJM_Diet_brief")
  actual <- parse_brief_table(out, row_labels = golden$stat)
  expect_table_match(actual, golden, label = "cutable1(NEJM, Diet, brief=T)", tol = 0.01)
})

test_that("cutable1(NEJM, Diet, compare=T) tcchange matches golden", {
  out <- capture.output(cutable1(NEJM, Diet, compare = TRUE, plot = "no"))
  golden <- load_golden("cutable1_NEJM_Diet_compare_tcchange")
  actual <- parse_section_table(out, "^tcchange", col_names = names(golden)[-1])
  expect_table_match(actual, golden, label = "cutable1 compare tcchange")
})

test_that("cutable1(NEJM, Diet, compare=T) diffvar matches golden", {
  out <- capture.output(cutable1(NEJM, Diet, compare = TRUE, plot = "no"))
  golden <- load_golden("cutable1_NEJM_Diet_compare_diffvar")
  actual <- parse_section_table(out, "^diffvar",
                                col_names = names(golden)[-1],
                                row_labels = golden$stat)
  expect_table_match(actual, golden, label = "cutable1 compare diffvar")
})
