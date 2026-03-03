# Tests for cutable1 based on cufuncs-tests.docx
# Golden values are in tests/testthat/golden/*.csv

attach_NEJM()
withr::defer(detach_NEJM(), teardown_env())

test_that("cutable1(tcchange, Diet) summary table matches golden", {
  out <- capture.output(cutable1(tcchange, Diet, plot = "no"))
  actual <- parse_summary_table(out)
  golden <- load_golden("cutable1_tcchange_Diet")
  expect_table_match(actual, golden, label = "cutable1(tcchange, Diet)")
})

test_that("cutable1(NEJM) summary table matches golden", {
  out <- capture.output(cutable1(NEJM, plot = "no"))
  golden <- load_golden("cutable1_NEJM")
  row_labels <- golden$stat

  # Find header lines (start with whitespace, contain column names)
  hdr_indices <- which(grepl("^\\s+\\S", out))

  # Parse each chunk and merge
  actual <- NULL
  for (h in hdr_indices) {
    end <- h + 1
    while (end <= length(out) && trimws(out[end]) != "")
      end <- end + 1
    chunk <- parse_fixed_width_table(out[h], out[(h + 1):(end - 1)],
                                     row_labels = row_labels)
    if (is.null(actual)) actual <- chunk
    else actual <- merge(actual, chunk, by = "stat", all = TRUE, sort = FALSE)
  }
  actual <- actual[match(row_labels, actual$stat), ]
  expect_table_match(actual, golden, label = "cutable1(NEJM)")
})

test_that("cutable1(NEJM, Diet, brief=T) matches golden", {
  out <- capture.output(cutable1(NEJM, Diet, brief = TRUE, plot = "no"))
  golden <- load_golden("cutable1_NEJM_Diet_brief")

  # Header is the first line starting with whitespace
  hdr_idx <- which(grepl("^\\s+\\S", out))
  end <- hdr_idx[1] + 1
  while (end <= length(out) && trimws(out[end]) != "")
    end <- end + 1
  actual <- parse_fixed_width_table(out[hdr_idx[1]], out[(hdr_idx[1] + 1):(end - 1)],
                                    row_labels = golden$stat)
  expect_table_match(actual, golden, label = "cutable1(NEJM, Diet, brief=T)")
})

test_that("cutable1(NEJM, Diet, compare=T) tcchange matches golden", {
  out <- capture.output(cutable1(NEJM, Diet, compare = TRUE, plot = "no"))
  golden <- load_golden("cutable1_NEJM_Diet_compare_tcchange")

  # Find the tcchange section
  tc_idx <- grep("^tcchange", out)
  col_names <- names(golden)[-1]
  end <- tc_idx[1] + 2
  while (end <= length(out) && trimws(out[end]) != "") end <- end + 1
  actual <- parse_fixed_width_table(out[tc_idx[1] + 1],
                                    out[(tc_idx[1] + 2):(end - 1)],
                                    col_names = col_names)
  expect_table_match(actual, golden, label = "cutable1 compare tcchange")
})

test_that("cutable1(NEJM, Diet, compare=T) diffvar matches golden", {
  out <- capture.output(cutable1(NEJM, Diet, compare = TRUE, plot = "no"))
  golden <- load_golden("cutable1_NEJM_Diet_compare_diffvar")

  # Find the diffvar section
  dv_idx <- grep("^diffvar", out)
  col_names <- names(golden)[-1]
  row_labels <- golden$stat
  end <- dv_idx[1] + 2
  while (end <= length(out) && trimws(out[end]) != "") end <- end + 1
  actual <- parse_fixed_width_table(out[dv_idx[1] + 1],
                                    out[(dv_idx[1] + 2):(end - 1)],
                                    col_names = col_names,
                                    row_labels = row_labels)
  expect_table_match(actual, golden, label = "cutable1 compare diffvar")
})
