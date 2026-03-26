# Tests for helper-golden.R functions
# Focus on failure cases — correct passes are validated by real tests.

#' Write a temp golden CSV that is auto-cleaned up after the test.
#' @param df Data frame to write
#' @param name Basename for the golden file (without .csv)
#' @param envir Environment for cleanup (default: caller's)
with_temp_golden <- function(df, name, envir = parent.frame()) {
  path <- test_path("golden", paste0(name, ".csv"))
  write.csv(df, path, row.names = FALSE)
  withr::defer(unlink(path), envir = envir)
  name
}

# --- compare_values ---

test_that("compare_values detects numeric mismatch (exact)", {
  expect_equal(compare_values("3.14", "3.15"), "expected 3.14, got 3.15")
})

test_that("compare_values passes on exact numeric match", {
  expect_null(compare_values("3.14", "3.14"))
})

test_that("compare_values detects mismatch outside tolerance", {
  # 10% off on value of 100
  msg <- compare_values("100", "111", tol = 0.10)
  expect_match(msg, "Mean relative difference")
})

test_that("compare_values passes within tolerance", {
  expect_null(compare_values("100", "100.5", tol = 0.01))
})

test_that("compare_values detects empty vs non-empty", {
  expect_match(compare_values("", "3.14"), "<empty>")
  expect_match(compare_values("3.14", ""), "<empty>")
})

test_that("compare_values treats both-empty as match", {
  expect_null(compare_values("", ""))
  expect_null(compare_values(NA, ""))
  expect_null(compare_values("NA", NA))
})

test_that("compare_values detects non-numeric string mismatch", {
  expect_equal(compare_values("<.001", "<.01"), "expected '<.001', got '<.01'")
})

test_that("compare_values passes on equal non-numeric strings", {
  expect_null(compare_values("<.001", "<.001"))
})

test_that("compare_values applies tolerance to numeric substrings in compound strings", {
  # Compound strings like "37.4±6.91" have numeric parts extracted and compared
  expect_null(compare_values("37.4\u00b16.91", "37.4\u00b16.92", tol = 0.02))
  expect_null(compare_values("26.1(23.7:29.6)", "26(23.7:29.6)", tol = 0.01))
  # Still fails if a component exceeds tolerance
  expect_true(!is.null(compare_values("37.4\u00b16.91", "37.4\u00b17.50", tol = 0.02)))
  # Strings with no numeric parts use exact match even with tolerance
  expect_equal(compare_values("abc", "def", tol = 1.0),
               "expected 'abc', got 'def'")
  # Exact match always passes
  expect_null(compare_values("<.001", "<.001", tol = 1.0))
})

test_that("compare_values detects string mismatch when numeric parts match", {
  expect_match(compare_values("A01", "B01", tol = 0.1), "expected 'A01', got 'B01'")
})

# --- parse_fixed_width_table ---

test_that("parse_fixed_width_table extracts values by column position", {
  hdr  <- "        AAD   Mono  Step1"
  data <- c("Mean   1.23   4.56   7.89",
            "SD     0.1    0.2    0.3")
  result <- parse_fixed_width_table(hdr, data)
  expect_equal(nrow(result), 2)
  expect_equal(result$stat, c("Mean", "SD"))
  expect_equal(result$AAD, c("1.23", "0.1"))
})

test_that("parse_fixed_width_table stops at blank line", {
  hdr  <- "        A    B"
  data <- c("X      1    2", "", "Y      3    4")
  result <- parse_fixed_width_table(hdr, data)
  expect_equal(nrow(result), 1)
  expect_equal(result$stat, "X")
})

test_that("parse_fixed_width_table uses row_labels for multi-word labels", {
  hdr  <- "        A    B"
  data <- c("1st Q   1    2")
  result <- parse_fixed_width_table(hdr, data, row_labels = c("1st Q"))
  expect_equal(result$stat, "1st Q")
})

# --- re_escape ---

test_that("re_escape escapes regex metacharacters", {
  expect_equal(re_escape("Step1(+)"), "Step1\\(\\+\\)")
  expect_equal(re_escape("x.y"), "x\\.y")
  # Letters and minus are not escaped
  expect_equal(re_escape("Mono minus AAD"), "Mono minus AAD")
})

# --- expect_posthoc_match failure cases ---

test_that("expect_posthoc_match fails on missing comparison line", {
  fake_output <- c("Some other text", "No posthoc here")
  gn <- with_temp_golden(
    data.frame(comparison = "A minus B", diff = 1, se = 0.5,
               p = 0.05, cl_lo = -0.5, cl_hi = 2.5,
               stringsAsFactors = FALSE),
    "test_posthoc_missing")
  expect_error(
    expect_posthoc_match(fake_output, gn, tol = 0),
    "not found in output"
  )
})

test_that("expect_posthoc_match fails on value mismatch", {
  # Output has diff=1.5 but golden says diff=1.0
  fake_output <- "A minus B: 1.5 ± 0.5, p=0.05, CL=[-0.5,2.5]"
  gn <- with_temp_golden(
    data.frame(comparison = "A minus B", diff = 1.0, se = 0.5,
               p = 0.05, cl_lo = -0.5, cl_hi = 2.5,
               stringsAsFactors = FALSE),
    "test_posthoc_mismatch")
  expect_error(
    expect_posthoc_match(fake_output, gn, tol = 0),
    "col 'diff'"
  )
})

test_that("expect_posthoc_match accepts values within tolerance", {
  # Output has diff=1.5 but golden says diff=1.0; tol=0.6 should pass
  fake_output <- "A minus B: 1.5 \u00b1 0.5, p=0.05, CL=[-0.5,2.5]"
  gn <- with_temp_golden(
    data.frame(comparison = "A minus B", diff = 1.0, se = 0.5,
               p = 0.05, cl_lo = -0.5, cl_hi = 2.5,
               stringsAsFactors = FALSE),
    "test_posthoc_tolerance_pass")
  expect_posthoc_match(fake_output, gn, tol = 0.6)
})

test_that("grouped posthoc rejects partial group name match", {
  fake_output <- c("Monopoly",
                   "M minus F: 5.0 ± 1.0, p=0.01, CL=[3,7]")
  gn <- with_temp_golden(
    data.frame(group = "Mono", comparison = "M minus F",
               diff = 5.0, se = 1.0, p = 0.01, cl_lo = 3, cl_hi = 7,
               stringsAsFactors = FALSE),
    "test_grouped_partial")
  expect_error(
    expect_grouped_posthoc_match(fake_output, gn, tol = 0),
    "group header not found"
  )
})

test_that("grouped posthoc matches exact group name", {
  fake_output <- c("Monopoly", "Mono",
                   "M minus F: 5.0 ± 1.0, p=0.01, CL=[3,7]")
  gn <- with_temp_golden(
    data.frame(group = "Mono", comparison = "M minus F",
               diff = 5.0, se = 1.0, p = 0.01, cl_lo = 3, cl_hi = 7,
               stringsAsFactors = FALSE),
    "test_grouped_exact")
  expect_grouped_posthoc_match(fake_output, gn, tol = 0)
})

test_that("grouped posthoc reports line that doesn't match expected comparison", {
  fake_output <- c("GroupA",
                   "unexpected garbage line")
  gn <- with_temp_golden(
    data.frame(group = "GroupA", comparison = "A minus B",
               diff = 1.0, se = 0.5, p = 0.05, cl_lo = -0.5, cl_hi = 2.5,
               stringsAsFactors = FALSE),
    "test_grouped_garbage")
  expect_error(
    expect_grouped_posthoc_match(fake_output, gn, tol = 0),
    "comparison not found under group"
  )
})

test_that("grouped posthoc skips unexpected lines to find successful match", {
  fake_output <- c("GroupA",
                   "random console warning",
                   "A minus B: 1.0 \u00b1 0.5, p=0.05, CL=[-0.5,2.5]")
  gn <- with_temp_golden(
    data.frame(group = "GroupA", comparison = "A minus B",
               diff = 1.0, se = 0.5, p = 0.05, cl_lo = -0.5, cl_hi = 2.5,
               stringsAsFactors = FALSE),
    "test_grouped_skip_garbage")
  
  expect_error(expect_grouped_posthoc_match(fake_output, gn, tol = 0), NA)
})

test_that("grouped posthoc sequentially matches duplicate repeating group headers correctly", {
  fake_output <- c("GroupA",
                   "A minus B: 1.0 \u00b1 0.5, p=0.05, CL=[-0.5,2.5]",
                   "GroupB",
                   "C minus D: 5.0 \u00b1 0.1, p=0.99, CL=[-0.1,0.1]",
                   "GroupA", # Identical header repeated later!
                   "E minus F: 9.9 \u00b1 0.9, p=0.50, CL=[1.0,2.0]")
  
  gn <- with_temp_golden(
    data.frame(group = c("GroupA", "GroupB", "GroupA"), 
               comparison = c("A minus B", "C minus D", "E minus F"),
               diff = c(1.0, 5.0, 9.9), se = c(0.5, 0.1, 0.9), p = c(0.05, 0.99, 0.50), 
               cl_lo = c(-0.5, -0.1, 1.0), cl_hi = c(2.5, 0.1, 2.0),
               stringsAsFactors = FALSE),
    "test_grouped_duplicate_headers")
  
  # The new cursor-based search explicitly forces it to find the SECOND "GroupA" chunk!
  expect_error(expect_grouped_posthoc_match(fake_output, gn, tol = 0), NA)
})

# --- expect_ordinal_posthoc_match ---

test_that("ordinal posthoc skips unexpected lines to find successful match", {
  fake_output <- c("GroupA",
                   "random console warning",
                   "A minus B: (2/3) RR=1.5, CL=[0.5,2.5] ... p=0.05")
  gn <- with_temp_golden(
    data.frame(group = "GroupA", comparison = "A minus B",
               RR = 1.5, lower = 0.5, upper = 2.5, p = 0.05,
               stringsAsFactors = FALSE),
    "test_ordinal_skip_garbage")
  
  expect_error(expect_ordinal_posthoc_match(fake_output, gn, tol = 1e-5), NA)
})

test_that("ordinal posthoc sequentially matches duplicate repeating group headers correctly", {
  fake_output <- c("GroupA",
                   "A minus B: (2/3) RR=1.5, CL=[0.5,2.5] ... p=0.05",
                   "GroupB",
                   "C minus D: (4/5) RR=5.0, CL=[-0.1,0.1] ... p=0.99",
                   "GroupA", # Identical header repeated later!
                   "E minus F: (9/10) RR=9.9, CL=[1.0,2.0] ... p=0.50")
  
  gn <- with_temp_golden(
    data.frame(group = c("GroupA", "GroupB", "GroupA"), 
               comparison = c("A minus B", "C minus D", "E minus F"),
               RR = c(1.5, 5.0, 9.9), lower = c(0.5, -0.1, 1.0), upper = c(2.5, 0.1, 2.0), p = c(0.05, 0.99, 0.50),
               stringsAsFactors = FALSE),
    "test_ordinal_duplicate_headers")
  
  expect_error(expect_ordinal_posthoc_match(fake_output, gn, tol = 1e-5), NA)
})

# --- expect_table_match failure cases ---

test_that("expect_table_match fails on missing row", {
  actual <- data.frame(stat = "A", x = "1", stringsAsFactors = FALSE)
  golden <- data.frame(stat = c("A", "B"), x = c("1", "2"),
                       stringsAsFactors = FALSE)
  expect_error(
    expect_table_match(actual, golden, label = "test"),
    "missing from actual"
  )
})

test_that("expect_table_match fails on value mismatch", {
  actual <- data.frame(stat = "A", x = "99", stringsAsFactors = FALSE)
  golden <- data.frame(stat = "A", x = "1", stringsAsFactors = FALSE)
  expect_error(
    expect_table_match(actual, golden, label = "test"),
    "expected 1, got 99"
  )
})

test_that("expect_table_match fails on extra rows", {
  actual <- data.frame(stat = c("A", "B"), x = c("1", "2"),
                       stringsAsFactors = FALSE)
  golden <- data.frame(stat = "A", x = "1", stringsAsFactors = FALSE)
  expect_error(
    expect_table_match(actual, golden, label = "test"),
    "Extra row"
  )
})

test_that("expect_table_match fails on missing column", {
  actual <- data.frame(stat = "A", y = "1", stringsAsFactors = FALSE)
  golden <- data.frame(stat = "A", x = "1", stringsAsFactors = FALSE)
  expect_error(
    expect_table_match(actual, golden, label = "test"),
    "column missing"
  )
})
