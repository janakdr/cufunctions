# Tests for the option-defaults hint system (zzz.R)
#
# These tests verify cuf_apply_defaults() and hint_option() in isolation,
# using a small wrapper function rather than the real analysis functions.

# A minimal function to test the introspection machinery
test_fn <- function(a, b = 10, cc = "hello", ...) {
  cuf_apply_defaults(match.call(), environment())
  list(a = a, b = b, cc = cc)
}

# --- hint_option ---

test_that("hint_option fires at every multiple of threshold", {
  .cuf_env$param_counts <- list()
  # Calls 1 and 2: no message
  expect_silent(hint_option("ebars", 4, threshold = 3L))
  expect_silent(hint_option("ebars", 4, threshold = 3L))
  # Call 3: message
  expect_message(hint_option("ebars", 4, threshold = 3L), "Tip:")
  # Calls 4 and 5: no message
  expect_silent(hint_option("ebars", 4, threshold = 3L))
  expect_silent(hint_option("ebars", 4, threshold = 3L))
  # Call 6: message again
  expect_message(hint_option("ebars", 4, threshold = 3L), "Tip:")
})

test_that("hint_option message contains the option name and value", {
  .cuf_env$param_counts <- list()
  expect_message(
    hint_option("ebars", 4, threshold = 1L),
    "cufunctions.ebars"
  )
})

test_that("hint_option message suggests suppression with cufunctions.hints_defaults", {
  .cuf_env$param_counts <- list()
  expect_message(
    hint_option("ebars", 4, threshold = 1L),
    "cufunctions.hints_defaults"
  )
})

test_that("hint_option is silent when cufunctions.hints_defaults is FALSE", {
  .cuf_env$param_counts <- list()
  withr::local_options(cufunctions.hints_defaults = FALSE)
  expect_silent(hint_option("ebars", 4, threshold = 1L))
  expect_silent(hint_option("ebars", 4, threshold = 1L))
  expect_silent(hint_option("ebars", 4, threshold = 1L))
})

test_that("hint_option is silent when the specific option is already set", {
  .cuf_env$param_counts <- list()
  withr::local_options(cufunctions.ebars = 4)
  expect_silent(hint_option("ebars", 4, threshold = 1L))
})

test_that("hint_option tracks different values separately", {
  .cuf_env$param_counts <- list()
  expect_silent(hint_option("ebars", 1, threshold = 2L))
  expect_silent(hint_option("ebars", 4, threshold = 2L))
  # 2nd call with value=1 fires at threshold=2
  expect_message(hint_option("ebars", 1, threshold = 2L), "Tip:")
  # 2nd call with value=4 also fires at threshold=2
  expect_message(hint_option("ebars", 4, threshold = 2L), "Tip:")
})

test_that("hint_option skips long (non-scalar) values", {
  .cuf_env$param_counts <- list()
  long_val <- paste(rep("x", 100), collapse = "")
  expect_silent(hint_option("group1", long_val, threshold = 1L))
})

# --- cuf_apply_defaults (via test_fn) ---

test_that("getOption override works when user does not pass the arg", {
  .cuf_env$param_counts <- list()
  withr::local_options(cufunctions.b = 42)
  result <- test_fn(a = 1)
  expect_equal(result$b, 42)
})

test_that("explicit arg takes precedence over getOption override", {
  .cuf_env$param_counts <- list()
  withr::local_options(cufunctions.b = 42)
  result <- test_fn(a = 1, b = 99)
  expect_equal(result$b, 99)
})

test_that("required params (no default) are skipped", {
  .cuf_env$param_counts <- list()
  # Setting an option for 'a' (which has no default) should NOT override
  withr::local_options(cufunctions.a = 999)
  result <- test_fn(a = 1)
  expect_equal(result$a, 1)
})

test_that("multiple params can be overridden via options", {
  .cuf_env$param_counts <- list()
  withr::local_options(cufunctions.b = 42, cufunctions.cc = "world")
  result <- test_fn(a = 1)
  expect_equal(result$b, 42)
  expect_equal(result$cc, "world")
})

test_that("cuf_apply_defaults triggers hint_option for explicit args", {
  .cuf_env$param_counts <- list()
  withr::local_options(cufunctions.hints_defaults = TRUE)
  # Call test_fn with explicit b=5 three times
  expect_silent(test_fn(a = 1, b = 5))
  expect_silent(test_fn(a = 1, b = 5))
  expect_message(test_fn(a = 1, b = 5), "Tip:")
})
