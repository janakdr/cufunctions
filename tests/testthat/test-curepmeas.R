library(testthat)
library(cufunctions)


test_setup <- function() {
  data(delta, envir = environment())
  return(delta)
}

test_that("curepmeas Case 1: single factor", {
  delta <- test_setup()
  
  output <- capture.output({
    res <- curepmeas(delta, "like", "Diet", 
                     ordinal = c("worst", "bad", "ok", "good", "best"))
  })
  
  # 1. Distribution Table
  actual_dist <- parse_curepmeas_table(output, "^like *$")
  golden_dist <- load_golden("curepmeas_like_dist")
  expect_table_match(actual_dist, golden_dist, id_col = "term", label = "Case 1 dist")
  
  # 2. Delta Count Table
  actual_del <- parse_curepmeas_table(output, "^like Δ count *$")
  golden_del <- load_golden("curepmeas_like_del")
  expect_table_match(actual_del, golden_del, id_col = "term", label = "Case 1 del")
  
  # 3. Up/Down Table
  actual_updn <- parse_curepmeas_table(output, "^like updn *$")
  golden_updn <- load_golden("curepmeas_like_updn")
  expect_table_match(actual_updn, golden_updn, id_col = "term", label = "Case 1 updn")
  
  # 4. Sign Test Table
  golden_sign <- load_golden("curepmeas_like_sign_test")
  sign_lines <- extract_section_lines(output, "^up:dn",
                                       stop_prefix_list = list("p-value NA"))
  sign_row_labels <- c("|Δ| > 3", "", "|Δ| > 2", "", "|Δ| > 1", "", "|Δ| > 0", "")
  actual_sign <- parse_fixed_width_table(sign_lines[1], sign_lines[-1],
                                          id_col = "threshold",
                                          row_labels = sign_row_labels)
  expect_table_match(actual_sign, golden_sign, id_col = "threshold", label = "Case 1 sign")
})

test_that("curepmeas Case 2: two factors", {
  delta <- test_setup()
  
  output <- capture.output({
    res <- curepmeas(delta, "like", "Diet", "sex", 
                     ordinal = c("worst", "bad", "ok", "good", "best"))
  })
  
  # 1. Distribution Table
  actual_dist <- parse_curepmeas_table(output, "^like *$")
  golden_dist <- load_golden("curepmeas_like_sex_dist")
  expect_table_match(actual_dist, golden_dist, id_col = "term", label = "Case 2 dist")
  
  # 2. Delta Count Table
  actual_del <- parse_curepmeas_table(output, "^like Δ count *$")
  golden_del <- load_golden("curepmeas_like_sex_del")
  expect_table_match(actual_del, golden_del, id_col = "term", label = "Case 2 del")
  
  # 3. Up/Down Table
  actual_updn <- parse_curepmeas_table(output, "^like updn *$")
  golden_updn <- load_golden("curepmeas_like_sex_updn")
  expect_table_match(actual_updn, golden_updn, id_col = "term", label = "Case 2 updn")
  
  # 4. Sign Test Table
  golden_sign <- load_golden("curepmeas_like_sex_sign_test")
  sign_lines <- extract_section_lines(output, "^up:dn",
                                       stop_prefix_list = list("p-value NA"))
  sign_row_labels <- c("|Δ| > 3", "", "|Δ| > 2", "", "|Δ| > 1", "", "|Δ| > 0", "")
  actual_sign <- parse_fixed_width_table(sign_lines[1], sign_lines[-1],
                                          id_col = "threshold",
                                          row_labels = sign_row_labels)
  expect_table_match(actual_sign, golden_sign, id_col = "threshold", label = "Case 2 sign")
})

