library(testthat)
library(cufunctions)


test_setup <- function() {
  data(hepc, envir = environment())
  return(hepc)
}

# helper to dynamically find the start of the 'N Observed' table block
extract_km_table <- function(out_lines, start_pattern) {
  start_idx <- grep(start_pattern, out_lines)[1]
  if (is.na(start_idx)) stop(paste("Pattern not found:", start_pattern))
  header_idx <- start_idx
  while(!grepl("N Observed", out_lines[header_idx])) header_idx <- header_idx + 1
  
  end_idx <- header_idx + 1
  while(trimws(out_lines[end_idx]) != "") end_idx <- end_idx + 1
  
  header <- out_lines[header_idx]
  data <- out_lines[(header_idx+1):(end_idx-1)]
  
  return(parse_fixed_width_table(header, data, id_col = "Treatment"))
}

test_that("cukm test 1 matches golden (Treatment)", {
  hepc <- test_setup()
  output <- capture.output(with(hepc, cukm(time, status, Treatment, ftype="pdf")))
  
  actual_df <- extract_km_table(output, "Control vs. Prednisolone")
  golden_df <- load_golden("cukm_hepc_test1")
  
  expect_table_match(actual_df, golden_df, id_col = "Treatment", label = "Test 1", tol = 0.01)
})

test_that("cukm test 2 matches golden (Treatment, kmtype=ci)", {
  hepc <- test_setup()
  # This is functionally identical in output to test 1 as shown in the docx
  output <- capture.output(with(hepc, cukm(time, status, Treatment, kmtype="ci", ftype="pdf")))
  
  actual_df <- extract_km_table(output, "Control vs. Prednisolone")
  golden_df <- load_golden("cukm_hepc_test1")
  
  expect_table_match(actual_df, golden_df, id_col = "Treatment", label = "Test 2", tol = 0.01)
})

test_that("cukm test 3 matches golden (Treat3 multi-category)", {
  hepc <- test_setup()
  output <- capture.output(with(hepc, cukm(time, status, Treat3, ftype="pdf")))
  
  # 1. Control vs Drugtwo
  actual_df1 <- extract_km_table(output, "Control vs. Drugtwo")
  golden_df1 <- load_golden("cukm_hepc_test3_1")
  expect_table_match(actual_df1, golden_df1, id_col = "Treatment", label = "Test 3 - Control vs Drugtwo", tol = 0.01)
  
  # 2. Overall
  overall_start <- grep("Overall", output)[1]
  actual_df4 <- extract_km_table(output[overall_start:length(output)], "Overall")
  golden_df4 <- load_golden("cukm_hepc_test3_overall")
  expect_table_match(actual_df4, golden_df4, id_col = "Treatment", label = "Test 3 - Overall", tol = 0.01)
})
