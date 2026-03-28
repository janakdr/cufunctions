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

  # Log-rank chi-sq test
  expect_format_match(output, "Chisq= %n  on 1 degrees of freedom, p= %n", c(8.9, 0.003), tol = 0.02)

  # Hazard Ratio
  expect_format_match(
    output,
    "Hazard Ratio Prednisolone:Control %n (95% CI %n-%n)",
    c(0.456, 0.264, 0.786),
    tol = 0.02
  )
})

test_that("cukm test 2 matches golden (Treatment, kmtype=ci)", {
  hepc <- test_setup()
  # Output is identical to test 1
  output <- capture.output(with(hepc, cukm(time, status, Treatment, kmtype="ci", ftype="pdf")))
  
  actual_df <- extract_km_table(output, "Control vs. Prednisolone")
  golden_df <- load_golden("cukm_hepc_test1")
  
  expect_table_match(actual_df, golden_df, id_col = "Treatment", label = "Test 2", tol = 0.01)

  # Log-rank chi-sq test (same as test 1)
  expect_format_match(output, "Chisq= %n  on 1 degrees of freedom, p= %n", c(8.9, 0.003), tol = 0.02)

  # Hazard Ratio (same as test 1)
  expect_format_match(
    output,
    "Hazard Ratio Prednisolone:Control %n (95% CI %n-%n)",
    c(0.456, 0.264, 0.786),
    tol = 0.02
  )
})

test_that("cukm test 3 matches golden (Treat3 multi-category)", {
  hepc <- test_setup()
  output <- capture.output(with(hepc, cukm(time, status, Treat3, ftype="pdf")))
  

  actual_df1 <- extract_km_table(output, "Control vs. Drugtwo")
  golden_df1 <- load_golden("cukm_hepc_test3_1")
  expect_table_match(actual_df1, golden_df1, id_col = "Treatment", label = "Test 3 - Control vs Drugtwo", tol = 0.01)

  # Control vs Drugtwo: log-rank and HR
  ctrl_drug_section <- output[grep("Control vs. Drugtwo", output)[1]:length(output)]
  expect_format_match(
    ctrl_drug_section,
    "Chisq= %n  on 1 degrees of freedom, p= %n",
    c(1.6, 0.2),
    tol = 0.02
  )
  expect_format_match(
    ctrl_drug_section,
    "Hazard Ratio Drugtwo:Control %n (95% CI %n-%n)",
    c(0.68, 0.351, 1.316),
    tol = 0.02
  )


  actual_df2 <- extract_km_table(output, "Control vs. Prednisolone")
  golden_df2 <- load_golden("cukm_hepc_test3_2")
  expect_table_match(actual_df2, golden_df2, id_col = "Treatment", label = "Test 3 - Control vs Prednisolone", tol = 0.01)

  # Control vs Prednisolone: log-rank and HR
  ctrl_pred_section <- output[grep("Control vs. Prednisolone", output)[1]:length(output)]
  expect_format_match(
    ctrl_pred_section,
    "Chisq= %n  on 1 degrees of freedom, p= %n",
    c(4.4, 0.04),
    tol = 0.02
  )
  expect_format_match(
    ctrl_pred_section,
    "Hazard Ratio Prednisolone:Control %n (95% CI %n-%n)",
    c(0.456, 0.211, 0.985),
    tol = 0.02
  )


  actual_df3 <- extract_km_table(output, "Drugtwo vs. Prednisolone")
  golden_df3 <- load_golden("cukm_hepc_test3_3")
  expect_table_match(actual_df3, golden_df3, id_col = "Treatment", label = "Test 3 - Drugtwo vs Prednisolone", tol = 0.01)

  # Drugtwo vs Prednisolone: log-rank and HR
  drug_pred_section <- output[grep("Drugtwo vs. Prednisolone", output)[1]:length(output)]
  expect_format_match(
    drug_pred_section,
    "Chisq= %n  on 1 degrees of freedom, p= %n",
    c(1.3, 0.3),
    tol = 0.02
  )
  expect_format_match(
    drug_pred_section,
    "Hazard Ratio Prednisolone:Drugtwo %n (95% CI %n-%n)",
    c(0.672, 0.349, 1.295),
    tol = 0.02
  )


  overall_start <- grep("Overall", output)[1]
  actual_df4 <- extract_km_table(output[overall_start:length(output)], "Overall")
  golden_df4 <- load_golden("cukm_hepc_test3_overall")
  expect_table_match(actual_df4, golden_df4, id_col = "Treatment", label = "Test 3 - Overall", tol = 0.01)

  # Overall: log-rank (2 df)
  overall_section <- output[overall_start:length(output)]
  expect_format_match(
    overall_section,
    "Chisq= %n  on 2 degrees of freedom, p= %n",
    c(4.4, 0.1),
    tol = 0.02
  )
})
