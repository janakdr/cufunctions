library(testthat)
library(cufunctions)


test_setup <- function() {
  data(coxdata, envir = environment())
  return(coxdata)
}

test_that("cucox matched golden", {
  coxdata <- test_setup()
  output <- capture.output(cucox(coxdata, "TimeToEvent", "Outcome", "C.Index + scale(LVEF) + scale(BNP) + gender", ftype="pdf"))
  
  # 1. Coefficients Table 1
  golden_coef1 <- load_golden("cucox_coxdata_coef1")
  actual_coef1 <- parse_coef_table(output, col_names = colnames(golden_coef1)[-1])
  expect_table_match(actual_coef1, golden_coef1, id_col = "term", label = "Coef 1", tol = 0.01)
  
  # 2. Model Selection Table
  modsel_lines <- extract_section_lines(output, "Model selection table", stop_prefix_list = list("Models ranked by"))
  expect_true(!is.null(modsel_lines))
  weight_idx <- grep("^ *weight[ \t]*$", modsel_lines)
  if (length(weight_idx) > 0) modsel_lines <- modsel_lines[1:(weight_idx[1]-1)]
  actual_modsel <- parse_fixed_width_table(modsel_lines[2], modsel_lines[3:length(modsel_lines)], id_col = "model")
  golden_modsel <- load_golden("cucox_coxdata_modsel")
  expect_table_match(actual_modsel, golden_modsel, id_col = "model", label = "Modsel", tol = 0.02)
  
  # 3. Schoenfeld Residuals
  schoenfeld_lines <- extract_section_lines(output, "Test of Proportional", stop_prefix_list = list("PH assumption"))
  expect_true(!is.null(schoenfeld_lines))
  actual_schoenfeld <- parse_fixed_width_table(schoenfeld_lines[2], schoenfeld_lines[3:length(schoenfeld_lines)], id_col = "term")
  golden_schoenfeld <- load_golden("cucox_coxdata_schoenfeld")
  expect_table_match(actual_schoenfeld, golden_schoenfeld, id_col = "term", label = "Schoenfeld", tol = 0.01)
  
  # 4. Coefficients Table 2
  coef2_start <- grep("^ *coef Hazard Ratio", output)[1]
  expect_true(!is.na(coef2_start))
  golden_coef2 <- load_golden("cucox_coxdata_coef2")
  actual_coef2 <- parse_coef_table(output[coef2_start:length(output)], col_names = colnames(golden_coef2)[-1])
  expect_table_match(actual_coef2, golden_coef2, id_col = "term", label = "Coef 2", tol = 0.01)
  
  # 5. Hazard Ratios Table
  hr_start <- grep("^ *Hazard Ratio HR", output)[1]
  hr_end <- grep("^Concordance=", output)[1]
  expect_true(!is.na(hr_start) && !is.na(hr_end))
  hr_lines <- output[hr_start:(hr_end-1)]
  hr_lines <- hr_lines[trimws(hr_lines) != ""]
  hr_col_names <- c("Hazard Ratio", "HR for non-event", "lower .95", "upper .95")
  actual_hr <- parse_fixed_width_table(hr_lines[1], hr_lines[2:length(hr_lines)], id_col = "term", col_names = hr_col_names)
  golden_hr <- load_golden("cucox_coxdata_hr")
  
  expect_contains(colnames(actual_hr), "HR for non-event")
  expect_table_match(actual_hr, golden_hr, id_col = "term", label = "Hazard Ratios", tol = 0.01)
})

test_that("cucox handles nscat == 0 safely without crashing", {
  coxdata <- test_setup()
  expect_error({
    # This formula contains only continuous numeric predictors which guarantees
    # the backward selection natively omits all non-numeric strata variables
    # forcing the ensuing plotlist builder to skip its logic and nscat == 0 natively!
    capture.output(cucox(coxdata, "TimeToEvent", "Outcome", "scale(LVEF) + scale(BNP)"))
  }, NA)
})
