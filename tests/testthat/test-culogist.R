library(testthat)
library(cufunctions)

context("culogist")

test_setup <- function() {
  data(AJCN, envir = environment())
  Met <- AJCN
  return(Met)
}

test_that("culogist standard run matches golden", {
  Met <- test_setup()
  
  output <- capture.output({
    res <- culogist(Met, "MetSyn", "HDL+LN_TG+BMI+GLUC+INS")
  })
  
  # 1. Predictors Table
  pred_lines <- extract_section_lines(output, "Predictor summary vs MetSyn and p-values", stop_regex_list = list("^Shapiro-Wilk"))
  expect_true(!is.null(pred_lines))
  pred_labels <- c("N from HDL on:", "HDL-Mean\u00b1SD", "LN_TG-Mean\u00b1SD",
                    "BMI-Median(IQR)", "GLUC-Median(IQR)", "INS-Median(IQR)")
  pred_col_names <- c("No", "Yes", "1 vs 2")
  actual_pred <- parse_fixed_width_table(pred_lines[3], pred_lines[4:length(pred_lines)],
                                          id_col = "predictor", row_labels = pred_labels,
                                          col_names = pred_col_names)
  golden_pred <- load_golden("culogist_Met_predictors")

  expect_table_match(actual_pred, golden_pred, id_col = "predictor", label = "Predictors", tol = 0.02)
  
  # 2. Coefficients 1 — use parse_coef_table which handles col_names and star-stripping
  z_col_names <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  actual_coef1 <- parse_coef_table(output, col_names = z_col_names)
  golden_coef1 <- load_golden("culogist_Met_coef1")
  # Tolerance 0.03: glm() estimates vary slightly across R versions/BLAS
  expect_table_match(actual_coef1, golden_coef1, id_col = "term", label = "Coef 1", tol = 0.03)
  
  # 3. Model Selection Table
  modsel_lines <- extract_section_lines(output, "^Model selection table", stop_regex_list = list("^Models ranked by"))
  expect_true(!is.null(modsel_lines))
  
  ids_modsel <- sapply(modsel_lines[3:length(modsel_lines)], function(l) {
    tokens <- strsplit(trimws(l), " +")[[1]]; if (length(tokens) > 0) tokens[1] else NA
  })
  ids_modsel <- ids_modsel[!is.na(ids_modsel)]
  actual_modsel <- parse_fixed_width_table(modsel_lines[2], modsel_lines[3:length(modsel_lines)], 
                                          id_col = "model", row_labels = ids_modsel)
  golden_modsel <- load_golden("culogist_Met_modsel")
  # Model weights near zero (e.g. 0.001) have huge relative diffs across R versions
  expect_table_match(actual_modsel, golden_modsel, id_col = "model", label = "Modsel", tol = 1.0)
  
  # 4. Coefficients 2 — parse from second "Coefficients:" header onward
  coef2_idx <- grep("^Coefficients:", output)
  expect_true(length(coef2_idx) >= 2)
  coef2_output <- output[coef2_idx[2]:length(output)]
  actual_coef2 <- parse_coef_table(coef2_output, col_names = z_col_names)
  golden_coef2 <- load_golden("culogist_Met_coef2")
  expect_table_match(actual_coef2, golden_coef2, id_col = "term", label = "Coef 2", tol = 0.03)
  
  # 5. Odds Ratio Stats — header has multi-word columns
  or_lines <- extract_section_lines(output, "^Odds Ratio stats")
  expect_true(!is.null(or_lines))
  or_col_names <- c("coeff", "Std Err", "Odds Ratio", "lower .95", "upper .95")
  actual_or <- parse_fixed_width_table(or_lines[2], or_lines[3:length(or_lines)],
                                        id_col = "term", col_names = or_col_names)
  golden_or <- load_golden("culogist_Met_oddsratio")
  expect_table_match(actual_or, golden_or, id_col = "term", label = "Odds Ratio", tol = 0.03)

})
