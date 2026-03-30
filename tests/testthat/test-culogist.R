library(testthat)
library(cufunctions)


test_setup <- function() {
  data(Met, envir = environment())
  return(Met)
}

test_that("culogist standard run matches golden", {
  Met <- test_setup()
  
  output <- capture.output({
    res <- culogist(Met, "MetSyn", "HDL+LN_TG+BMI+GLUC+INS")
  })
  

  pred_lines <- extract_section_lines(output, "Predictor summary vs MetSyn and p-values", stop_prefix_list = list("Shapiro-Wilk"))
  expect_true(!is.null(pred_lines))
  golden_pred <- load_golden("culogist_Met_predictors")
  pred_labels <- golden_pred$predictor
  pred_col_names <- colnames(golden_pred)[-1]
  actual_pred <- parse_fixed_width_table(pred_lines[3], pred_lines[4:length(pred_lines)],
                                          id_col = "predictor", row_labels = pred_labels,
                                          col_names = pred_col_names)

  expect_contains(colnames(actual_pred), "1 vs 2")
  expect_table_match(actual_pred, golden_pred, id_col = "predictor", label = "Predictors", tol = 0.02)
  
  # Coefficients 1 — use parse_coef_table which handles col_names and star-stripping
  golden_coef1 <- load_golden("culogist_Met_coef1")
  z_col_names <- colnames(golden_coef1)[-1]
  actual_coef1 <- parse_coef_table(output, col_names = z_col_names)
  expect_contains(colnames(actual_coef1), c("Std. Error", "z value"))
  # Tolerance 0.03: glm() estimates vary slightly across R versions/BLAS
  expect_table_match(actual_coef1, golden_coef1, id_col = "term", label = "Coef 1", tol = 0.03)
  

  modsel_lines <- extract_section_lines(output, "Model selection table", stop_prefix_list = list("Models ranked by"))
  expect_true(!is.null(modsel_lines))
  
  # Line 1 is the table title ("Model selection table"), Line 2 contains the column headers.
  # Data rows begin at Line 3.
  actual_modsel <- parse_fixed_width_table(modsel_lines[2], modsel_lines[3:length(modsel_lines)], 
                                          id_col = "model")
  golden_modsel <- load_golden("culogist_Met_modsel")
  expect_table_match(actual_modsel, golden_modsel, id_col = "model", label = "Modsel", tol = 0.03)
  
  # Coefficients 2 — parse from second "Coefficients:" header onward
  coef2_idx <- grep("^Coefficients:", output)
  expect_true(length(coef2_idx) >= 2)
  coef2_output <- output[coef2_idx[2]:length(output)]
  actual_coef2 <- parse_coef_table(coef2_output, col_names = z_col_names)
  golden_coef2 <- load_golden("culogist_Met_coef2")
  expect_table_match(actual_coef2, golden_coef2, id_col = "term", label = "Coef 2", tol = 0.03)
  
  # Odds Ratio Stats — header has multi-word columns
  or_lines <- extract_section_lines(output, "Odds Ratio stats")
  expect_true(!is.null(or_lines))
  golden_or <- load_golden("culogist_Met_oddsratio")
  or_col_names <- colnames(golden_or)[-1]
  actual_or <- parse_fixed_width_table(or_lines[2], or_lines[3:length(or_lines)],
                                        id_col = "term", col_names = or_col_names)
  expect_table_match(actual_or, golden_or, id_col = "term", label = "Odds Ratio", tol = 0.03)

  # Null/Residual deviance + AIC — full model
  expect_format_match(
    output,
    "Null deviance: %n  on %n  degrees of freedom",
    c(92.751, 84),
    tol = 0.02
  )
  # Restrict to full model section (before model selection)
  full_model_section <- output[1:grep("Model selection table", output)[1]]
  expect_format_match(
    full_model_section,
    "Residual deviance: %n  on %n  degrees of freedom",
    c(55.207, 79),
    tol = 0.5
  )
  expect_format_match(full_model_section, "AIC: %n", c(67.207), tol = 0.5)

  # Likelihood Ratio test
  expect_format_match(
    output,
    "vs 17: LR, degf, χ² p-value %n, %n, %n",
    c(12.3, 1, 4.43e-04),
    tol = 0.1
  )

  # Null/Residual deviance + AIC — reduced model
  reduced_model_section <- output[grep("Model selection table", output)[1]:length(output)]
  expect_format_match(
    reduced_model_section,
    "Residual deviance: %n  on %n  degrees of freedom",
    c(58.413, 82),
    tol = 0.5
  )
  expect_format_match(reduced_model_section, "AIC: %n", c(64.413), tol = 0.5)

  # 2×2 Classification Table
  expect_format_match(
    output,
    "Accuracy %n%; Sensitivity %n%; Specificity %n%",
    c(87.1, 60, 95.4),
    tol = 0.02
  )
  expect_format_match(
    output,
    "Positive Predictive Value %n%; Negative Predictive Value %n%",
    c(80, 88.6),
    tol = 0.02
  )


  expect_format_match(output, "AUC =  %n", c(0.878), tol = 0.02)

})
