library(testthat)
library(cufunctions)


test_setup <- function() {
  data(AJCN, envir = environment())
  Met <- AJCN
  return(Met)
}

test_that("culinreg standard run matches golden", {
  Met <- test_setup()
  
  output <- capture.output({
    res <- culinreg(Met, "LDL", "TG+TC+HDL+LN_TG+BMI", ftype="pdf")
  })
  

  corr_lines <- extract_section_lines(output, "Pairwise correlations and P-values", stop_prefix_list = list("n="))
  expect_true(!is.null(corr_lines))
  actual_corr <- parse_fixed_width_table(corr_lines[2], corr_lines[3:length(corr_lines)], id_col = "Var")
  golden_corr <- load_golden("culinreg_Met_correlations")
  expect_table_match(actual_corr, golden_corr, id_col = "Var", label = "Correlations", tol = 0.01)
  

  p_idx <- grep("^P$", output)
  if (length(p_idx) == 0) p_idx <- grep("^P *$", output)
  expect_true(length(p_idx) > 0)
  stop_idx <- grep("^Call:", output)[1]
  p_lines <- output[(p_idx+1):(stop_idx-1)]
  p_lines <- p_lines[trimws(p_lines) != ""]
  actual_p <- parse_fixed_width_table(p_lines[1], p_lines[2:length(p_lines)], id_col = "Var")
  golden_p <- load_golden("culinreg_Met_pvalues")
  expect_table_match(actual_p, golden_p, id_col = "Var", label = "P-values", tol = 0.01)
  

  golden_coef1 <- load_golden("culinreg_Met_coef1")
  actual_coef1 <- parse_coef_table(output, col_names = colnames(golden_coef1)[-1])
  expect_contains(colnames(actual_coef1), c("Std. Error", "t value"))
  expect_table_match(actual_coef1, golden_coef1, id_col = "term", label = "Coef 1", tol = 0.01)
  

  modsel_lines <- extract_section_lines(output, "Model selection table", stop_prefix_list = list("Models ranked by"))
  expect_true(!is.null(modsel_lines))
  weight_idx <- grep("weight", modsel_lines)
  if (length(weight_idx) > 0) modsel_lines <- modsel_lines[1:(weight_idx[1]-1)]
  
  actual_modsel <- parse_fixed_width_table(modsel_lines[2], modsel_lines[3:length(modsel_lines)], id_col = "model")
  golden_modsel <- load_golden("culinreg_Met_modsel")
  expect_table_match(actual_modsel, golden_modsel, id_col = "model", label = "Modsel", tol = 0.02)
  

  coef2_start <- grep("^Coefficients:", output)[2]
  expect_true(length(coef2_start) > 0)
  coef2_output <- output[coef2_start:length(output)]
  golden_coef2 <- load_golden("culinreg_Met_coef2")
  actual_coef2 <- parse_coef_table(coef2_output, col_names = colnames(golden_coef2)[-1])
  expect_table_match(actual_coef2, golden_coef2, id_col = "term", label = "Coef 2", tol = 0.01)

  # Spot-check LR tests of top model
  expect_format_match(
    output,
    "vs # 27 : LR, degf, χ² p-value %n %n %n",
    c(41.7, 1, 1.06e-10),
    tol = 0.1
  )
  expect_format_match(
    output,
    "vs # 10 : LR, degf, χ² p-value %n %n %n",
    c(315, 2, 0),
    tol = 0.02
  )

  # Model fit stats — full model
  full_section <- output[1:grep("Model selection table", output)[1]]
  expect_format_match(
    full_section,
    "Residual standard error: %n on %n degrees of freedom",
    c(1.805, 79),
    tol = 0.02
  )
  expect_format_match(
    full_section,
    "Multiple R-squared:  %n",
    c(0.9949),
    tol = 0.02
  )

  # Model fit stats — reduced model
  reduced_section <- output[grep("Model selection table", output)[1]:length(output)]
  expect_format_match(
    reduced_section,
    "Residual standard error: %n on %n degrees of freedom",
    c(1.794, 80),
    tol = 0.02
  )
  expect_format_match(
    reduced_section,
    "F-statistic:  %n on %n and %n DF",
    c(3893, 4, 80),
    tol = 0.1
  )
})
