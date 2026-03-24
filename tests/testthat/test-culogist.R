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
  actual_pred <- parse_fixed_width_table(pred_lines[3], pred_lines[4:length(pred_lines)], id_col = "predictor")
  golden_pred <- load_golden("culogist_Met_predictors")

  expect_table_match(actual_pred, golden_pred, id_col = "predictor", label = "Predictors", tol = 0.02)
  
  # 2. Coefficients 1
  coef1_lines <- extract_section_lines(output, "^Coefficients:")
  expect_true(!is.null(coef1_lines))
  labels1 <- c("(Intercept)", "HDL", "LN_TG", "BMI", "GLUC", "INS")
  actual_coef1 <- parse_fixed_width_table(coef1_lines[2], coef1_lines[3:(length(coef1_lines)-1)], 
                                          id_col = "term", row_labels = labels1)
  actual_coef1[[ncol(actual_coef1)]] <- gsub(" \\*\\*\\*| \\*\\*| \\*| \\.|", "", actual_coef1[[ncol(actual_coef1)]])
  golden_coef1 <- load_golden("culogist_Met_coef1")
  expect_table_match(actual_coef1, golden_coef1, id_col = "term", label = "Coef 1", tol = 0.02)
  
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
  expect_table_match(actual_modsel, golden_modsel, id_col = "model", label = "Modsel", tol = 0.02)
  
  # 4. Coefficients 2
  coef2_idx <- grep("^Coefficients:", output)
  expect_true(length(coef2_idx) >= 2)
  coef2_start <- coef2_idx[2]
  coef2_lines <- output[coef2_start:length(output)]
  end <- 2
  while (end <= length(coef2_lines) && coef2_lines[end] != "" && !grepl("^>", coef2_lines[end])) end <- end + 1
  coef2_lines <- coef2_lines[1:(end-1)]
  actual_coef2 <- parse_fixed_width_table(coef2_lines[2], coef2_lines[3:(length(coef2_lines)-1)], 
                                          id_col = "term", row_labels = c("(Intercept)", "BMI", "LN_TG"))
  actual_coef2[[ncol(actual_coef2)]] <- gsub(" \\*\\*\\*| \\*\\*| \\*| \\.|", "", actual_coef2[[ncol(actual_coef2)]])
  golden_coef2 <- load_golden("culogist_Met_coef2")
  expect_table_match(actual_coef2, golden_coef2, id_col = "term", label = "Coef 2", tol = 0.02)
  
  # 5. Odds Ratio Stats
  or_lines <- extract_section_lines(output, "^Odds Ratio stats")
  expect_true(!is.null(or_lines))
  actual_or <- parse_fixed_width_table(or_lines[2], or_lines[3:length(or_lines)], id_col = "term")
  golden_or <- load_golden("culogist_Met_oddsratio")
  actual_or$term <- gsub(" [0-9\\.]+$", "", actual_or$term)
  golden_or$term <- gsub(" [0-9\\.]+$", "", golden_or$term)
  expect_table_match(actual_or, golden_or, id_col = "term", label = "Odds Ratio", tol = 0.02)

})
