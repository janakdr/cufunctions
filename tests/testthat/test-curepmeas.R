library(testthat)
library(cufunctions)


test_setup <- function() {
  data(delta, envir = environment())
  return(delta)
}

test_that("curepmeas Case 1: single factor", {
  delta <- test_setup()
  
  output <- wide_capture({
    res <- curepmeas(delta, "like", "Diet", 
                     ordinal = c("worst", "bad", "ok", "good", "best"))
  })
  
  # 1. Distribution Table
  dist_lines <- extract_section_lines(output, "^like *$",
                                       stop_prefix_list = list("[1]", "p-value NA"))
  actual_dist <- parse_fixed_width_table(dist_lines[2], dist_lines[-(1:2)], id_col = "term")
  golden_dist <- load_golden("curepmeas_like_dist")
  expect_table_match(actual_dist, golden_dist, id_col = "term", label = "Case 1 dist")
  
  # 2. Delta Count Table
  del_lines <- extract_section_lines(output, "^like Δ count *$",
                                      stop_prefix_list = list("[1]", "p-value NA"))
  actual_del <- parse_fixed_width_table(del_lines[2], del_lines[-(1:2)], id_col = "term")
  golden_del <- load_golden("curepmeas_like_del")
  expect_table_match(actual_del, golden_del, id_col = "term", label = "Case 1 del")
  
  # 3. Up/Down Table
  updn_lines <- extract_section_lines(output, "^like updn *$",
                                       stop_prefix_list = list("[1]", "p-value NA"))
  actual_updn <- parse_fixed_width_table(updn_lines[2], updn_lines[-(1:2)], id_col = "term")
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
  
  output <- wide_capture({
    res <- curepmeas(delta, "like", "Diet", "sex", 
                     ordinal = c("worst", "bad", "ok", "good", "best"))
  })
  
  # 1. Distribution Table
  dist_lines <- extract_section_lines(output, "^like *$",
                                       stop_prefix_list = list("[1]", "p-value NA"))
  actual_dist <- parse_fixed_width_table(dist_lines[2], dist_lines[-(1:2)], id_col = "term")
  golden_dist <- load_golden("curepmeas_like_sex_dist")
  expect_table_match(actual_dist, golden_dist, id_col = "term", label = "Case 2 dist")
  
  # 2. Delta Count Table
  del_lines <- extract_section_lines(output, "^like Δ count *$",
                                      stop_prefix_list = list("[1]", "p-value NA"))
  actual_del <- parse_fixed_width_table(del_lines[2], del_lines[-(1:2)], id_col = "term")
  golden_del <- load_golden("curepmeas_like_sex_del")
  expect_table_match(actual_del, golden_del, id_col = "term", label = "Case 2 del")
  
  # 3. Up/Down Table
  updn_lines <- extract_section_lines(output, "^like updn *$",
                                       stop_prefix_list = list("[1]", "p-value NA"))
  actual_updn <- parse_fixed_width_table(updn_lines[2], updn_lines[-(1:2)], id_col = "term")
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

# --- curepmeas(delta, "TG", "Diet") — continuous, single factor ---

test_that("curepmeas Case 3: continuous TG, single factor", {
  delta <- test_setup()

  output <- wide_capture({
    res <- curepmeas(delta, "TG", "Diet", ebars=1)
    #  TODO (Janak): get rid of ebars=1
  })

  # Posthoc comparisons
  expect_posthoc_match(output, "curepmeas_TG_Diet_posthoc", tol = 0.02)

  # P-value matrix (header is "Summary of p-values", not "Pairwise comparisons")
  pmat_section <- extract_section_lines(output, "TG compared across repeats of Diet")
  actual_pw <- parse_matrix_from_header(pmat_section, 2)
  golden_pw <- load_golden("curepmeas_TG_Diet_pmatrix")
  expect_table_match(actual_pw, golden_pw, label = "curepmeas TG Diet pmatrix", tol = 0.02)
})

# --- curepmeas(delta, "TG", "Diet", "sex") — continuous, two factors ---

test_that("curepmeas Case 4: continuous TG, two factors", {
  delta <- test_setup()

  output <- wide_capture({
    res <- curepmeas(delta, "TG", "Diet", "sex", ebars=1)
  })

  # Grouped posthoc comparisons (tol=0.5 needed because near-zero diffs like
  # 0.0211 have large relative differences across R versions)
  expect_grouped_posthoc_match(output, "curepmeas_TG_Diet_sex_posthoc", tol = 0.5)

  # P-value matrix — find header line in full output and parse from there
  pmat_idx <- grep("TG compared across Diet&sex groups", output, fixed = TRUE)[1]
  header_idx <- pmat_idx + 1
  actual_pw <- parse_matrix_from_header(output, header_idx)
  golden_pw <- load_golden("curepmeas_TG_Diet_sex_pmatrix")
  expect_table_match(actual_pw, golden_pw, label = "curepmeas TG Diet sex pmatrix", tol = 0.02)
})

# --- curepmeas(delta, "TG", "Diet", "sex", cov="age+age*Diet") — with covariate ---

test_that("curepmeas Case 5: continuous TG, two factors with covariate", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("MuMIn")
  # MuMIn::dredge needs lme.formula on the search path
  require(nlme, quietly = TRUE)
  delta <- test_setup()

  output <- wide_capture({
    res <- curepmeas(delta, "TG", "Diet", "sex", cov = "age+age*Diet", ebars=1)
  })

  # Grouped posthoc is the same as without covariate (cov doesn't change posthoc)
  expect_grouped_posthoc_match(output, "curepmeas_TG_Diet_sex_posthoc", tol = 0.5)

  # Verify model selection output is present
  expect_match(paste(output, collapse = "\n"), "Model selection table", fixed = TRUE)
})
