source("tests/testthat/helper-golden.R")

# Create golden directory if not exists
dir.create("tests/testthat/golden", showWarnings = FALSE, recursive = TRUE)

# Read the docx.txt file
lines <- readLines("cufuncs-tests.docx.txt")

# Find culogist call
idx <- grep("> culogist\\(Met, \\\"MetSyn\\\", \\\"HDL\\+LN_TG\\+BMI\\+GLUC\\+INS\\\"\\)", lines)
if (length(idx) == 0) {
  stop("Could not find culogist call")
}
start_idx <- idx[1]
cat("Found culogist at line", start_idx, "\n")

# --- Sub-table Parsers ---

# Helper to find and extract lines for a section
extract_section_lines <- function(header_regex, start_idx, stop_regex_list = list("^$")) {
  idx <- grep(header_regex, lines[start_idx:length(lines)])
  if (length(idx) == 0) return(NULL)
  table_start <- start_idx + idx[1] - 1
  
  end <- table_start + 1
  while (end <= length(lines)) {
    line <- lines[end]
    matched_stop <- FALSE
    for (stop_regex in stop_regex_list) {
      if (grepl(stop_regex, line)) {
        matched_stop <- TRUE
        break
      }
    }
    if (matched_stop || line == "" || grepl("^>", line)) break
    end <- end + 1
  }
  table_lines <- lines[table_start:(end-1)]
  return(table_lines)
}

cat("Processing culogist tables...\n")

# 1. Predictor Summary Table
# Title row is "Predictor summary vs MetSyn and p-values"
# Header row is "                              No             Yes    1 vs 2"
# Data starts at "N from HDL on:"
# Ends at "Shapiro-Wilk" note
pred_lines <- extract_section_lines("Predictor summary vs MetSyn and p-values", start_idx, 
                                    stop_regex_list = list("^Shapiro-Wilk"))
if (!is.null(pred_lines)) {
  cat("Extracted predictor summary lines:\n")
  print(pred_lines)
  # Header index is 3 (Title is 1, sub-title is 2)
  # Actually:
  # [1] "Predictor summary vs MetSyn and p-values"
  # [2] "Entire dataset dsnodep across dsnomiss[, 1] levels"
  # [3] "                              No             Yes    1 vs 2"
  # so Header is [3]
  df_pred <- parse_fixed_width_table(pred_lines[3], pred_lines[4:length(pred_lines)], id_col = "predictor")
  write.csv(df_pred, "tests/testthat/golden/culogist_Met_predictors.csv", row.names = FALSE, na="")
  cat("Saved culogist_Met_predictors\n")
}

# 2. Coefficients 1
coef1_lines <- extract_section_lines("^Coefficients:", start_idx)
if (!is.null(coef1_lines)) {
  cat("Extracted coef1 lines:\n")
  print(coef1_lines)
  # Pass row labels to avoid parsing errors with single space
  labels1 <- c("(Intercept)", "HDL", "LN_TG", "BMI", "GLUC", "INS")
  df_coef1 <- parse_fixed_width_table(coef1_lines[2], coef1_lines[3:(length(coef1_lines)-1)], 
                                      id_col = "term", row_labels = labels1)
  df_coef1[[ncol(df_coef1)]] <- gsub(" \\*\\*\\*| \\*\\*| \\*| \\.|", "", df_coef1[[ncol(df_coef1)]])
  write.csv(df_coef1, "tests/testthat/golden/culogist_Met_coef1.csv", row.names = FALSE, na="")
  cat("Saved culogist_Met_coef1\n")
}

# 3. Model Selection Table
modsel_lines <- extract_section_lines("^Model selection table", start_idx, stop_regex_list = list("^Models ranked by"))
if (!is.null(modsel_lines)) {
  cat("Extracted modsel lines:\n")
  print(modsel_lines)
  
  # Use flexible regex for delta weight
  delta_weight_idx <- grep("^ *delta weight", modsel_lines)
  if (length(delta_weight_idx) > 0) {
    sub1_lines <- modsel_lines[2:(delta_weight_idx - 1)]
    sub2_lines <- modsel_lines[delta_weight_idx:length(modsel_lines)]
    
    # Parse Sub 1: Coefficients and stats
    # Sub 1 has unlabeled first column (Model ID)
    # Row labels are the first numbers in each line
    ids1 <- sapply(sub1_lines[-1], function(l) {
      tokens <- strsplit(trimws(l), " +")[[1]]
      if (length(tokens) > 0) return(tokens[1]) else return(NA)
    })
    ids1 <- ids1[!is.na(ids1)]
    
    df1 <- parse_fixed_width_table(sub1_lines[1], sub1_lines[-1], id_col = "model", row_labels = ids1)
    
    # Parse Sub 2: delta and weight
    # Sub 2 might also have the same IDs
    # Line index in sub2_lines:
    # [1] "   delta weight"
    # [2] "18  0.00  0.277"
    ids2 <- sapply(sub2_lines[-1], function(l) {
      tokens <- strsplit(trimws(l), " +")[[1]]
      if (length(tokens) > 0) return(tokens[1]) else return(NA)
    })
    ids2 <- ids2[!is.na(ids2)]
    
    df2 <- parse_fixed_width_table(sub2_lines[1], sub2_lines[-1], id_col = "model", row_labels = ids2)
    
    if (!is.null(df1) && !is.null(df2)) {
      df_modsel <- merge(df1, df2, by = "model", all = TRUE)
      write.csv(df_modsel, "tests/testthat/golden/culogist_Met_modsel.csv", row.names = FALSE, na="")
      cat("Saved culogist_Met_modsel\n")
    }
  }
}
# 4. Coefficients 2 (for chosen model)
coef2_idx <- grep("^Coefficients:", lines[(start_idx + 50):length(lines)])
if (length(coef2_idx) > 0) {
  coef2_start <- start_idx + 50 + coef2_idx[1] - 1
  coef2_lines <- extract_section_lines("^Coefficients:", coef2_start)
  if (!is.null(coef2_lines)) {
    # Pass row labels for coef2
    labels2 <- c("(Intercept)", "BMI", "LN_TG")
    df_coef2 <- parse_fixed_width_table(coef2_lines[2], coef2_lines[3:(length(coef2_lines)-1)], 
                                        id_col = "term", row_labels = labels2)
    df_coef2[[ncol(df_coef2)]] <- gsub(" \\*\\*\\*| \\*\\*| \\*| \\.|", "", df_coef2[[ncol(df_coef2)]])
    write.csv(df_coef2, "tests/testthat/golden/culogist_Met_coef2.csv", row.names = FALSE, na="")
    cat("Saved culogist_Met_coef2\n")
  }
}

# 5. Odds Ratio Stats
or_lines <- extract_section_lines("^Odds Ratio stats", start_idx)
if (!is.null(or_lines)) {
  df_or <- parse_fixed_width_table(or_lines[2], or_lines[3:length(or_lines)], id_col = "term")
  write.csv(df_or, "tests/testthat/golden/culogist_Met_oddsratio.csv", row.names = FALSE, na="")
  cat("Saved culogist_Met_oddsratio\n")
}

