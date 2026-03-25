source("tests/testthat/helper-golden.R")

# Create golden directory if not exists
dir.create("tests/testthat/golden", showWarnings = FALSE, recursive = TRUE)

# Read the docx.txt file
lines <- readLines("cufuncs-tests.docx.txt")

# --- Case 1: curepmeas(delta, "like", "Diet", ordinal=...) ---
cat("Extracting curepmeas Case 1...\n")

# 1. Frequency Table (like)
# Starts around line 2734 in original file if not shifted.
# We will search for it dynamically.

# Helper to parse fixed width table lines into a DataFrame, handling wrapping
parse_lines_to_df <- function(table_lines, id_col_name = "term") {
  if (length(table_lines) < 2) return(NULL)
  
  # Find sub-table starts based on "Nhave" row.
  # The line BEFORE "Nhave" is the header line.
  nhave_idxs <- grep("^ *Nhave", table_lines)
  if (length(nhave_idxs) == 0) {
    # Fallback to single table parsing
    return(parse_single_subtable(table_lines, id_col_name))
  }
  
  sub_tables <- list()
  for (k in seq_along(nhave_idxs)) {
    nhave_idx <- nhave_idxs[k]
    header_idx <- nhave_idx - 1
    
    # Find end of this sub-table
    if (k < length(nhave_idxs)) {
      end_idx <- nhave_idxs[k+1] - 2 # Stops before next header
    } else {
      end_idx <- length(table_lines)
    }
    
    sub_lines <- table_lines[header_idx:end_idx]
    sub_df <- parse_single_subtable(sub_lines, id_col_name)
    if (!is.null(sub_df)) {
      sub_tables[[k]] <- sub_df
    }
  }
  
  if (length(sub_tables) == 0) return(NULL)
  
  # Merge sub-tables column-wise
  merged_df <- sub_tables[[1]]
  if (length(sub_tables) > 1) {
    for (k in 2:length(sub_tables)) {
      merged_df <- merge(merged_df, sub_tables[[k]], by = id_col_name, all = TRUE)
    }
  }
  return(merged_df)
}

# Generic single sub-table parser
parse_single_subtable <- function(table_lines, id_col_name = "term") {
  if (length(table_lines) < 1) return(NULL)
  df <- parse_fixed_width_table(table_lines[1], table_lines[-1], id_col = id_col_name)
  return(df)
}



# Helper to extract table and save
extract_and_save <- function(header_regex, start_idx, output_name, id_col = "term") {
  idx <- grep(header_regex, lines[start_idx:(start_idx+150)])
  if (length(idx) == 0) {
    cat("Table not found for", header_regex, "\n")
    return()
  }
  table_start <- start_idx + idx[1] - 1
  # Find end of table
  end <- table_start + 1
  while (end <= length(lines) && lines[end] != "" && 
         !grepl("^\\[1\\]", lines[end]) && 
         !grepl("^>", lines[end]) &&
         !grepl("^p-value NA", lines[end])) {
    end <- end + 1
  }
  table_lines <- lines[table_start:(end-1)]
  cat("Extracted tables lines for", output_name, ":\n")
  print(table_lines)
  
  df <- parse_lines_to_df(table_lines, id_col)
  if (!is.null(df)) {
    write.csv(df, file.path("tests/testthat/golden", paste0(output_name, ".csv")), row.names = FALSE, quote = FALSE)
    cat("Saved", output_name, "\n")
  }
}

case1_idx <- grep("^> curepmeas\\(delta,\"like\",\"Diet\"", lines)[1]
if (!is.na(case1_idx)) {
  cat("Processing Case 1 at line", case1_idx, "\n")
  extract_and_save("^like *$", case1_idx, "curepmeas_like_dist")
  extract_and_save("^like Δ count *$", case1_idx, "curepmeas_like_del")
  extract_and_save("^like updn *$", case1_idx, "curepmeas_like_updn")
  extract_and_save("^up:dn", case1_idx, "curepmeas_like_sign_test")
}

case2_idx <- grep("> curepmeas\\(delta,\"like\",\"Diet\",\"sex\"", lines)[1]
if (!is.na(case2_idx)) {
  cat("Processing Case 2 at line", case2_idx, "\n")
  extract_and_save("^like *$", case2_idx, "curepmeas_like_sex_dist")
  extract_and_save("^like Δ count *$", case2_idx, "curepmeas_like_sex_del")
  extract_and_save("^like updn *$", case2_idx, "curepmeas_like_sex_updn")
  extract_and_save("^up:dn", case2_idx, "curepmeas_like_sex_sign_test")
}

