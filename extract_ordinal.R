# extract_ordinal.R
source("tests/testthat/helper-golden.R")

output <- readLines("cufuncs-tests.docx.txt")

# Helper to save golden
save_golden <- function(df, name) {
  dir.create("tests/testthat/golden", showWarnings = FALSE, recursive = TRUE)
  path <- file.path("tests/testthat/golden", paste0(name, ".csv"))
  write.csv(df, path, row.names = FALSE, na = "")
  cat("Saved", path, "\n")
}

# --- 1. cu1way(WTCAT, MetSyn, ordinal) ---
idx1 <- grep("cu1way\\(WTCAT, MetSyn", output)
if (length(idx1) > 0) {
  start <- idx1[1]
  lines <- output[start:length(output)]
  
  # Summary Table
  sum_start <- grep("WTCAT compared across", lines)

  if (length(sum_start) > 0) {
    sum_lines <- lines[sum_start:length(lines)]
    end <- 2
    while (end <= length(sum_lines) && sum_lines[end] != "") end <- end + 1
    sum_block <- sum_lines[1:(end-1)]
    
    # Parse using extract_section_lines format or manual
    header <- "               No     Yes       All"
    data <- c(
      "Nhave         65      20        85",
      "NA's           0       0         0",
      "lean   38.5%: 25   5%: 1 30.6%: 26",
      "overwt   40%: 26 50%: 10 42.4%: 36",
      "obese  21.5%: 14  45%: 9 27.1%: 23"
    )
    # Since we know the structure, well just parse it
    # Wait, we can use parse_fixed_width_table with CONCATENATE aggregation!
    # Let's see if creating it manually or relying on helper is best
    # We'll rely on the helper during testing and just save the block to verify
    
    df_sum <- data.frame(
      stat = c("Nhave", "NA's", "lean", "overwt", "obese"),
      No = c("65", "0", "38.5%: 25", "40%: 26", "21.5%: 14"),
      Yes = c("20", "0", "5%: 1", "50%: 10", "45%: 9"),
      All = c("85", "0", "30.6%: 26", "42.4%: 36", "27.1%: 23"),
      stringsAsFactors = FALSE
    )
    save_golden(df_sum, "cu1way_ordinal_summary")
  }
}

# --- 2. cu2way(feel, WTCAT, Sex, ordinal) ---
idx2 <- grep("^> cu2way\\(feel,WTCAT,Sex", output)
if (length(idx2) > 0) {
  start <- idx2[1]
  lines <- output[start:length(output)]
  
  # Counts Matrix
  counts_start <- grep("^     lean&F", lines)
  if (length(counts_start) > 0) {
    c_lines <- lines[counts_start:length(lines)]
    end <- 2
    while (end <= length(c_lines) && c_lines[end] != "") end <- end + 1
    
    df_counts <- data.frame(
      stat = c("bad", "ok", "good"),
      "lean&F" = c(0, 4, 9),
      "lean&M" = c(0, 9, 4),
      "overwt&F" = c(6, 6, 6),
      "overwt&M" = c(6, 6, 6),
      "obese&F" = c(4, 7, 0),
      "obese&M" = c(8, 4, 0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    save_golden(df_counts, "cu2way_ordinal_counts")
  }
  
  # Relative Risks Posthoc
  df_post <- data.frame(
    group = c(rep("bad vs > bad in:", 9), rep("<good vs good in:", 9)),
    comparison = c(
      "lean vs overwt", "lean vs obese", "overwt vs obese", # F
      "lean vs overwt", "lean vs obese", "overwt vs obese", # M
      "lean", "overwt", "obese", # F vs M
      "lean vs overwt", "lean vs obese", "overwt vs obese", # <good vs good, F
      "lean vs overwt", "lean vs obese", "overwt vs obese", # M
      "lean", "overwt", "obese" # F vs M
    ),
    RR = c(
      0.667, 0.636, 0.955,
      0.667, 0.333, 0.5,
      1, 1, 0.524,
      0.481, 0, 0,
      1.08, 0, 0,
      0.444, 1, 1
    ),
    lower = c(
      0.486, 0.413, 0.549,
      0.486, 0.169, 0.211,
      0.867, 0.63, 0.209,
      0.228, 0, 0,
      0.381, 0, 0,
      0.182, 0.397, 0.0198
    ),
    upper = c(
      0.958, 1.02, 1.66,
      0.958, 0.763, 1.19,
      1.15, 1.59, 1.31,
      1.02, 0.948, 1.97,
      3.08, 2.01, 1.83,
      1.08, 2.52, 43
    ),
    p = c(
      0.0275, 0.0311, 1,
      0.0275, 0.000458, 0.135,
      1, 1, 0.22,
      0.0732, 0.000589, 0.0576,
      1, 0.0957, 0.0568,
      0.115, 1, 1
    ),

    stringsAsFactors = FALSE
  )
  save_golden(df_post, "cu2way_ordinal_posthoc")
}

