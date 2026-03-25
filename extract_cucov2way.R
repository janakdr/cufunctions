# Script to extract golden files from cufuncs-tests.docx.txt for cucov2way cases
library(testthat)

test_path <- function(dir) file.path("tests", "testthat", dir)
source("tests/testthat/helper-golden.R")

lines <- readLines("cufuncs-tests.docx.txt")

extract_block <- function(lines, start_pattern, end_pattern = "^> ") {
  idx <- grep(start_pattern, lines, fixed = TRUE)
  if (length(idx) == 0) stop(paste("Pattern not found:", start_pattern))
  idx <- idx[1]
  
  end_idx <- idx + 1
  while (end_idx <= length(lines)) {
    if (grepl(end_pattern, lines[end_idx])) break
    end_idx <- end_idx + 1
  }
  lines[(idx + 1):(end_idx - 1)]
}

parse_posthoc_to_df <- function(lines, group_headers) {
  rows <- list()
  current_group <- NULL
  
  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    if (line %in% group_headers) { current_group <- line; next }
    
    m <- regexec("([^:]+): +(-?[0-9.]+[^ ]+) ± +(-?[0-9.]+[^,]*), +p=([^,]+), +CL=\\[([^,]+),([^]]+)\\]", line)
    if (m[[1]][1] != -1) {
      groups <- regmatches(line, m)[[1]]
      rows[[length(rows) + 1]] <- data.frame(
        group = if (is.null(current_group)) "" else current_group,
        comparison = groups[2],
        diff = groups[3],
        se = groups[4],
        p = groups[5],
        cl_lo = groups[6],
        cl_hi = groups[7],
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

process_case <- function(case_name, case_lines) {
  cat(paste("Processing", case_name, "...\n"))
  
  # Summary
  sec <- grep("tcstudy compared across 6 Diet&sex groups", case_lines)
  if (length(sec) > 0) {
    summary_lines <- case_lines[sec[1]:length(case_lines)]
    actual_summary <- parse_summary_table(summary_lines)
    write.csv(actual_summary, paste0("tests/testthat/golden/", case_name, "_summary.csv"), row.names = FALSE)
  }
  
  # Pairwise
  tryCatch({
    actual_pw <- parse_pairwise_matrix(case_lines)
    write.csv(actual_pw, paste0("tests/testthat/golden/", case_name, "_pairwise.csv"), row.names = FALSE)
  }, error = function(e) cat(paste("Skipping pairwise for", case_name, "\n")))
  
  # Coef
  actual_coef <- parse_coef_table(case_lines)
  write.csv(actual_coef, paste0("tests/testthat/golden/", case_name, "_coef.csv"), row.names = FALSE)
  
  # Posthoc (Merged)
  diet_start <- grep("Diet comparisons", case_lines)
  f_start <- grep("Partial F-test", case_lines)
  
  if (length(diet_start) > 0) {
    end_idx <- if (length(f_start) > 0) f_start[1] else length(case_lines)
    posthoc_lines <- case_lines[diet_start[1]:end_idx]
    # Combined headers
    allowed_groups <- c("F", "M", "M minus F")
    posthoc_df <- parse_posthoc_to_df(posthoc_lines, allowed_groups)
    write.csv(posthoc_df, paste0("tests/testthat/golden/", case_name, "_posthoc.csv"), row.names = FALSE)
  }
  
  # F-tests
  if (length(f_start) > 0) {
    actual_f <- parse_f_tests(case_lines)
    write.csv(actual_f, paste0("tests/testthat/golden/", case_name, "_f_tests.csv"), row.names = FALSE)
  }
}

# Case 1
case1_lines <- extract_block(lines, "> cucov2way(tcstudy, tcpre, Diet, sex)")
process_case("cucov2way_tcstudy_tcpre_Diet_sex", case1_lines)

# Case 2
case2_lines <- extract_block(lines, "> cucov2way(tcstudy, tcpre, Diet, sex, c(160,180,200))")
process_case("cucov2way_tcstudy_tcpre_Diet_sex_c160", case2_lines)

# Case 3
case3_lines <- extract_block(lines, "> cucov2way(tcstudy, tcpre, Diet, sex, interact=F)")
process_case("cucov2way_tcstudy_tcpre_Diet_sex_nointeract", case3_lines)

# Case 4
case4_lines <- extract_block(lines, "> cucov2way(tcstudy, tcpre, Diet, sex, interact=F, c(160,180,200))")
process_case("cucov2way_tcstudy_tcpre_Diet_sex_nointeract_c160", case4_lines)

cat("All cases extracted.\n")
