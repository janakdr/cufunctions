# Helper functions for golden-value testing
# This file is auto-loaded by testthat (files matching helper-*.R)

#' Attach the NEJM dataset for the current test file.
#' Use in test files as:
#'   local_NEJM <- function(env = parent.frame()) {
#'     attach_NEJM()
#'     withr::defer(detach_NEJM(), envir = env)
#'   }
attach_NEJM <- function() {
  data(NEJM, envir = parent.frame())
  attach(NEJM, warn.conflicts = FALSE)
}

detach_NEJM <- function() {
  detach(NEJM)
}

#' Load a golden CSV from the golden/ directory
#'
#' @param name Basename of the CSV file (without .csv extension)
#' @return A data.frame with column "stat" as row identifier
load_golden <- function(name) {
  path <- file.path(test_path("golden"), paste0(name, ".csv"))
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Parse a fixed-width table given a header line and data lines.
#'
#' Determines column positions from the header line using right-edge alignment,
#' then extracts values from each data line by assigning tokens to the column
#' whose right edge is >= the token's right edge.
#'
#' @param header_line The header line containing column names
#' @param data_lines Character vector of data lines to parse
#' @param id_col Name for the first column (row labels), default "stat"
#' @param col_names Optional column names to search for in the header.
#'   Required for multi-word headers (e.g. "Std. Error"). If NULL, splits
#'   on whitespace (assumes single-word column names).
#' @param row_labels Optional row labels to match at the start of each line.
#'   Required for multi-word row labels (e.g. "1st Q"). If NULL, takes the
#'   first whitespace-delimited token.
#' @return A data.frame with id_col and one column per header column
parse_fixed_width_table <- function(header_line, data_lines, id_col = "stat",
                                    col_names = NULL, row_labels = NULL) {
  if (is.null(col_names)) {
    # Single-word headers: split on whitespace
    hdr_matches <- gregexpr("\\S+", header_line)[[1]]
    col_names <- regmatches(header_line, list(hdr_matches))[[1]]
    col_ends <- hdr_matches + attr(hdr_matches, "match.length") - 1
  } else {
    # Multi-word headers: find each name by plain string search
    col_ends <- integer(length(col_names))
    for (k in seq_along(col_names)) {
      pos <- regexpr(col_names[k], header_line, fixed = TRUE)
      if (pos[1] == -1)
        stop("Column header '", col_names[k], "' not found in: ", header_line)
      col_ends[k] <- pos[1] + attr(pos, "match.length") - 1
    }
  }
  ncols <- length(col_names)

  rows <- list()
  label_idx <- 0
  for (line in data_lines) {
    if (trimws(line) == "") break

    # Determine row label and where data starts
    if (!is.null(row_labels)) {
      # Labels are in order — use the next one
      label_idx <- label_idx + 1
      if (label_idx > length(row_labels)) break
      label <- row_labels[label_idx]
      label_pos <- regexpr(label, line, fixed = TRUE)
      if (label_pos[1] == -1)
        stop("Row label '", label, "' not found in line: ", line)
      label_end <- label_pos + attr(label_pos, "match.length") - 1
    } else {
      label <- strsplit(trimws(line), "\\s+")[[1]][1]
      label_end <- regexpr(label, line, fixed = TRUE)
      label_end <- label_end + attr(label_end, "match.length") - 1
    }

    # Extract values by position
    remaining <- substr(line, label_end + 1, nchar(line))
    remaining_start <- label_end + 1
    values <- rep("", ncols)

    matches <- gregexpr("\\S+", remaining)[[1]]
    if (matches[1] != -1) {
      for (m in seq_along(matches)) {
        token_start <- remaining_start - 1 + matches[m]
        token_end <- token_start + attr(matches, "match.length")[m] - 1
        best_col <- ncols
        for (k in seq_len(ncols)) {
          if (col_ends[k] >= token_end) {
            best_col <- k
            break
          }
        }
        values[best_col] <- substr(line, token_start, token_end)
      }
    }

    rows[[length(rows) + 1]] <- c(setNames(label, id_col),
                                   setNames(values, col_names))
  }

  as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
}

#' Parse the summary statistics table from captured console output.
#'
#' Strips leading blank lines, then treats line 1 as title, line 2 as header,
#' and line 3+ as data until the first blank line.
#'
#' @param output Character vector of captured output lines
#' @return A data.frame with a "stat" column and one column per group
parse_summary_table <- function(output) {
  lines <- output
  if (length(lines) == 1) lines <- strsplit(lines, "\n")[[1]]

  # Strip leading blank lines so title is always line 1, header line 2
  while (length(lines) > 0 && trimws(lines[1]) == "") lines <- lines[-1]

  # Find the end of data rows (first blank line after header)
  end <- length(lines)
  for (i in 3:length(lines)) {
    if (trimws(lines[i]) == "") { end <- i - 1; break }
  }

  parse_fixed_width_table(lines[2], lines[3:end])
}

#' Parse a table with significance stars from captured console output.
#'
#' Finds the header line by searching for col_names, strips significance
#' stars from data lines, and stops at "---" or "Signif.". Verifies that
#' the stripped stars match what's expected from the p-value column.
#'
#' Used for both ANOVA and regression coefficients tables.
#'
#' @param output Character vector of captured output lines
#' @param id_col Name for the row-label column (e.g. "source" or "term")
#' @param col_names Column names to search for in the header.
#' @param p_col Name of the p-value column for star verification.
#' @return A data.frame with id_col and one column per header column
parse_starred_table <- function(output, id_col, col_names, p_col = NULL) {
  lines <- output
  if (length(lines) == 1) lines <- strsplit(lines, "\n")[[1]]

  # Find header line: the line containing all col_names
  header_idx <- NULL
  for (i in seq_along(lines)) {
    if (all(sapply(col_names, function(n) grepl(n, lines[i], fixed = TRUE)))) {
      header_idx <- i
      break
    }
  }
  if (is.null(header_idx)) return(data.frame())

  # Collect raw and stripped data lines until --- or Signif.
  raw_lines <- character()
  data_lines <- character()
  for (i in (header_idx + 1):length(lines)) {
    line <- lines[i]
    trimmed <- trimws(line)
    if (trimmed == "" || grepl("^---", trimmed) || grepl("^Signif", trimmed)) break
    raw_lines <- c(raw_lines, line)
    data_lines <- c(data_lines, sub("\\s+[*\\.]+\\s*$", "", line))
  }

  result <- parse_fixed_width_table(lines[header_idx], data_lines,
                                    id_col = id_col, col_names = col_names)

  # Verify significance stars from p-value column
  if (!is.null(p_col) && p_col %in% names(result)) {
    for (i in seq_len(nrow(result))) {
      p_val <- suppressWarnings(as.numeric(result[[p_col]][i]))
      if (is.na(p_val) || result[[p_col]][i] == "") next
      expected_stars <- if (p_val < 0.001) "***"
                        else if (p_val < 0.01) "**"
                        else if (p_val < 0.05) "*"
                        else if (p_val < 0.1) "."
                        else ""
      star_match <- regmatches(raw_lines[i], regexpr("[*\\.]+\\s*$", raw_lines[i]))
      actual_stars <- if (length(star_match) > 0) trimws(star_match) else ""
      if (actual_stars != expected_stars) {
        testthat::fail(sprintf(
          "Row '%s': expected stars '%s' for p=%s, got '%s'",
          result[[id_col]][i], expected_stars, result[[p_col]][i], actual_stars))
      }
    }
  }

  result
}

#' Convenience wrapper: parse ANOVA table.
parse_anova_table <- function(output, col_names = NULL) {
  if (is.null(col_names)) col_names <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
  parse_starred_table(output, id_col = "source", col_names = col_names,
                      p_col = "Pr(>F)")
}

#' Convenience wrapper: parse regression coefficients table.
parse_coef_table <- function(output, col_names = NULL) {
  if (is.null(col_names)) col_names <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  parse_starred_table(output, id_col = "term", col_names = col_names,
                      p_col = "Pr(>|t|)")
}



#' Check that post-hoc comparison lines from a golden CSV appear in the output.
#'
#' Formats each golden CSV row into the expected output string
#' (e.g. "Mono minus AAD: -17.9 ± 5.06, p=0.0012, CL=[-28.2,-7.63]")
#' and verifies it appears in the captured output.
#'
#' @param output Character vector of captured output lines
#' @param golden_name Basename of the golden CSV file (without .csv)
expect_posthoc_match <- function(output, golden_name) {
  golden <- load_golden(golden_name)
  out_text <- paste(output, collapse = "\n")
  for (i in seq_len(nrow(golden))) {
    row <- golden[i, ]
    expected <- sprintf("%s: %s \u00b1 %s, p=%s, CL=[%s,%s]",
                        row$comparison, row$diff, row$se, row$p,
                        row$cl_lo, row$cl_hi)
    expect_match(out_text, expected, fixed = TRUE,
                 label = sprintf("post-hoc line '%s'", row$comparison))
  }
}

#' Compare two data frames cell by cell, collecting all mismatches.
#'
#' Reports all differences in a single failure message rather than
#' failing on the first mismatch.
#'
#' @param actual A data.frame parsed from function output
#' @param golden A data.frame loaded from golden CSV
#' @param id_col Name of the column used as row identifier (default "stat")
#' @param label Descriptive label for the comparison (used in error message)
expect_table_match <- function(actual, golden, id_col = "stat", label = "table") {
  mismatches <- character()

  # Match rows by id_col
  for (i in seq_len(nrow(golden))) {
    golden_id <- golden[[id_col]][i]
    actual_row <- which(actual[[id_col]] == golden_id)

    if (length(actual_row) == 0) {
      mismatches <- c(mismatches, sprintf("Row '%s': missing from actual output", golden_id))
      next
    }
    actual_row <- actual_row[1]

    # Compare each non-id column
    data_cols <- setdiff(names(golden), id_col)
    for (col in data_cols) {
      if (!(col %in% names(actual))) {
        mismatches <- c(mismatches, sprintf("Row '%s', col '%s': column missing from actual", golden_id, col))
        next
      }
      g_val <- as.character(golden[i, col])
      a_val <- as.character(actual[actual_row, col])

      # Treat NA and empty string as equivalent
      g_is_empty <- is.na(g_val) || g_val == "" || g_val == "NA"
      a_is_empty <- is.na(a_val) || a_val == "" || a_val == "NA"

      if (g_is_empty && a_is_empty) next
      if (g_is_empty != a_is_empty) {
        mismatches <- c(mismatches,
          sprintf("Row '%s', col '%s': expected '%s', got '%s'",
                  golden_id, col,
                  ifelse(g_is_empty, "<empty>", g_val),
                  ifelse(a_is_empty, "<empty>", a_val)))
        next
      }

      # Try numeric comparison
      g_num <- suppressWarnings(as.numeric(g_val))
      a_num <- suppressWarnings(as.numeric(a_val))

      if (!is.na(g_num) && !is.na(a_num)) {
        if (g_num != a_num) {
          mismatches <- c(mismatches,
            sprintf("Row '%s', col '%s': expected %s, got %s",
                    golden_id, col, g_val, a_val))
        }
      } else {
        # String comparison for non-numeric values (like "<.001")
        if (g_val != a_val) {
          mismatches <- c(mismatches,
            sprintf("Row '%s', col '%s': expected '%s', got '%s'",
                    golden_id, col, g_val, a_val))
        }
      }
    }
  }

  # Check for extra rows in actual not in golden
  extra_ids <- setdiff(actual[[id_col]], golden[[id_col]])
  if (length(extra_ids) > 0) {
    mismatches <- c(mismatches,
      sprintf("Extra rows in actual not in golden: %s", paste(extra_ids, collapse = ", ")))
  }

  if (length(mismatches) > 0) {
    msg <- sprintf("%s comparison found %d mismatch(es):\n%s",
                   label, length(mismatches),
                   paste("  -", mismatches, collapse = "\n"))
    testthat::fail(msg)
  } else {
    testthat::succeed()
  }
}

#' Check that Kruskal-Wallis test output matches golden CSV values.
#'
#' Formats the golden row into the expected string
#' (e.g. "Kruskal-Wallis chi-squared = 10.193, df = 2, p-value = 0.006118")
#' and verifies it appears in the captured output.
#'
#' @param output Character vector of captured output lines
#' @param golden_name Basename of the golden CSV file (without .csv)
expect_kw_match <- function(output, golden_name) {
  golden <- load_golden(golden_name)
  out_text <- paste(output, collapse = "\n")
  row <- golden[1, ]
  expected <- sprintf("Kruskal-Wallis chi-squared = %s, df = %s, p-value = %s",
                      row$chi_squared, row$df, row$p)
  expect_match(out_text, expected, fixed = TRUE,
               label = "Kruskal-Wallis result")
}

#' Parse Dunn's pairwise comparison matrix from captured output.
#'
#' Finds the fixed subtitle "Pairwise comparisons using Dunn's all-pairs test",
#' skips to the header line (group names), collects data rows until the next
#' blank line or "P value", and delegates to parse_fixed_width_table.
#'
#' @param output Character vector or single string of captured output
#' @return A data.frame in matrix form with group names as columns
parse_dunn_table <- function(output) {
  lines <- output
  if (length(lines) == 1) lines <- strsplit(lines, "\n")[[1]]

  # Find the Dunn title line, then verify the subheader follows
  dunn_idx <- grep("Pairwise comparisons using Dunn's all-pairs test", lines, fixed = TRUE)
  if (length(dunn_idx) == 0) testthat::fail("Dunn title not found in output")
  dunn_idx <- dunn_idx[1]
  if (!grepl("(generalizes Wilcoxon rank-sum)", lines[dunn_idx + 1], fixed = TRUE))
    testthat::fail("Dunn subheader not found after title")
  header_idx <- dunn_idx + 2
  while (header_idx <= length(lines) && trimws(lines[header_idx]) == "") header_idx <- header_idx + 1

  # Collect data lines until blank or "P value"
  data_lines <- character()
  for (i in (header_idx + 1):length(lines)) {
    trimmed <- trimws(lines[i])
    if (trimmed == "" || grepl("^P value", trimmed)) break
    data_lines <- c(data_lines, lines[i])
  }

  # Parse the lower-triangular matrix with read.table (fills left-to-right)
  col_names <- strsplit(trimws(lines[header_idx]), "\\s+")[[1]]
  block <- paste(c(lines[header_idx], data_lines), collapse = "\n")
  df <- read.table(text = block, header = TRUE, fill = TRUE,
                   stringsAsFactors = FALSE, check.names = FALSE)
  # Add the row-label column
  df <- cbind(stat = rownames(df), df, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  # Replace NA with "" for consistency with golden CSV
  df[is.na(df)] <- ""
  df
}
