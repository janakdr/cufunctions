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
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
           fileEncoding = "UTF-8")
}

#' Parse a fixed-width table given a header line and data lines.
#'
#' Determines column positions from the header line using right-edge alignment,
#' then extracts values from each data line by assigning tokens to the column
#' whose right edge is >= the token's right edge.
#'
#' Example input:
#'   header_line: "        AAD   Mono  Step1    All"
#'   data_lines:  c("Mean    -1.91  -19.8  -14.2    -12", ...)
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
    hdr_matches <- gregexpr("[^ ]+", header_line)[[1]]
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
      # Labels are in order -- use the next one
      label_idx <- label_idx + 1
      if (label_idx > length(row_labels)) break
      label <- row_labels[label_idx]
      label_pos <- regexpr(enc2utf8(label), enc2utf8(line), fixed = TRUE)
      if (label_pos[1] == -1)
        stop("Row label '", label, "' not found in line: ", line)
      label_end <- label_pos + attr(label_pos, "match.length") - 1
    } else {
      label <- strsplit(trimws(line), " +")[[1]][1]
      label_end <- regexpr(label, line, fixed = TRUE)
      label_end <- label_end + attr(label_end, "match.length") - 1
    }

    # Extract values by position
    remaining <- substr(line, label_end + 1, nchar(line))
    remaining_start <- label_end + 1
    values <- rep("", ncols)

    matches <- gregexpr("[^ ]+", remaining)[[1]]
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
#' Example output block:
#'   " tcchange compared across Diet groups"
#'   "        AAD   Mono  Step1    All"
#'   "Mean  -1.91  -19.8  -14.2    -12"
#'   ...
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
#' Example ANOVA output:
#'   "            Df Sum Sq Mean Sq F value  Pr(>F)"
#'   "group1       2   2015  1007.4   6.562 0.00399 **"
#'   ...
#'   "---"
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
    data_lines <- c(data_lines, sub(" +[*\\.]+ *$", "", line))
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
      star_match <- regmatches(raw_lines[i], regexpr("[*\\.]+ *$", raw_lines[i]))
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
#'
#' Example output:
#'   "            Df Sum Sq Mean Sq F value  Pr(>F)"
#'   "group1       2   2015  1007.4   6.562 0.00399 **"
#'   "Residuals   33   5066   153.5"
#'   "---"
parse_anova_table <- function(output, col_names = NULL) {
  if (is.null(col_names)) col_names <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
  parse_starred_table(output, id_col = "source", col_names = col_names,
                      p_col = "Pr(>F)")
}

#' Convenience wrapper: parse regression coefficients table.
#'
#' Example output:
#'   "            Estimate Std. Error t value Pr(>|t|)"
#'   "(Intercept)   -1.908      3.577  -0.534   0.5972"
#'   "DietMono     -17.925      5.058  -3.544   0.0012 **"
#'   ...
parse_coef_table <- function(output, col_names = NULL) {
  if (is.null(col_names)) col_names <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  parse_starred_table(output, id_col = "term", col_names = col_names,
                      p_col = "Pr(>|t|)")
}

#' Parse output with multiple table chunks sharing the same row labels.
#'
#' Finds all header lines (lines starting with whitespace), parses each chunk
#' using parse_fixed_width_table, and merges them by `stat` column.
#' Each table chunk is assumed to end with a blank line.
#'
#' Example output (two chunks with same row labels, different columns):
#'   "        tcchange  tgchange"
#'   "Mean       -12      -11.3"
#'   ...
#'   ""
#'   "        hcchange"
#'   "Mean      -1.34"
#'   ...
#'
#' @param output Character vector of captured output lines
#' @param row_labels Character vector of expected row labels
#' @return A data.frame with all columns merged
parse_multi_chunk_table <- function(output, row_labels) {
  hdr_indices <- which(grepl("^ +[^ ]", output))
  actual <- NULL
  for (h in hdr_indices) {
    end <- h + 1
    while (end <= length(output) && trimws(output[end]) != "")
      end <- end + 1
    chunk <- parse_fixed_width_table(output[h], output[(h + 1):(end - 1)],
                                     row_labels = row_labels)
    if (is.null(actual)) actual <- chunk
    else actual <- merge(actual, chunk, by = "stat", all = TRUE, sort = FALSE)
  }
  actual[match(row_labels, actual$stat), ]
}

#' Parse a single fixed-width table from output, located by header pattern.
#'
#' Finds the first line starting with whitespace (the header), then parses
#' data lines below it until a blank line.
#'
#' Example output:
#'   "        AAD     Mono    Step1"
#'   "Mean  -1.91    -19.8    -14.2"
#'   ...
#'
#' @param output Character vector of captured output lines
#' @param row_labels Character vector of expected row labels
#' @return A data.frame
parse_brief_table <- function(output, row_labels) {
  hdr_idx <- which(grepl("^ +[^ ]", output))
  end <- hdr_idx[1] + 1
  while (end <= length(output) && trimws(output[end]) != "")
    end <- end + 1
  parse_fixed_width_table(output[hdr_idx[1]], output[(hdr_idx[1] + 1):(end - 1)],
                           row_labels = row_labels)
}

#' Parse a named section's table from compare-style output.
#'
#' Finds a section by grep pattern, then parses the fixed-width table below it.
#' The header is on the line after the section name, data starts 2 lines after.
#'
#' Example output:
#'   "tcchange"
#'   "        AAD   Mono  Step1"
#'   "Mean  -1.91  -19.8  -14.2"
#'   ...
#'
#' @param output Character vector of captured output lines
#' @param section_pattern Regex pattern to find the section header
#' @param col_names Column names for the table (excluding id column)
#' @param row_labels Optional row labels (passed to parse_fixed_width_table)
#' @return A data.frame
parse_section_table <- function(output, section_pattern,
                                col_names = NULL, row_labels = NULL) {
  idx <- grep(section_pattern, output)
  end <- idx[1] + 2
  while (end <= length(output) && trimws(output[end]) != "") end <- end + 1
  parse_fixed_width_table(output[idx[1] + 1], output[(idx[1] + 2):(end - 1)],
                           col_names = col_names, row_labels = row_labels)
}

# --- Shared comparison internals ---

#' Compare two scalar values, returning NULL on match or an error string.
#'
#' Handles empty/NA values, numeric comparison with optional tolerance,
#' and string fallback for non-numeric values.
#'
#' @param expected Expected value (character)
#' @param actual Actual value (character)
#' @param tol Relative tolerance (0 = exact numeric match)
#' @return NULL if values match, or a descriptive error string
#' @keywords internal
compare_values <- function(expected, actual, tol = 0) {
  g_empty <- is.na(expected) || expected == "" || expected == "NA"
  a_empty <- is.na(actual) || actual == "" || actual == "NA"
  if (g_empty && a_empty) return(NULL)
  if (g_empty != a_empty)
    return(sprintf("expected '%s', got '%s'",
                   if (g_empty) "<empty>" else expected,
                   if (a_empty) "<empty>" else actual))
  g_num <- suppressWarnings(as.numeric(expected))
  a_num <- suppressWarnings(as.numeric(actual))
  if (!is.na(g_num) && !is.na(a_num)) {
    if (tol == 0) {
      if (g_num != a_num)
        return(sprintf("expected %s, got %s", expected, actual))
    } else {
      rel_diff <- abs(g_num - a_num) / max(abs(g_num), abs(a_num), 1e-10)
      if (rel_diff > tol)
        return(sprintf("expected %s, got %s (rel diff %.2g > tol %.2g)",
                       expected, actual, rel_diff, tol))
    }
    return(NULL)
  }
  if (expected != actual)
    return(sprintf("expected '%s', got '%s'", expected, actual))
  NULL
}

# --- Posthoc comparison helpers ---

#' Regexp for matching a number (with optional sign and scientific notation).
#' @keywords internal
posthoc_num_re <- "(-?[0-9]+[.]?[0-9]*(?:[eE][+-]?[0-9]+)?)"

#' Build the posthoc value-matching regexp for a given label.
#'
#' Matches lines like:
#'   "Mono minus AAD: -17.9 ± 5.06, p=0.0012, CL=[-28.2,-7.63]"
#'
#' Pattern: <label>: <diff> ± <se>, p=<p>, CL=[<cl_lo>,<cl_hi>]
#' Each numeric field is captured as a group (5 groups total).
#'
#' @param label_re Escaped label to match before the colon
#' @return A perl-compatible regexp with 5 capture groups
#' @keywords internal
posthoc_pattern <- function(label_re) {
  re <- posthoc_num_re
  sprintf("%s: %s ± %s, p=%s, CL=\\[%s,%s\\]",
          label_re, re, re, re, re, re)
}

#' Compare 5 captured posthoc values against golden, collecting mismatches.
#' @param actual_vals Character vector of 5 captured values
#' @param golden_row A row from the golden data.frame
#' @param row_label Label for mismatch messages
#' @param tol Relative tolerance (0 = exact)
#' @return Character vector of mismatch messages (empty if all match)
#' @keywords internal
compare_posthoc_values <- function(actual_vals, golden_row, row_label, tol = 0) {
  col_names <- c("diff", "se", "p", "cl_lo", "cl_hi")
  mismatches <- character()
  for (j in seq_along(col_names)) {
    msg <- compare_values(as.character(golden_row[[col_names[j]]]),
                          actual_vals[j], tol)
    if (!is.null(msg))
      mismatches <- c(mismatches, sprintf("%s, col '%s': %s", row_label, col_names[j], msg))
  }
  mismatches
}

#' Escape a string for use in a perl regexp.
#' @keywords internal
re_escape <- function(s) gsub("([+().])", "\\\\\\1", s)

#' Check that flat post-hoc comparison lines match a golden CSV.
#'
#' Golden CSV columns: comparison, diff, se, p, cl_lo, cl_hi
#'
#' Example output line:
#'   "Mono minus AAD: -17.9 +/- 5.06, p=0.0012, CL=[-28.2,-7.63]"
#'   ...
#'
#' @param output Character vector of captured output lines
#' @param golden_name Basename of the golden CSV file (without .csv)
#' @param tol Relative tolerance for numeric comparisons (0 = exact match)
expect_posthoc_match <- function(output, golden_name, tol = 0) {
  golden <- load_golden(golden_name)
  out_text <- paste(output, collapse = "\n")
  mismatches <- character()

  for (i in seq_len(nrow(golden))) {
    row <- golden[i, ]
    pattern <- posthoc_pattern(re_escape(row$comparison))
    # regexec returns list of 1: element [1] is full match, [2:6] are captures
    match_groups <- regmatches(out_text, regexec(pattern, out_text, perl = TRUE))[[1]]
    if (length(match_groups) == 0) {
      mismatches <- c(mismatches,
        sprintf("Row '%s': not found in output", row$comparison))
      next
    }
    captured_values <- match_groups[2:6]  # diff, se, p, cl_lo, cl_hi
    mismatches <- c(mismatches,
      compare_posthoc_values(captured_values, row,
                             sprintf("Row '%s'", row$comparison), tol))
  }

  if (length(mismatches) > 0) {
    msg <- sprintf("Post-hoc '%s' found %d mismatch(es):\n%s",
                   golden_name, length(mismatches),
                   paste("  -", mismatches, collapse = "\n"))
    testthat::fail(msg)
  } else {
    testthat::succeed()
  }
}

#' Check grouped post-hoc comparisons (two-factor, e.g. cu2way).
#'
#' Golden CSV columns: group, comparison, diff, se, p, cl_lo, cl_hi
#'
#' Example output:
#'   "AAD"
#'   "M minus F: -20.1 +/- 4.37, p=6.91e-05, CL=[-29.1,-11.2]"
#'   ""
#'   "Mono"
#'   "M minus F: 19.9 +/- 4.37, p=7.94e-05, CL=[11,28.9]"
#'   ...
#'
#' @param output Character vector of captured output lines
#' @param golden_name Basename of the golden CSV file (without .csv)
#' @param tol Relative tolerance for numeric comparisons (0 = exact match)
expect_grouped_posthoc_match <- function(output, golden_name, tol = 0) {
  golden <- load_golden(golden_name)
  lines <- output
  if (length(lines) == 1) lines <- strsplit(lines, "\n")[[1]]
  mismatches <- character()

  # Process golden rows grouped by group name, in order
  groups <- unique(golden$group)
  for (grp in groups) {
    grp_rows <- golden[golden$group == grp, ]
    # Find the group header line
    group_idx <- grep(sprintf("^%s *$", re_escape(grp)), lines)
    if (length(group_idx) == 0) {
      for (k in seq_len(nrow(grp_rows)))
        mismatches <- c(mismatches,
          sprintf("Group '%s', row '%s': group header not found",
                  grp, grp_rows$comparison[k]))
      next
    }
    # Walk through non-blank lines below the header, matching in order
    golden_k <- 1
    for (j in (group_idx[1] + 1):length(lines)) {
      if (golden_k > nrow(grp_rows)) break
      if (trimws(lines[j]) == "") break  # group boundary
      expected_comp <- grp_rows$comparison[golden_k]
      comp_pattern <- posthoc_pattern(re_escape(expected_comp))
      # regexec returns [1]=full match, [2:6]=captured values
      match_groups <- regmatches(lines[j], regexec(comp_pattern, lines[j], perl = TRUE))[[1]]
      if (length(match_groups) == 0) {
        mismatches <- c(mismatches,
          sprintf("Group '%s', row '%s': expected comparison not found on line: %s",
                  grp, expected_comp, trimws(lines[j])))
        golden_k <- golden_k + 1
        next
      }
      captured_values <- match_groups[2:6]  # diff, se, p, cl_lo, cl_hi
      mismatches <- c(mismatches,
        compare_posthoc_values(captured_values, grp_rows[golden_k, ],
                               sprintf("Group '%s', row '%s'", grp, expected_comp), tol))
      golden_k <- golden_k + 1
    }
    # Report any remaining unmatched comparisons
    while (golden_k <= nrow(grp_rows)) {
      mismatches <- c(mismatches,
        sprintf("Group '%s', row '%s': comparison not found under group",
                grp, grp_rows$comparison[golden_k]))
      golden_k <- golden_k + 1
    }
  }

  if (length(mismatches) > 0) {
    msg <- sprintf("Grouped post-hoc '%s' found %d mismatch(es):\n%s",
                   golden_name, length(mismatches),
                   paste("  -", mismatches, collapse = "\n"))
    testthat::fail(msg)
  } else {
    testthat::succeed()
  }
}

#' Compare two data frames cell by cell, collecting all mismatches.
#'
#' Reports all differences in a single failure message rather than
#' failing on the first mismatch. Numeric values are compared with
#' optional relative tolerance; non-numeric values use exact string match.
#'
#' Example golden CSV:
#'   stat, AAD,  Mono, Step1
#'   Mean, -1.91, -19.8, -14.2
#'   ...
#'
#' @param actual A data.frame parsed from function output
#' @param golden A data.frame loaded from golden CSV
#' @param id_col Name of the column used as row identifier (default "stat")
#' @param label Descriptive label for the comparison (used in error message)
expect_table_match <- function(actual, golden, id_col = "stat", label = "table", tol = 0) {
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
      msg <- compare_values(g_val, a_val, tol)
      if (!is.null(msg))
        mismatches <- c(mismatches, sprintf("Row '%s', col '%s': %s", golden_id, col, msg))
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
#' Golden CSV columns: chi_squared, df, p
#'
#' Example output line:
#'   "Kruskal-Wallis chi-squared = 10.193, df = 2, p-value = 0.006118"
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
#' Example output:
#'   "Pairwise comparisons using Dunn's all-pairs test"
#'   "(generalizes Wilcoxon rank-sum)"
#'   "      AAD  Mono"
#'   "Mono  0.16 -"
#'   ...
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
  col_names <- strsplit(trimws(lines[header_idx]), " +")[[1]]
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

#' Parse a pairwise comparison matrix from output (general, non-Dunn).
#'
#' Finds the header by searching for "Pairwise comparisons using",
#' locates the matrix, and parses it with read.table.
#'
#' Example output:
#'   "Pairwise comparisons using t tests with pooled SD 12.4"
#'   "data:  tcchange compared across 3 Diet groups..."
#'   "      AAD    Mono"
#'   "Mono  0.0012 -"
#'   ...
#'
#' @param output Character vector of captured output lines
#' @return A data.frame in matrix form with group names as columns
parse_pairwise_matrix <- function(output) {
  lines <- output
  if (length(lines) == 1) lines <- strsplit(lines, "\n")[[1]]

  # Find "Pairwise comparisons using" line
  pw_idx <- grep("Pairwise comparisons using", lines, fixed = TRUE)
  if (length(pw_idx) == 0) testthat::fail("Pairwise comparisons header not found")
  pw_idx <- pw_idx[1]

  # Skip blanks + "data:" line + blanks to find the matrix header
  header_idx <- pw_idx + 1
  while (header_idx <= length(lines) &&
         (trimws(lines[header_idx]) == "" || grepl("^data:", lines[header_idx])))
    header_idx <- header_idx + 1

  # Collect data lines until blank or "P value"
  data_lines <- character()
  for (i in (header_idx + 1):length(lines)) {
    trimmed <- trimws(lines[i])
    if (trimmed == "" || grepl("^P value", trimmed)) break
    data_lines <- c(data_lines, lines[i])
  }

  block <- paste(c(lines[header_idx], data_lines), collapse = "\n")
  df <- read.table(text = block, header = TRUE, fill = TRUE,
                   stringsAsFactors = FALSE, check.names = FALSE)
  df <- cbind(stat = rownames(df), df, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  df[is.na(df)] <- ""
  df
}
