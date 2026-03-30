# Helper functions for golden-value testing
# This file is auto-loaded by testthat (files matching helper-*.R)

#' Capture output with a very wide terminal to prevent table wrapping.
#'
#' testthat's local_reproducible_output() forces width=80 inside each test,
#' so we must set width=10000 inside the capture.output() call itself.
#'
#' @param expr Expression to evaluate while capturing output
#' @return Character vector of output lines
wide_capture <- function(expr) {
  old_width <- getOption("width")
  options(width = 10000)
  on.exit(options(width = old_width))
  # Use a png device so Unicode characters (e.g. Δ) in plot titles work
  # on macOS CI, where the default quartz device fails with mbcsToSbcs.
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  capture.output(expr)
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

#' Extract lines belonging to a section from captured output.
#'
#' Example usage:
#'   extract_section_lines(output, "Predictor summary", list("Shapiro-Wilk", "Models ranked"))
#'
#' @param output Character vector of captured output lines
#' @param header_pattern Pattern to identify the start of the section
#'   (matched via grepl, so regex is supported).
#' @param stop_prefix_list List of strings that mark the end of the section (checked via startsWith). Default is an empty list.
#' @return Character vector of section lines, or NULL if not found
extract_section_lines <- function(output, header_pattern, stop_prefix_list = list()) {
  if (length(output) == 1) output <- strsplit(output, "\n")[[1]]
  idx <- which(grepl(header_pattern, output))
  if (length(idx) == 0) return(NULL)
  start <- idx[1]
  end <- start + 1
  while (end <= length(output)) {
    line <- output[end]
    if (line == "" || startsWith(line, ">")) break
    matched_stop <- FALSE
    for (stop_prefix in stop_prefix_list) {
      if (startsWith(line, stop_prefix)) { matched_stop <- TRUE; break }
    }
    if (matched_stop) break
    end <- end + 1
  }
  return(output[start:(end-1)])
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
        if (values[best_col] == "") {
          values[best_col] <- substr(line, token_start, token_end)
        } else {
          # For column entries with spaces, append the token to the existing value.
          values[best_col] <- paste(values[best_col], substr(line, token_start, token_end))
        }
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
parse_summary_table <- function(output, col_names = NULL) {
  lines <- output
  if (length(lines) == 1) lines <- strsplit(lines, "\n")[[1]]

  # Strip leading blank lines
  while (length(lines) > 0 && trimws(lines[1]) == "") lines <- lines[-1]

  # Find the N row to anchor the table (N(tot), Nhave, or just N)
  # Limit search to the first 20 lines to avoid jumping to later sections
  search_limit <- min(length(lines), 20)
  n_idx <- grep("^N\\(tot\\)|^N |^Nhave", trimws(lines[1:search_limit]))
  if (length(n_idx) == 0) testthat::fail("Summary table (N row) not found near start of output")
  n_idx <- n_idx[1]

  # Header is the line immediately above N
  header_idx <- n_idx - 1
  if (header_idx < 1) testthat::fail("Summary table header not found above N row")

  # Find the end of data rows (first blank line after N row)
  end <- length(lines)
  for (i in (n_idx + 1):length(lines)) {
    if (trimws(lines[i]) == "") { end <- i - 1; break }
  }

  parse_fixed_width_table(lines[header_idx], lines[n_idx:end], col_names = col_names)
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
    if (trimmed == "" || startsWith(trimmed, "---") || startsWith(trimmed, "Signif")) break
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
  section <- extract_section_lines(output, section_pattern)
  if (is.null(section) || length(section) < 3) return(NULL)
  parse_fixed_width_table(section[2], section[3:length(section)],
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
  # Pure numeric comparison
  g_num <- suppressWarnings(as.numeric(expected))
  a_num <- suppressWarnings(as.numeric(actual))
  if (!is.na(g_num) && !is.na(a_num)) {
    if (tol == 0) {
      if (g_num != a_num)
        return(sprintf("expected %s, got %s", expected, actual))
    } else {
      res <- all.equal.numeric(g_num, a_num, tolerance = tol, scale = NULL)
      if (!isTRUE(res))
        return(sprintf("expected %s, got %s (%s)", expected, actual, res[1]))
    }
    return(NULL)
  }
  # Compound string comparison: when tol > 0, extract all numeric substrings
  # and compare pairwise (e.g. "37.4±6.91" vs "37.4±6.92", or
  # "26.1(23.7:29.6)" vs "26(23.7:29.6)")
  if (tol > 0 && expected != actual) {
    # Regular expression breakdown for numeric extraction:
    # -?                     : Optional leading negative sign
    # [0-9]+                 : One or more integer digits
    # \\.?                   : Optional decimal point
    # [0-9]*                 : Zero or more fractional digits
    # (?:[eE][+-]?[0-9]+)?   : Optional exponent (e.g., e-4, E+10) in a non-capturing group
    num_re <- "-?[0-9]+\\.?[0-9]*(?:[eE][+-]?[0-9]+)?"
    
    fmt_plus_minus <- paste0("^", num_re, " *\u00b1 *", num_re, "$")
    fmt_range <- paste0("^", num_re, " *\\( *", num_re, " *: *", num_re, " *\\)$")
    
    if ((grepl(fmt_plus_minus, expected) && grepl(fmt_plus_minus, actual)) ||
        (grepl(fmt_range, expected) && grepl(fmt_range, actual))) {
      
      g_matches <- regmatches(expected, gregexpr(num_re, expected, perl = TRUE))[[1]]
      a_matches <- regmatches(actual, gregexpr(num_re, actual, perl = TRUE))[[1]]
      if (length(g_matches) > 0 && length(g_matches) == length(a_matches)) {
        # Iterate and compare each component numeric value recursively
        for (k in seq_along(g_matches)) {
          msg <- compare_values(g_matches[k], a_matches[k], tol)
          if (!is.null(msg)) {
            return(sprintf("expected '%s', got '%s' (component %d: %s)",
                           expected, actual, k, msg))
          }
        }
        return(NULL)  # All numeric components within tolerance
      }
    }
  }
  if (expected != actual)
    return(sprintf("expected '%s', got '%s'", expected, actual))
  NULL
}

# --- Format matching: literal pattern with %n numeric placeholders ---

#' Match a literal pattern with %n numeric placeholders against text.
#'
#' The pattern is treated as literal text (all regex special characters are
#' escaped automatically). Each %n is replaced with a numeric capture group.
#' Captured numbers are compared against golden_nums with tolerance.
#'
#' @param text Single string to search in
#' @param pattern Literal string with %n placeholders for numbers
#' @param golden_nums Numeric vector of expected values (one per %n)
#' @param tol Relative tolerance for numeric comparisons (0 = exact)
#' @return A list with two elements:
#'   \item{found_matching_template}{Boolean indicating if the pattern was found structurally}
#'   \item{mismatches}{Character vector of numeric mismatch messages (empty on success)}
#' @keywords internal
check_format_match <- function(text, pattern, golden_nums, tol = 0) {
  num_re <- "(-?[0-9]+\\.?[0-9]*(?:[eE][+-]?[0-9]+)?)"
  escaped <- re_escape(pattern)
  re <- gsub("%n", num_re, escaped, fixed = TRUE)

  m <- regmatches(text, regexec(re, text, perl = TRUE))[[1]]
  if (length(m) == 0) {
    return(list(found_matching_template = FALSE, mismatches = character()))
  }

  captured <- m[2:(length(golden_nums) + 1)]
  mismatches <- character()
  for (i in seq_along(golden_nums)) {
    msg <- compare_values(as.character(golden_nums[i]), captured[i], tol)
    if (!is.null(msg))
      mismatches <- c(mismatches, sprintf("value %d: %s", i, msg))
  }
  return(list(found_matching_template = TRUE, mismatches = mismatches))
}

#' Assert that a literal pattern with %n numeric placeholders matches output.
#'
#' Wrapper around check_format_match that calls testthat::fail/succeed.
#'
#' @param output Character vector of captured output lines
#' @param pattern Literal string with %n placeholders for numbers
#' @param golden_nums Numeric vector of expected values (one per %n)
#' @param tol Relative tolerance for numeric comparisons (0 = exact)
#' @param label Descriptive label for error messages
expect_format_match <- function(output, pattern, golden_nums, tol = 0, label = "") {
  text <- paste(output, collapse = "\n")
  res <- check_format_match(text, pattern, golden_nums, tol)
  if (!res$found_matching_template) {
    testthat::fail(paste0(if (nchar(label) > 0) paste0(label, ": "), "Pattern not found: ", pattern))
  } else if (length(res$mismatches) > 0) {
    msg <- paste(res$mismatches, collapse = "; ")
    testthat::fail(paste0(if (nchar(label) > 0) paste0(label, ": "), msg))
  } else {
    testthat::succeed()
  }
}

# --- Posthoc comparison helpers ---

#' Escape a string for use in a perl regexp.
#' @keywords internal
re_escape <- function(s) gsub("([\\[\\](){}.*+?^$|\\\\])", "\\\\\\1", s, perl = TRUE)

#' Posthoc literal pattern (with %n placeholders) for a given label.
#'
#' Matches lines like:
#'   "Mono minus AAD: -17.9 ± 5.06, p=0.0012, CL=[-28.2,-7.63]"
#' Pattern: <label>: <diff> ± <se>, p=<p>, CL=[<cl_lo>,<cl_hi>]
#'
#' @keywords internal
posthoc_fmt <- function(label) {
  paste0(label, ": %n \u00b1 %n, p=%n, CL=[%n,%n]")
}

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
    pattern <- posthoc_fmt(row$comparison)
    golden_nums <- c(row$diff, row$se, row$p, row$cl_lo, row$cl_hi)
    res <- check_format_match(out_text, pattern, golden_nums, tol)
    if (!res$found_matching_template) {
      mismatches <- c(mismatches, sprintf("Row '%s': Pattern not found", row$comparison))
    } else if (length(res$mismatches) > 0) {
      msg <- paste(res$mismatches, collapse = "; ")
      mismatches <- c(mismatches, sprintf("Row '%s': %s", row$comparison, msg))
    }
  }

  if (length(mismatches) > 0) {
    testthat::fail(sprintf("Post-hoc '%s' found %d mismatch(es):\n%s",
                   golden_name, length(mismatches),
                   paste("  -", mismatches, collapse = "\n")))
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

  cursor <- 1
  current_group <- NULL

  for (i in seq_len(nrow(golden))) {
    row <- golden[i, ]

    # If group changes, find the next group header in output after cursor
    if (is.null(current_group) || row$group != current_group) {
      group_pattern <- sprintf("^%s *$", re_escape(row$group))
      found <- FALSE
      for (j in cursor:length(lines)) {
        if (grepl(group_pattern, lines[j])) {
          cursor <- j + 1
          current_group <- row$group
          found <- TRUE
          break
        }
      }
      if (!found) {
        mismatches <- c(mismatches,
          sprintf("Group '%s', row '%s': group header not found after line %d",
                  row$group, row$comparison, cursor))
        next
      }
    }

    # Find comparison sequentially below group header
    found_comp <- FALSE
    comp_pattern <- posthoc_fmt(row$comparison)
    golden_nums <- c(row$diff, row$se, row$p, row$cl_lo, row$cl_hi)

    while (cursor <= length(lines)) {
      if (trimws(lines[cursor]) == "") { cursor <- cursor + 1; next }

      res <- check_format_match(lines[cursor], comp_pattern, golden_nums, tol)
      if (!res$found_matching_template) {
        # If the current line does not match the target comparison, advance the
        # cursor to skip over irrelevant output or text until it is found.
        cursor <- cursor + 1
      } else {
        if (length(res$mismatches) > 0) {
          msg <- paste(res$mismatches, collapse = "; ")
          mismatches <- c(mismatches,
            sprintf("Group '%s', row '%s': %s", row$group, row$comparison, msg))
        }
        cursor <- cursor + 1
        found_comp <- TRUE
        break
      }
    }

    if (!found_comp) {
      mismatches <- c(mismatches,
        sprintf("Group '%s', row '%s': comparison not found under group",
                row$group, row$comparison))
    }
  }

  if (length(mismatches) > 0) {
    testthat::fail(sprintf("Grouped post-hoc '%s' found %d mismatch(es):\n%s",
                   golden_name, length(mismatches),
                   paste("  -", mismatches, collapse = "\n")))
  } else {
    testthat::succeed()
  }
}

#' Check that ordinal relative risk posthoc output matches golden CSV values.
#'
#' Example input:
#'   "bad vs > bad in:"
#'   "   lean vs overwt:(13/13 vs 12/18) RR=0.667, CL=[0.486,0.958] Fisher's Exact p=0.0275"
#'
#' Golden CSV columns: group, comparison, n1, d1, n2, d2, RR, lower, upper, p
#'
#' @param output Character vector of captured output lines
#' @param golden_name Basename of the golden CSV file (without .csv)
#' @param tol Relative tolerance for numeric comparisons (0 = exact match)
expect_ordinal_posthoc_match <- function(output, golden_name, tol = 0) {
  golden <- load_golden(golden_name)
  lines <- output
  if (length(lines) == 1) lines <- strsplit(lines, "\n")[[1]]
  mismatches <- character()

  cursor <- 1
  current_group <- NULL

  for (i in seq_len(nrow(golden))) {
    row <- golden[i, ]

    if (is.null(current_group) || row$group != current_group) {
      group_pattern <- sprintf("^%s *$", re_escape(row$group))
      found <- FALSE
      for (j in cursor:length(lines)) {
        if (grepl(group_pattern, lines[j])) {
          cursor <- j + 1
          current_group <- row$group
          found <- TRUE
          break
        }
      }
      if (!found) {
        mismatches <- c(mismatches, sprintf("Group '%s': header not found", row$group))
        next
      }
    }

    found_comp <- FALSE
    comp_pattern <- paste0(row$comparison,
      ":(%n/%n vs %n/%n) RR=%n, CL=[%n,%n] Fisher's Exact p=%n")
    golden_nums <- c(row$n1, row$d1, row$n2, row$d2,
                     row$RR, row$lower, row$upper, row$p)

    while (cursor <= length(lines)) {
      if (trimws(lines[cursor]) == "") { cursor <- cursor + 1; next }

      res <- check_format_match(lines[cursor], comp_pattern, golden_nums, tol)
      if (!res$found_matching_template) {
        # If the current line does not match the target comparison, advance the
        # cursor to skip over irrelevant output or text until it is found.
        cursor <- cursor + 1
      } else {
        if (length(res$mismatches) > 0) {
          msg <- paste(res$mismatches, collapse = "; ")
          mismatches <- c(mismatches,
            sprintf("Group '%s', row '%s': %s", row$group, row$comparison, msg))
        }
        cursor <- cursor + 1
        found_comp <- TRUE
        break
      }
    }
    if (!found_comp)
      mismatches <- c(mismatches, sprintf("Group '%s', row '%s': not found",
                                           row$group, row$comparison))
  }

  if (length(mismatches) > 0) {
    testthat::fail(sprintf("Ordinal posthoc found %d mismatches:\n%s",
                   length(mismatches), paste(" -", mismatches, collapse = "\n")))
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

  # Compare rows positionally — row order is expected to match
  n_rows <- max(nrow(golden), nrow(actual))
  for (i in seq_len(n_rows)) {
    if (i > nrow(golden)) {
      mismatches <- c(mismatches, sprintf("Extra row %d in actual: '%s'", i, actual[[id_col]][i]))
      next
    }
    if (i > nrow(actual)) {
      mismatches <- c(mismatches, sprintf("Row '%s': missing from actual output", golden[[id_col]][i]))
      next
    }

    golden_id <- golden[[id_col]][i]
    actual_id <- actual[[id_col]][i]
    if (golden_id != actual_id) {
      mismatches <- c(mismatches, sprintf("Row %d: expected id '%s', got '%s'", i, golden_id, actual_id))
      next
    }

    # Compare each non-id column
    data_cols <- setdiff(names(golden), id_col)
    for (col in data_cols) {
      if (!(col %in% names(actual))) {
        mismatches <- c(mismatches, sprintf("Row '%s', col '%s': column missing from actual", golden_id, col))
        next
      }
      g_val <- as.character(golden[i, col])
      a_val <- as.character(actual[i, col])
      msg <- compare_values(g_val, a_val, tol)
      if (!is.null(msg))
        mismatches <- c(mismatches, sprintf("Row '%s', col '%s': %s", golden_id, col, msg))
    }
  }

  if (length(mismatches) > 0) {
    actual_str <- paste(utils::capture.output(print(actual)), collapse = "\n")
    golden_str <- paste(utils::capture.output(print(golden)), collapse = "\n")
    msg <- sprintf(
      "%s comparison found %d mismatch(es):\n%s\n\nActual table:\n%s\n\nExpected (golden) table:\n%s",
      label, length(mismatches),
      paste("  -", mismatches, collapse = "\n"),
      actual_str, golden_str)
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
#' @param tol Relative tolerance for numeric comparisons (0 = exact)
expect_kw_match <- function(output, golden_name, tol = 0) {
  golden <- load_golden(golden_name)
  row <- golden[1, ]
  expect_format_match(output,
    "Kruskal-Wallis chi-squared = %n, df = %n, p-value = %n",
    c(row$chi_squared, row$df, row$p), tol = tol,
    label = "Kruskal-Wallis")
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
#'   ""
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

  parse_matrix_from_header(lines, header_idx)
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
         (trimws(lines[header_idx]) == "" || startsWith(lines[header_idx], "data:")))
    header_idx <- header_idx + 1

  parse_matrix_from_header(lines, header_idx)
}

#' Parse a lower-triangular matrix starting from a known header line.
#'
#' Collects data lines below the header until a blank line or a footer
#' pattern ("P value", "Each p-value", "At tcpre"). Uses read.table to
#' parse the block.
#'
#' @param lines Character vector of all output lines
#' @param header_idx Index of the header line (group names)
#' @return A data.frame with a "stat" column and one column per group
#' @keywords internal
parse_matrix_from_header <- function(lines, header_idx) {
  # Collect data lines until blank or footer
  data_lines <- character()
  for (i in (header_idx + 1):length(lines)) {
    trimmed <- trimws(lines[i])
    if (trimmed == "" || startsWith(trimmed, "P value") ||
        startsWith(trimmed, "Each p-value") || startsWith(trimmed, "At tcpre")) break
    data_lines <- c(data_lines, lines[i])
  }

  # Parse the header to get column names
  col_names <- strsplit(trimws(lines[header_idx]), "\\s+")[[1]]

  # Parse each data line: first token is row name, rest are values (left-to-right)
  rows <- list()
  for (dl in data_lines) {
    tokens <- strsplit(trimws(dl), "\\s+")[[1]]
    row_name <- tokens[1]
    vals <- tokens[-1]
    # Pad with NA to fill remaining columns
    padded <- c(vals, rep(NA, length(col_names) - length(vals)))
    rows[[length(rows) + 1]] <- c(row_name, padded)
  }

  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("stat", col_names)
  # Replace NA with "" for consistency with golden CSV
  df[is.na(df)] <- ""
  df
}

#' Parse "Partial F-test vs simpler models" section.
#'
#' Example output:
#'   "Partial F-test vs simpler models:"
#'   "p=1.2e-09 vs model with just Diet - cu1way(tcstudy,Diet)"
#'   ...
#'
#' @param output Character vector of captured output lines
#' @return A data.frame with columns p_value and model
parse_f_tests <- function(output) {
  section <- extract_section_lines(output, "Partial F-test vs simpler models:")
  if (is.null(section)) testthat::fail("F-tests header not found")

  f_rows <- list()
  for (line in section[-1]) {
    m <- regmatches(trimws(line), regexec("^p=([^ ]+) vs (.+)$", trimws(line)))[[1]]
    if (length(m) > 2) {
      f_rows[[length(f_rows) + 1]] <- data.frame(p_value = m[2], model = m[3],
                                                 stringsAsFactors = FALSE)
    }
  }

  do.call(rbind, f_rows)
}

#' Check that Partial F-tests match a golden CSV.
#'
#' Golden CSV columns: p_value, model
#'
#' @param output Character vector of captured output lines
#' @param golden_name Basename of the golden CSV file (without .csv)
#' @param tol Relative tolerance for numeric comparisons (0 = exact match)
expect_partial_f_match <- function(output, golden_name, tol = 0) {
  actual <- parse_f_tests(output)
  golden <- load_golden(golden_name)
  expect_table_match(actual, golden, id_col = "model",
                     label = paste("F-tests", golden_name), tol = tol)
}
