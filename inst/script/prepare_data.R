#!/usr/bin/env Rscript
# prepare_data.R — Import all tabs from cufunctions-data-new.xlsx into data/*.rda
#
# Run from the package root:
#   Rscript inst/script/prepare_data.R
#
# This script reads each sheet from the Google-Sheets-exported xlsx,
# applies the same cleaning logic that was used for the original datasets,
# and saves each as a compressed .rda file in data/.

library(readxl)

xlsx_path <- "cufunctions-data-new.xlsx"
if (!file.exists(xlsx_path)) {
  stop("Cannot find ", xlsx_path, " — run from the package root directory")
}

sheets <- excel_sheets(xlsx_path)
cat("Found sheets:", paste(sheets, collapse = ", "), "\n")

# Helper: coerce columns to the right type after import.
# readxl sometimes imports numbers as character when there are header issues.
coerce_types <- function(df) {
  for (cn in colnames(df)) {
    vals <- df[[cn]]
    # If already numeric, leave it alone
    if (is.numeric(vals)) next
    # Try converting to numeric; if <50 % NA we keep numeric
    num_vals <- suppressWarnings(as.numeric(vals))
    pct_na_before <- mean(is.na(vals))
    pct_na_after  <- mean(is.na(num_vals))
    if (pct_na_after <= pct_na_before + 0.1) {
      # Nearly all values are numeric — treat as numeric
      df[[cn]] <- num_vals
    } else {
      # Character / factor column
      df[[cn]] <- as.factor(vals)
    }
  }
  df
}

dir.create("data", showWarnings = FALSE)

for (sheet in sheets) {
  cat("\n--- Processing sheet:", sheet, "---\n")
  raw <- as.data.frame(read_excel(xlsx_path, sheet = sheet))
  cat("  Raw dimensions:", nrow(raw), "x", ncol(raw), "\n")
  cat("  Columns:", paste(colnames(raw), collapse = ", "), "\n")

  # --- Per-sheet cleaning ---
  if (sheet == "hepc") {
    # Drop extra metadata columns (empty/notes cols from the spreadsheet)
    keep_cols <- c("time", "status", "Treatment", "dead", "Treat3")
    raw <- raw[, keep_cols, drop = FALSE]
    cat("  Cleaned hepc: kept", ncol(raw), "columns\n")
  }

  df <- coerce_types(raw)

  if (sheet == "volc") {
    # varnam should be character (variable names), not factor
    df$varnam <- as.character(df$varnam)
  }

  # Print summary of types
  for (cn in colnames(df)) {
    cat("    ", cn, ": ", class(df[[cn]]), "\n")
  }

  # Assign to the sheet name and save
  assign(sheet, df)
  save(list = sheet, file = file.path("data", paste0(sheet, ".rda")),
       compress = "xz")
  cat("  Saved data/", sheet, ".rda  (", nrow(df), " rows, ", ncol(df),
      " cols)\n", sep = "")
}

# Remove old AJCN.rda (replaced by Met)
ajcn_path <- file.path("data", "AJCN.rda")
if (file.exists(ajcn_path)) {
  file.remove(ajcn_path)
  cat("\n✗ Removed data/AJCN.rda (replaced by Met)\n")
}

cat("\n✓ All sheets imported successfully.\n")

# Summary of what's in data/
cat("\nFiles in data/:\n")
cat(paste(" ", list.files("data"), collapse = "\n"), "\n")
