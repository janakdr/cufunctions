library(readr)

files <- list.files("tests/testthat/golden", pattern = "\\.csv$", full.names = TRUE)
for (f in files) {
  lines <- readLines(f)
  if (length(lines) <= 1) {
    cat("Skipping empty or header-only file:", f, "\n")
    next
  }
  df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character", na.strings = c())
  write_csv(df, f, na = "")
}
cat("Finished updating golden files with minimal quoting.\n")
