lines <- readLines("cufuncs-tests.docx.txt")

cucov_starts <- grep("^> cucov2way", lines)

# For each start, we find the block "Analyzing (just as point of reference)"
# up to (but not including) "...controlling for tcpre "
remove_indices <- integer()

for (idx in cucov_starts) {
  block_start <- grep("^Analyzing \\(just as point of reference\\)", lines[(idx+1):(idx+15)])
  if (length(block_start) > 0) {
    start_line <- idx + block_start[1]
    # Find the end line: look for "...controlling for tcpre "
    end_line <- idx + grep("controlling for tcpre *$", lines[(idx+1):(idx+50)])[1] - 1
    remove_indices <- c(remove_indices, start_line:end_line)
  }
}

if (length(remove_indices) > 0) {
  lines <- lines[-remove_indices]
  writeLines(lines, "cufuncs-tests.docx.txt")
  message("Removed ", length(remove_indices), " lines from cufuncs-tests.docx.txt")
} else {
  message("No lines to remove.")
}
