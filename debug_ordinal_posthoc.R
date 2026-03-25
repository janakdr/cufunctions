# debug_ordinal_posthoc.R
source("tests/testthat/helper-golden.R")
data(AJCN, envir = environment())

out <- capture.output(with(AJCN, cu2way(feel, WTCAT, Sex, g1order=c("lean","overwt","obese"), ordinal=c("bad","ok","good"))))

# Inspect expect_ordinal_posthoc_match's match behavior manually
golden <- read.csv("tests/testthat/golden/cu2way_ordinal_posthoc.csv", stringsAsFactors = FALSE)
lines <- out


cursor <- 1
current_group <- NULL
num_re <- "(-?[0-9\\.]+e?-?[0-9]*|Inf|-Inf)"

cat("--- Starting Debug Walk ---\n")
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
        cat("Found group header:", row$group, "at index", j, "\n")
        break
      }
    }
  }

  comp_re <- sprintf("%s:.*RR=%s, CL=\\[%s,%s\\].*p=%s", 
                     re_escape(row$comparison), num_re, num_re, num_re, num_re)
  found_comp <- FALSE
  
  cat("\nLooking for comparison:", row$comparison, "in group", current_group, "\n")
  cat("Regex pattern:", comp_re, "\n")
  
  while (cursor <= length(lines)) {
    if (trimws(lines[cursor]) == "") {
      cursor <- cursor + 1; next
    }
    
    cat("Evaluating line", cursor, ":", lines[cursor], "\n")
    match_groups <- regmatches(lines[cursor], regexec(comp_re, lines[cursor], perl = TRUE))[[1]]
    if (length(match_groups) > 0) {
      cat("--> MATCH SUCCESS at line", cursor, "\n")
      cat("Captured RR:", match_groups[2], "\n")
      cursor <- cursor + 1
      found_comp <- TRUE
      break
    } else {
      cursor <- cursor + 1
    }
  }
  if (!found_comp) {
    cat("--> FAILED TO FIND:", row$comparison, "\n")
  }
}
