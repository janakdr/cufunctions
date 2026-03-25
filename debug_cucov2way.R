library(testthat)
attach_NEJM()

out <- capture.output(cucov2way(tcstudy, tcpre, Diet, sex, plot="no"))
writeLines(out, "scratch_cucov2way_output.txt")
cat("Captured output saved.\n")
