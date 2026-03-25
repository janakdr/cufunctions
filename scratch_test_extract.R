source('tests/testthat/helper-golden.R')
output <- readLines('scratch_culogist_output.txt')
or_lines <- extract_section_lines(output, "Odds Ratio stats")
print(or_lines)
