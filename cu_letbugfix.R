cu_letbugfix = function(letbar,namegroup1,gp1,gp2)
{
#  cat("\nDue to a bug in an outside package,")
#  cat("\nthe significance letters on the bars may be in reverse order within each level of",namegroup1)
#  cat("\nThe correct order of significance letters going from left to right:\n")
#  cat("\n",letbar)
  letbfix = letbar; locb = 0
  for (i in 1:gp1) {
    for (j in 1:gp2) letbfix[locb+j] = letbar[locb+gp2+1-j]
    locb = locb+gp2
  }
#  cat("\n",letbfix)
  return(letbfix)
}
