#' internal cufunction to display chi-squared and Fisher exact tables
#' @keywords internal
cu_chifishout = function(casecont,rrortab,chitab,fishtab,nlev,namegroup,groupvar,maxfor2) {
  maxnch = min(12, max(nchar(namegroup), nchar(levels(groupvar))))
  cat("\n",cu_sprintstr(ifelse(casecont,"OR","RR"),maxnch)," ",sep="")
  for (i in 1:(nlev-1)) cat(cu_sprintstr(levels(groupvar)[i],9))
  for (i in 2:nlev) {
    cat("\n",cu_sprintstr(levels(groupvar)[i],maxnch)," ",sep="")
    for (j in 1:(i-1)) cat(cu_pval9(rrortab[i,j]))
  }
  chifish = c("Chi-squared p-values","Fisher exact p-values")
  if (nlev <= maxfor2) {
    cat("\n",cu_sprintstr(namegroup,maxnch)," ",sep="")
    # if (maxnch>7) cat(rep(" ",maxnch-7),sep="")
    for (jcf in 1:2) {
      for (i in 1:(nlev-1)) cat(cu_sprintstr(levels(groupvar)[i],9))
      cat("        ")
    }
    for (i in 2:nlev) {
      cat("\n",cu_sprintstr(levels(groupvar)[i],maxnch)," ",sep="")
      for (j in 1:(i-1)) cat(cu_pval9(chitab[i,j]))
      cat(c(rep("        ",nlev-i+1)))
      for (j in 1:(i-1)) cat(cu_pval9(fishtab[i,j]))
      # cat("\n")
    }
  }
  else for (jcf in 1:2) {
    cat("\n",chifish[jcf])
    cat("\n",cu_sprintstr(namegroup,maxnch)," ",sep="")
    for (i in 1:(nlev-1)) cat(cu_sprintstr(levels(groupvar)[i],9))
    for (i in 2:nlev) {
      cat("\n",cu_sprintstr(levels(groupvar)[i],maxnch)," ",sep="")
      for (j in 1:(i-1)) cat(cu_pval9(ifelse(jcf==1,chitab[i,j],fishtab[i,j])))
    }
  }
}
