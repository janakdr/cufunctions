#' internal cufunction to print p.value with labels (in cucov2way)
#' @keywords internal
cu_pvalmatout = function(p.value,levnams) {
  nlev = length(levnams); nlevm1 = nlev-1
  cat(sprintf("%-9.9s",""))
  #print(p.value)
  for (i in 1:nlevm1) cat(sprintf("%-9.9s",levnams[i]))
  for (j in 2:nlev) {
    cat(sprintf("\n%-9.9s",levnams[j]))
    for (i in 1:(j-1)) cat(cu_pval9(p.value[nlevm1*(i-1)+j-1]))
  }
}
