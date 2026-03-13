#' internal cufunction to write p-value to console 9 chars
#' @keywords internal
cu_pval9 = function(pval) {
  if (pval < -0.9) return(" NA      ")
  else if (pval < -0.7) return(" Inacc.  ")
  else if (pval < 0) return(" No need ")
  else if (pval<0.001) return(sprintf("%.2e%1s",pval,""))
  else return(sprintf("%-9.9s",signif(pval,3)))
}
