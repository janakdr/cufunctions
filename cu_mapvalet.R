#' Map p-value to label or asterisks
#'
#' @param pval Numeric p-value.
#' @param pvlab Character, "p" for numeric or "*" for asterisks.
#' @param pvcuts Numeric vector of significance cut points.
#' @return Formatted character string for the p-value.
#' @keywords internal
cu_mapvalet = function(pval, pvlab, pvcuts)
  return(ifelse(is.na(pval),"",ifelse(pvlab=="p", ifelse(pval>pvcuts[1],"NS",
         ifelse(pval>pvcuts[2], sprintf("%.3f",pval),
         ifelse(pval>pvcuts[4], sprintf("%.5f",pval),
         ifelse(pvcuts[4]>=0.01, sprintf("<%.2f",pvcuts[4]),
         ifelse(pvcuts[4]>=0.001, sprintf("<%.3f",pvcuts[4]),
         sprintf("<%.4f",pvcuts[4])))))),  cu_mapvalst(pval, pvcuts))))
