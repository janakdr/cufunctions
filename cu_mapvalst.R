#' Map p-value to asterisk notation
#'
#' @param pval Numeric p-value.
#' @param pvcuts Numeric vector of significance cut points.
#' @return Character string: "ns", "*", "**", "***", or "****".
#' @keywords internal
cu_mapvalst = function(pval, pvcuts)
  return(ifelse(pval>pvcuts[2],"ns", ifelse(pval>pvcuts[3],"*",
         ifelse(pval>pvcuts[4],"**", ifelse(pval>pvcuts[5],"***","****")))))
