#' Find substring position (equivalent of PL/I index function)
#'
#' @param str Character string to search in.
#' @param sub Character substring to search for.
#' @return Integer position of first occurrence, or 0 if not found.
#' @keywords internal
cu_index = function(str,sub) {
  # equivalent of PL/I index function
  if ((nchart=nchar(str)) < (nchars=nchar(sub))) return(0)
  for (i in 1:nchart-nchars+1) {
    #cat(" /check",i)
    if (sub==substr(str,i,i+nchars-1)) return(i)
  }
  return(0)
}
