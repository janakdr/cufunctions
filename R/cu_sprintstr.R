#' internal cufunction to create a string of length width, padded or truncated
#' @keywords internal
cu_sprintstr = function(chstr,width) {
  return(ifelse(nchar(chstr)>width, substr(chstr,1,width), sprintf("%-*s",width,chstr)))
}
