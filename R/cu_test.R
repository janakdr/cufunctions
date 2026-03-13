#' Test function for internal development
#'
#' @param val Test input value.
#' @return A list with test objects.
#' @keywords internal
cu_test = function(val) {
  obj = NULL
  obj$first = "string"; obj$second = c(7,9,11)
  obj$third = matrix(data=c(1,4,7,10,13,16),nrow=3,ncol=4)
  return(obj)
  intfunc = function(inp) {yloc <<- 5; xglo=xglo*4; cat(xglo); return(xglo*inp)}
  xglo = 4; retval = intfunc(val)
  cat ("\nxglo,yloc,val,retval",xglo,yloc,val,retval)
  ret = NULL; ret$mapa = c(3,5); ret$mapb = c(4,2,17)
  ret
}
