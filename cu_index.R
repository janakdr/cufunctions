cu_index = function(str,sub) {
  # equivalent of PL/I index function
  if ((nchart=nchar(str)) < (nchars=nchar(sub))) return(0)
  for (i in 1:nchart-nchars+1) {
    #cat(" /check",i)
    if (sub==substr(str,i,i+nchars-1)) return(i)
  }
  return(0)
}
