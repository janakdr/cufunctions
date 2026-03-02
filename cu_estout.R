cu_estout = function(fit, vec, conf.int=0.95,minimal=F) {
  #' internal cufunction to print estimable results, return p-value
  oldw <- getOption("warn")
  for (i in (2:length(vec))) { 
    if (is.na(fit$coefficients[i])) {
      if (!minimal) cat(":coeff ",i," (",names(fit$coefficients)[i],
          ") unavailable (missing combination?)\n",sep="")
      return(1.0)
    }
  }
  options(warn = -1)
  est = estimable(fit, vec, conf.int=conf.int)
  options(warn = oldw)
  if (!minimal) {
    cat(": ",signif(est[,1],3)," \u00B1 ",signif(est[,2],3),", p=",signif(est[,5],3),
        ", CL=[",signif(est[,6],3),",",signif(est[,7],3),"]",sep="")
    # cat(" (",vec,")",sep=" ")
    cat("\n")
  }
  return (est[,5])
}
