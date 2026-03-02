#' internal cufunction to print 2x2 and derived quantities
#' @keywords internal
cu_2x2out = function(mytab,row1name,row2name) {
  if (nrow(mytab)==2) rownames(mytab) <- c(row1name,row2name)
  else rownames(mytab)[1] <- ifelse(
    rownames(mytab)[1]=="TRUE",row2name,row1name)
  print(mytab)
  if (!is.na(mytab[4])) 
    cat("Accuracy ",signif(100*(mytab[1]+mytab[4])/(mytab[1]+mytab[3]+mytab[2]+mytab[4]),3),
        "%; Sensitivity ",signif(100*mytab[4]/(mytab[3]+mytab[4]),3),
        "%; Specificity ",signif(100*mytab[1]/(mytab[1]+mytab[2]),3),
        "%\nPositive Predictive Value ",signif(100*mytab[4]/(mytab[2]+mytab[4]),3),
        "%; Negative Predictive Value ",signif(100*mytab[1]/(mytab[1]+mytab[3]),3),
        "%\n",sep="")
}  