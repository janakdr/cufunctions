#' internal cufunction to calculate chi-squared and Fisher exact p-values
#' returns 6-list with chi-squared and Fisher p-values, RR/OR, conf.limits, SE
#' @keywords internal
cu_chi2fish = function(contab, prelabel, casecontrol=F) {
  mtt = 0
  for (i in (1:nrow(contab))) {
    rowsum = sum(contab[i,]); mtt = mtt+rowsum
    minrowsum = ifelse(i==1, rowsum, min(minrowsum,rowsum))
  }
  for (j in (1:ncol(contab))) {
    colsum = sum(contab[,j])
    mincolsum = ifelse(j==1, colsum, min(mincolsum,colsum))
  }
  # cat("\ni,j,mtt,minrowsum,mincolsum",i,j,mtt,minrowsum,mincolsum,"\n")
  minrc = minrowsum*mincolsum; RROR = -1; cl1 = -1; cl2 = -1; selogrror = -1
      # if (minrc<0) {pchi2 = -1; pfish = -1}
  minexp = minrc/mtt
  pchi2 = ifelse(minexp<5, -0.8, chisq.test(contab)$p.value)
  pfish = ifelse(minexp>10, -0.5, fisher.test(contab,simulate.p.value=TRUE)$p.value)
  if (nrow(contab)>2 || ncol(contab)>2) {
    cat("\n")
    if (pchi2>0) cat("Overall Pearson's \u03C7\u00B2 p-value = ",
                     cu_pval9(pchi2),"\n",sep="")
    if (pfish>0) cat("Overall Fisher's Exact p-value = ",
                     cu_pval9(pfish),"\n",sep="")
  }
  else {
    m11 = contab[1,1]; m12 = contab[1,2]; m21 = contab[2,1]; m22 = contab[2,2]
    rowsum1 = m11+m12; rowsum2 = m21+m22
    if (casecontrol) {den2 = m21; den1 = m11;  chrror = "OR="; chslcol = ":"}
    else {den2 = rowsum2;  den1 = rowsum1;  chrror = "RR="; chslcol = "/"}
    RROR = ifelse(den2*m12 > 0, (m22/den2) * (den1/m12),
                  ifelse(den1*m22 > 0, 1e50, 1))  #1e50 is infinity
    logrror = ifelse(RROR==0,-1e50, log(RROR))
    if (min(contab) == 0) {
      pt51 = ifelse(casecontrol,0.5,1)
      m12c = m12+0.5; m22c = m22+0.5; den1c = den1+pt51; den2c = den2+pt51
      RRORc = (m22c/den2c) * (den1c/m12c)
      selogrror = ifelse(casecontrol, sqrt(1/m12c + 1/m22c + 1/den1c + 1/den2c),
                         sqrt(1/m12c + 1/m22c - 1/den1c - 1/den2c))
    }
    else {
      RRORc = RROR
      selogrror = ifelse(casecontrol, sqrt(1/m12 + 1/m22 + 1/den1 + 1/den2),
                       sqrt(1/m12 + 1/m22 - 1/den1 - 1/den2))
    }
    clfac = exp(1.96*selogrror)
    cl2 = RRORc*clfac; cl1 = ifelse(RROR==0, 0, RRORc/clfac)
    # cat ("\n",m11,m12,m21,m22)
    if (prelabel != "") cat("\n   ",prelabel,
                            "(",m12,chslcol,den1," vs ",m22,chslcol,den2,") ",chrror,
                            signif(RROR,3),", CL=[",signif(cl1,3),",",signif(cl2,3),"] ",
                            ifelse(pfish>0, paste("Fisher's Exact p=",signif(pfish,3),sep=""),
                                   paste("\u03C7\u00B2 p=",signif(pchi2,3),sep="")),sep="")
  }
  return(c(pchi2,pfish,RROR,cl1,cl2,selogrror))
}
