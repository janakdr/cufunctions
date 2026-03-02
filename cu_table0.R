#' internal cufunction to calculate summary stats for variables
#' @keywords internal
cu_table0 = function(depvar, group1=NULL, brief=FALSE, sdsamerow=FALSE,
    doAll=FALSE, pnorm=.05, chariqr=":",  nlevmax=9)
{
  nrowmax = 16; irmean=3; irmed=9; # constants
  # options(warn = 1)
  if (is.character(depvar) && nlevels(as.factor(depvar)) > nlevmax) 
    depvar = rep(T, length(depvar))
  if (is.character(depvar) || is.logical(depvar)) depvar = as.factor(depvar)
  if (is.null(group1)) {nlev = 0; endloop = 1} # doAll not set
  else {
    if (!is.factor(group1)) group1 = as.factor(group1)
    nlev =  nlevels(group1)
    # cat("\nnlev in table0",nlev); print(depvar,group1)
    desc = by(depvar, group1, cusum.default)  # has right number of rows!
    endloop = nlev + ifelse (doAll, 1, 0)
    #print(desc)
    for (i in 1:endloop) { # deal with missing groups in 2way
      if (is.null(unlist(desc[i]))) desc[i] = c(0)
    }
  }
  if (is.factor(depvar)) {
    nrownna = ifelse(brief,1,2)
    df = data.frame(V1 = rep(0,nrownna+nlevels(depvar)),stringsAsFactors = F)
    if (brief) rownames(df) = c("Nhave",levels(depvar)) # no NA row if brief
    else rownames(df) = c("Nhave","NA's",levels(depvar))
    for (i in 1:endloop) { # check normality, also summarize all (doAll or no group1)
      if (i<=nlev) uldesb = unlist(desc[i])
      else {descall = cusum.default(depvar); uldesb = unlist(descall)} # doAll
      if (brief) {
        for (j in 1:nlevels(depvar)) {uldesb[j+1] = uldesb[j+2]}
        uldesb = uldesb[1:length(uldesb)-1] 
      }
      # print(uldesb)
      df[i]= uldesb
    }
    normTF = NULL; pnormin = NULL
  }
  else { # not factor
    pnormin = 1
    for (i in 1:endloop) { # check normality, also summarize all (doAll or no group1)
      if (i<=nlev) uldesb = unlist(desc[i])
      else { # doAll
        descall = cusum.default(depvar); uldesb = unlist(descall)
        if (!is.null(group1) && (length(uldesb) > nrowmax-3)) uldesb = uldesb[1:(nrowmax-3)]
        # print(uldesb) # above, no p-values if group1
      }
      if ((i<=nlev) || is.null(group1)) { # no norm test if group1
        pvalch = uldesb[nrowmax]
        # cat ("\ni pvalch",i,"'",pvalch,"'"); print(uldesb)
        if (!is.na(pvalch) && !is.null(pvalch) && pvalch!=" ") 
          if (regexpr("<",pvalch) > 0) pnormin = 0 # assumed to be <0.001
          else pnormin = min(pnormin,as.numeric(pvalch))
        # cat("i",i,pnormin)
      }
    }
    normTF = pnormin >= pnorm
    nrowdf = ifelse(brief,ifelse(sdsamerow,2,3), ifelse(pnormin<1,nrowmax,nrowmax-3))
    #cat("\nnrowdf,pnormin,pnorm,normTF:",nrowdf,pnormin,pnorm,normTF)
    df = data.frame(V1=rep(0,nrowdf),stringsAsFactors = F)
    # print(df)
    for (i in 1:endloop) {
      if (i<=nlev) uldesc = unlist(desc[i])
      else uldesc = uldesb # from cusum above
      if (nrowdf>length(uldesc)) uldesc=c(uldesc, rep(" ",nrowdf-length(uldesc))) # assuming it is 12 long
      # cat("\n",i,"\n"); print(uldesc)
      if (!brief) df[i]= uldesc  # apparently ok if uldesc is shorter
      else {
        if (normTF) {
          if (!sdsamerow) df[i]= c(uldesc[1],uldesc[irmean],uldesc[irmean+1])
          else df[i] = c(uldesc[1], paste(uldesc[irmean], "\u00B1", 
                                          trimws(uldesc[irmean+1]), sep=""))
        }
        else {
          iqrange = paste("(",trimws(uldesc[irmed-1]),chariqr,trimws(uldesc[irmed+1]),")",sep="")
          if (!sdsamerow) df[i]= c(uldesc[1],uldesc[irmed],iqrange)
          else df[i] = c(uldesc[1], paste(uldesc[irmed],iqrange, sep=""))
        }
      }
      # print(df)
    }
    if (!brief) {
      rownam3 = c("Nhave","NA's","Mean","SD","CV%","SEM","Min.","1st_Q","Median","3rd_Q","Max.","Skew","Kurt.")
      if (nrowdf==nrowmax-3) rownames(df)=rownam3
      else rownames(df)=c(rownam3,"Skew_p","Kurt_p","norm_?")
    }
    else {  # no NA row if brief
      if (sdsamerow) {
        if (normTF) rownames(df)=c("Nhave",paste("Mean","\u00B1","SD", sep=""))
        else rownames(df)=c("Nhave","Median(IQR)")
      }
      else {
        if (normTF) rownames(df)=c("Nhave","Mean","SD")
        else rownames(df)=c("Nhave","Median","IQR")
      }
    }
  }
  # cat ("\ntable0 pre-return,df:\n"); print(df)
  return (list(df, normTF, pnormin))
}
