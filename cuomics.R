cuomics = function(funcname, dsgiven, dnamf, dnaml, fnom, ...)
{
  #' Analyzes multiple outcomes by specified function name
  #' @param funcname: "1way","2way","cov1way","cov2way","scatter","repmeasl","repmeasw"
  #' @param dsgiven ,dnamf,dnaml required: dataset, char-strings of names of first&last variable being analyzed
  #' @param fnom = string as prefix for output file cuomics.csv
  #' @param ... remaining arguments for chosen function
  #' @return nothing
  #' @examples
  #' cuomics("1way", NEJM, "tcstudy", "hcchange", "tchc1w", Diet)
  #' cuomics("2way", NEJM, "tcstudy", "hcchange", "tchc2w", Diet, sex)
  #' cuomics("cov1way", NEJM, "tcstudy", "hcchange", "tchccov1", tcpre, Diet)
  #' cuomics("cov2way", NEJM, "tcstudy", "hcchange", "tchccov2", tcpre, Diet, sex)
  #' cuomics("scatter", NEJM, "tcstudy", "hcchange", "tchcscat", tcpre)
  #' cuomics("repmeasw", delta, "TC", "like", "tclikerep", "Diet", "sex")
  #' @export
  funcpos = c("1way", "2way","cov1way","cov2way","scatter","repmeasl","repmeasw")
  if (!is.character(funcname)) stop ("function name must be in quotes")
  if (substr(funcname,1,2)=="cu") funcname = substr(funcname,3,10)
  ifun = 1; nfuncpos = length(funcpos)
  while (ifun<=nfuncpos) {
    if (funcname == funcpos[ifun]) break
    ifun = ifun+1
  }
  if (ifun > nfuncpos) stop ("function name must be one of ",funcpos[1]," ",
    funcpos[2]," ",funcpos[3]," ",funcpos[4]," ",funcpos[5]," ",funcpos[6]," ",
    funcpos[7])
  if (dnamf == dnaml) stop("\nfirst and last variables same:",
    '"',dnamf,'"  "',dnaml,'"\nIf only one, just run', funcname)
  # print(fnom)
  fnom = paste(fnom,ifelse(fnom == "","","-"),"cuomics.csv",sep="")
  # print(fnom)
  ncols = length(dsgiven); namlist = c()
  rmwall = NULL # used by only repmeas
  if (ifun < nfuncpos) dvardot = dnamf # not repmeas wide 
  else {dvardot = paste(dnamf,".",sep=""); dvarnc1 = nchar(dnamf)+1}
  for (i in 1:ncols) {
    if (ifun < nfuncpos) dvartest = names(dsgiven[i])
    else dvartest = substr(names(dsgiven[i]),1,dvarnc1)
    # cat("\ndvardot,test,namlist:",dvardot,",",dvartest,",",namlist[1],">")
    if (dvartest==dvardot)
      if (is.null(namlist)) {
        ilocb = i-1 # used by all but repmeas
        namlist = c(dnamf) #; cat("\nnamlist:",namlist)
        if (dnamf == dnaml) {cat("\nRunning",funcname,"just once:\n"); break}
        if (ifun < nfuncpos) dvardot = dnaml
        else {dvardot = paste(dnaml,".",sep=""); dvarnc1 = nchar(dnaml)+1}
      }
    else {namlist = c(namlist,dnaml); break}
    else if (!is.null(namlist)) { #between first and last
      if (ifun < nfuncpos) dnami = names(dsgiven[i])
      else {
        dnami = names(dsgiven[i])
        if((jdot=cu_index(dnami,".")) > 0) dnami = substr(dnami,1,jdot-1)
      }
      if (!(dnami %in% namlist)) namlist = c(namlist,dnami)
      # cat("\ndnami,namlist",dnami,",",namlist,">\n")
    }
  }
  # cat ("\ndvardot,dvartest,i,namlist",dvardot,"'",dvartest,"'",i,namlist)
  if (is.null(namlist)) 
    stop("\nNo variable name begins with <",dvardot,"> Check and re-run\n", sep="")
  nvartodo = length(namlist)
  # cat("\nnvartodo,namlist",nvartodo,namlist)
  if (namlist[nvartodo] != dnaml)
    stop("\nNo variable name begins with <",dnaml,".> Check and re-run\n", sep="")
  for (ivar in 1:nvartodo) { # test if continuous, skip if not??
    depname = namlist[ivar]
    if (ifun==1) repout = cu1way(dsgiven[,ilocb+ivar], ..., depname=depname,minimal=T)
    else if (ifun==2) repout = cu2way(dsgiven[,ilocb+ivar], ..., depname=depname,minimal=T)
    else if (ifun>=nfuncpos-1) {
      repmret = curepmeas(dsgiven, depname, ..., rmwall=rmwall, minimal=T)
      repout = repmret[[1]]; rmwall = repmret[[2]]
    }
    else stop("\nneed code for ifun=",ifun," '",funcname,"'",sep="")
    if (ivar==1) {
      lenrep = length(repout)
      repmat = matrix(nrow=nvartodo, ncol=lenrep)
      colnames(repmat) = names(repout)
      #repmat = repout
    }
    else 
      if (lenrep != length(repout)) 
        cat("\n",ivar,namlist[ivar],"has different size",
            length(repout),"not",lenrep,"\n")
    # print(repout); print(lenrep); print(repmat)
    # for (j in 1:lenrep) repmat[ivar,j] = repout[2,j]
    repmat[ivar,] = repout
    # print(repmat)
  }
  #print(repmat)
  if (ifun>=nfuncpos-1) {
    if (is.null(repout)) cat("\nNo results. Only one visit?")
    else write.csv(repmat,file=fnom, row.names=F)
    if (normvisit > 0) {
      filenorm = paste(normway,".",fnom,sep="")
      write.csv(rmwall,file=filenorm, row.names=F)
      cat ("\nTransformed data in",filenorm)
    }
  }
  else write.csv(repmat, file=fnom, row.names=F)

}