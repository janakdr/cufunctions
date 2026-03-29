#' Volcano plot
#'
#' @param dsomics data frame from cuomics
#' @param gpairs list of group pairs to be compared
#' @param xdash =0.5 (default) to locate dashed vertical lines
#' @param psig =0.05 (default) for p-value threshold of interest
#' @param pfactor =1 (default) for factor to divide psig by for multiple comparison correction
#' @param psigmin =0.2 (default) for upper limit of p-values to display
#' @param foldmin =NULL (default) for left end of log2(FoldChange) (usually set to -2)
#' @param foldmax =NULL (default) for right end of log2(FoldChange) (usually set to 2)
#' @param ylim =5 (default) for upper limit of graph (-log10(pval))
#' @examples
#' cuvolcano(volcomic, c("lean", "obese"))
#' @export
cuvolcano = function(dsomics, gpairs, xdash=0.5, psig = 0.05, psigmin = 0.2, pfactor=1, 
                  foldmin=NULL, foldmax=NULL, ylim=5, titlend="Volcano Plot",
                  ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                  fheight=NULL, dpi=300, remove=NULL, suff=NULL) {
  cuf_apply_defaults(match.call(), environment())
  locategp = function(namegp) {
    teststr = paste(namegp,"_M",sep=""); lenv = nchar(teststr)
    for (jc in 2:ncolom) {
      #cat("\n",jc,substr(colnames(dsomics)[jc],1,lenv),"")
      if (substr(colnames(dsomics)[jc],1,lenv) == teststr) return(jc)
    }
    stop(teststr," not found in dsomics. Check and rerun")
  }
  locatepv = function() {
    teststr = paste(gp1,"vs",gp2,sep=""); teststsp = paste(gp1,".vs.",gp2,sep="")
    #cat ("\n'",teststr,"'",teststsp,"'",sep="")
    for (jc in ncolom:2) { # loop backwards since p-values are at end
      if ((colnames(dsomics)[jc]==teststr) || (colnames(dsomics)[jc]==teststsp))
         return(jc)
      #cat("\n",jc,"'",colnames(dsomics)[jc],"'")
    }
    stop(teststr," not found in dsomics. Tell Sekhar")
  }
  nvars = nrow(dsomics); ncolom = ncol(dsomics)
  ds = data.frame(A = dsomics[,1], stringsAsFactors = F)
  nvolc = length(gpairs)/2
  if (2*nvolc != length(gpairs)) stop("List of group pairs is",length(gpairs),
                                     ". Must be even")
  for (jvolc in 1:nvolc) {
    gp1 = gpairs[2*jvolc-1]; gp2 = gpairs[2*jvolc]
    jgp1 = locategp(gp1); jgp2 = locategp(gp2); jpv = locatepv()
    namepval = paste(gp1,"vs",gp2)
    #ds$C= dsomics[,jpv]
    for (iv in 1:nvars) {
      oldw <- getOption("warn"); options(warn = -1)
      val1 = as.numeric(dsomics[iv,jgp1]); val2 = as.numeric(dsomics[iv,jgp2])
      pv=ifelse(grepl("ns",dsomics[iv,jpv]), 1, as.numeric(dsomics[iv,jpv]))
      options(warn = oldw)
      if (is.na(val1) || is.na(val2) || is.na(pv) || val1<=0 || val2<=0) {
        cat("\n",namepval,":",ds[iv,1],"bad:",
            dsomics[iv,jgp1],dsomics[iv,jgp2],dsomics[iv,jpv])
        rat = 1; pv = 1
      }
      else rat = val2/val1 
      ds[iv,2] = rat; ds[iv,3] = pv
    }
    if (jvolc==1) colnames(ds) = c("varnam","fold","pval")
    # print(ds)
    volc = cuvolc(ds,xdash=0.5, psig = psig, pfactor=pfactor, 
           foldmin=foldmin, foldmax=foldmax,
           psigmin = psigmin, ylim=ylim, title = paste(namepval,titlend))
    # print(volc)
    cu_plout(volc,"volcano", suff=suff, ftype=ftype, fname=fname, scale=fscale,
    width=fwidth, height=fheight, dpi=dpi, remove=remove)
  }
 }
