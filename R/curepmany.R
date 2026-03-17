#' Does repeated measures one- or two-way anova and post-hoc tests (format wide or long - 1/many lines for each subject)
#' @param dsgiven ,dnamf,dnaml,repnam required: dataset, char-strings of names of first&last variable being analyzed, repeated grouping factor
#' @param fac2 char-string of name of 2nd, non-repeated grouping factor (default NULL)
#' @param idnam (to indicate long format) =char-string of name of ID/Subject variable (default NULL)
#' @param minimal = TRUE (default), F or FALSE to get all console output and graphs
#' @return nothing
#' @examples
#' \dontrun{
#' curepmeas(delta,"TC","TG","Diet")  # one line per subject; TG.xx TG.yy etc as names of TG data
#' curepmeas(delta,"TC","TG","Diet", "sex")  # with sex as second factor, Diet*sex interaction in model
#' curepmeas(delta,"TC","TG","Diet", "sex", interact=F)  # no Diet*sex interaction
#' curepmeas(delta,"TC","TG","Diet", "sex", cov="age+sex*age")  # with covariate adjustment
#' }
#' @export
curepmany = function(dsgiven, dnamf, dnaml, repnam, fac2=NULL, idnam=NULL, minimal=T,
                     interact=TRUE, partialF=TRUE, cov=NULL, ytrans="none",
                     maxlines=0, mainx=1, sumdiff=1, scale="frequency", scatter="yes",
                     ebars=1, dots=0, barcolor="black", barfill=NULL, plot="bar",
                     ordinal=NULL, g1order=NULL, g2order=NULL, difforder=NULL, 
                     depname=NULL, g1name=NULL, g2name=NULL, title=NULL, chariqr=":",
                     psigcld=0, conf.int=0.95, ddepn=NULL, shape=16, caption=NULL,
                     pvpairs="std", pvypos=NULL, pvstinc=0.05, pvlab="p", 
                     pvprefix="p=", pvsize=NULL, chpvref="ref", pvspill=F,
                     pnosig=0.2, psignif=0.05, p2stars=0.01, p3stars=0.001, p4stars=0.0001,
                     pvnshide=T, pvtipl=0.01, 
                     normvisit=0, normway="fold", needvar=1,
                     linetype="n", linecolor="black", linesize=1, theme="bw",
                     legend="top", size=NULL, width=NULL, yscale="none",
                     xangle=NULL, yangle=NULL, orientation="vertical",
                     posd=NULL, binwfac=30, dotsize=NULL, dotshape=NULL, dotcolor=NULL,
                     fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                     fontxticks=NULL, fontyticks=NULL,
                     axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                     ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                     dodredge=T, usemod=1, nmodshow=16,
                     nverscat=2, nhorscat=2, ifdeb=F,
                     m.min=1, m.max=99, fixed=NULL, subset=as.expression(1),
                     ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                     fheight=NULL,dpi=300,remove=NULL) 
{
  cuf_apply_defaults(match.call(), environment())
  if (dnamf == dnaml) stop("\nfirst and last variables same:",
    '"',dnamf,'"  "',dnaml,'"\nIf only one, just run curepmeas')
  ncols = length(dsgiven); namlist = c(); rmwall = NULL
  if (!is.null(idnam)) dvardot = dnamf # long format input
  else {dvardot = paste(dnamf,".",sep=""); dvarnc1 = nchar(dnamf)+1}
  for (i in 1:ncols) {
    if (!is.null(idnam)) dvartest = names(dsgiven[i])
      else dvartest = substr(names(dsgiven[i]),1,dvarnc1)
    # cat("\ndvardot,test,namlist:",dvardot,",",dvartest,",",namlist[1],">")
    if (dvartest==dvardot)
      if (is.null(namlist)) {
        namlist = c(dnamf) #; cat("\nnamlist:",namlist)
        if (!is.null(idnam)) dvardot = dnaml
        else {dvardot = paste(dnaml,".",sep=""); dvarnc1 = nchar(dnaml)+1}
      }
      else {namlist = c(namlist,dnaml); break}
    else if (!is.null(namlist)) { #between first and last
      if (!is.null(idnam)) dnami = names(dsgiven[i])
      else {
        dnami = names(dsgiven[i])
        if((jdot=cu_index(dnami,".")) > 0) dnami = substr(dnami,1,jdot-1)
      }
      if (!(dnami %in% namlist)) namlist = c(namlist,dnami)
      # cat("\ndnami,namlist",dnami,",",namlist,">\n")
    }
  }
  if (is.null(namlist)) 
    stop("\nNo variable name begins with <",dvardot,"> Check and re-run\n", sep="")
  nvartodo = length(namlist)
  # cat("\nnvartodo,namlist",nvartodo,namlist)
  if (namlist[nvartodo] != dnaml)
    stop("\nNo variable name begins with <",dnaml,".> Check and re-run\n", sep="")
  if (is.character(fname)) fname = paste(fname,"-",sep="")
  else fname = ""
  fname = paste(fname,"curepmany.csv",sep="")
  for (ivar in 1:nvartodo) {
    repmret = curepmeas(dsgiven, namlist[ivar], repnam, fac2=fac2, idnam=idnam, minimal=T,
     interact=interact, partialF=partialF, cov=cov, ytrans=ytrans,
     maxlines=maxlines, mainx=mainx, sumdiff=sumdiff, scale=scale, scatter=scatter,
     ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill, plot=plot,
     ordinal=ordinal, g1order=g1order, g2order=g2order, difforder=difforder, 
     depname=depname, g1name=g1name, g2name=g2name, title=title,
     psigcld=psigcld, conf.int=conf.int, ddepn=ddepn, shape=shape, caption=caption,
     pvpairs=pvpairs, pvypos=pvypos, pvstinc=pvstinc, pvlab=pvlab, 
     pvprefix=pvprefix, pvsize=pvsize, chpvref=chpvref, pvspill=pvspill,
     pnosig=pnosig, psignif=psignif, p2stars=p2stars, p3stars=p3stars, p4stars=p4stars,
     pvnshide=pvnshide, pvtipl=pvtipl, 
     normvisit=normvisit, normway=normway, rmwall=rmwall, needvar=needvar,
     linetype=linetype, linecolor=linecolor, linesize=linesize, theme=theme,
     legend=legend, size=size, width=width, yscale=yscale,
     xangle=xangle, yangle=yangle, orientation=orientation,
     posd=posd, binwfac=binwfac, dotsize=dotsize, dotshape=dotshape, dotcolor=dotcolor,
     fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
     fontxticks=fontxticks, fontyticks=fontyticks,
     axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
     ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
     dodredge=dodredge, usemod=usemod, nmodshow=nmodshow,
     nverscat=nverscat, nhorscat=nhorscat, ifdeb=ifdeb,
     m.min=m.min, m.max=m.max, fixed=fixed, subset=subset,
     ftype=ftype,fname=fname,fscale=fscale,fwidth=fwidth,
     fheight=fheight,dpi=dpi,remove=remove)
    repout = repmret[[1]]; rmwall = repmret[[2]]
    #if (ivar==1) repomat = data.frame()
    #repomat[ivar,] = repout
    # print(repout)
    if (ivar==1) {
      lenrep = length(repout)
      repmat = matrix(nrow=nvartodo, ncol=lenrep)
      colnames(repmat) = names(repout)
      #repmat = repout
      ifdeb = F
    }
    else 
      if (lenrep != length(repout)) 
      cat("\n",ivar,namlist[ivar],"has different size",
          length(repout),"not",lenrep,"\n")
    repmat[ivar,] = repout
  }
  # print(repmat)
  if (is.null(repout)) cat("\nNo results. Only one visit?")
    else write.csv(repmat,file=fname, row.names=F)
  if (normvisit > 0) {
    filenorm = paste(normway,".",fname,sep="")
    write.csv(rmwall,file=filenorm, row.names=F)
    cat ("\nTransformed data in",filenorm)
  }
}
