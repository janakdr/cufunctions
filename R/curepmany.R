#' Does repeated measures one- or two-way anova and post-hoc tests (format wide or long - 1/many lines for each subject)
#' @param dsgiven required: dataset
#' @param dnamf required: char-string name of first variable being analyzed
#' @param dnaml required: char-string name of last variable being analyzed
#' @param repnam required: char-string name of repeated grouping factor
#' @param fac2 char-string of name of 2nd, non-repeated grouping factor (default NULL)
#' @param idnam (to indicate long format) =char-string of name of ID/Subject variable (default NULL)
#' @param minimal = TRUE (default), F or FALSE to get all console output and graphs
#' @param interact =TRUE (default), FALSE for no interaction
#' @param partialF =TRUE (default), FALSE for no partial F test
#' @param cov =NULL (default) covariate model string
#' @param ytrans ="none" (default) to transform depvar: "sqrt" "log"
#' @param maxlines =0 (default) for no line plot, 1 for line plots
#' @param mainx =1 (default), 2 to interchange the two factors
#' @param sumdiff =1 (default) or -1 for difference summary
#' @param scale ="frequency" (default) or "percent"
#' @param scatter ="yes" (default) for scatter plots
#' @param ebars =1 (default)/2/3/4 for error bar type
#' @param dots =0 (default), 1 to display data on graph
#' @param barcolor ="black" (default) for bar outline color
#' @param barfill =NULL (default) for bar fill color
#' @param plot ="bar" (default) for bar graphs
#' @param ordinal =NULL (default) to treat as ordinal
#' @param g1order =NULL (default) to reorder bars
#' @param g2order =NULL (default) to reorder second factor
#' @param difforder =NULL (default) to reorder differences
#' @param depname =NULL to override dependent variable name
#' @param g1name =NULL to override first factor name
#' @param g2name =NULL to override second factor name
#' @param title =NULL to override title
#' @param chariqr =":" character to separate quartiles
#' @param psigcld =0 (no letters)/x for CLD letters on bars
#' @param conf.int =0.95 (default) confidence interval width
#' @param ddepn =NULL dependent variable difference name
#' @param shape =16 (default) dot shape in regression
#' @param caption =NULL (default) caption text
#' @param pvpairs ="std" (default) p-value pairs to show
#' @param pvypos =NULL (default) position of p-value lines
#' @param pvstinc =0.05 (default) p-value line increment
#' @param pvlab ="p" (default) p-value label format
#' @param pvprefix ="p=" (default) prefix to p-values
#' @param pvsize =NULL (default) size of p-values
#' @param chpvref ="ref" (default) reference char
#' @param pvspill =FALSE (default) allow p-value spill
#' @param pnosig =0.2 (default) threshold for non-significant
#' @param psignif =0.05 (default) significance threshold
#' @param p2stars =0.01 (default) two stars threshold
#' @param p3stars =0.001 (default) three stars threshold
#' @param p4stars =0.0001 (default) four stars threshold
#' @param pvnshide =TRUE (default) hide non-significant p-values
#' @param pvtipl =0.01 (default) p-value line tip length
#' @param normvisit =0 (default) reference visit for normalization
#' @param normway ="fold" (default) normalization method
#' @param needvar =1 (default) number of other variables for rmwall
#' @param linetype ="n" (default) line type
#' @param linecolor ="black" (default) line color
#' @param linesize =1 (default) line thickness
#' @param theme ="bw" (default) plot theme
#' @param legend ="top" (default) legend position
#' @param size =NULL point/outline size
#' @param width =NULL box width
#' @param yscale ="none" (default) y-axis scale
#' @param xangle =NULL x-axis label angle
#' @param yangle =NULL y-axis label angle
#' @param orientation ="vertical" (default) plot orientation
#' @param posd =NULL bar spacing adjustment
#' @param binwfac =30 (default) bin width factor
#' @param dotsize =NULL dot size
#' @param dotshape =NULL dot shape
#' @param dotcolor =NULL dot color
#' @param fontmain =NULL main (title) font
#' @param fontxname =NULL x-axis name font
#' @param fontyname =NULL y-axis name font
#' @param fontxticks =NULL x-axis tick label font
#' @param fontyticks =NULL y-axis tick label font
#' @param axiscolor =NULL axis color
#' @param tickcolor =NULL tick color
#' @param axisthick =NULL axis thickness
#' @param tickthick =NULL tick thickness
#' @param ticklength =NULL tick length
#' @param xticks.by =NULL x-tick spacing
#' @param yticks.by =NULL y-tick spacing
#' @param dodredge =TRUE (default) run dredge
#' @param usemod =1 (default) use nth best dredge model
#' @param nmodshow =16 (default) number of top models to show
#' @param nverscat =2 (default) vertical scatter panels
#' @param nhorscat =2 (default) horizontal scatter panels
#' @param ifdeb =FALSE (default) debug output
#' @param m.min =1 (default) smallest model size in dredge
#' @param m.max =99 (default) largest model size in dredge
#' @param fixed =NULL variables that must be in model
#' @param subset =as.expression(1) model subset expression
#' @param ftype =NULL output file type
#' @param fname =NULL output file name prefix
#' @param fscale =NULL output file scale
#' @param fwidth =NULL output file width
#' @param fheight =NULL output file height
#' @param dpi =300 (default) resolution in dpi
#' @param remove =NULL elements to remove from plot
#' @return nothing
#' @examples
#' \dontrun{
#' # These examples write CSV files to the working directory
#' curepmany(delta, "TC", "TG", "Diet")  # one line per subject; TG.xx TG.yy etc as names of TG data
#' # with sex as second factor, Diet*sex interaction in model
#' curepmany(delta, "TC", "TG", "Diet", "sex")
#' curepmany(delta, "TC", "TG", "Diet", "sex", interact=FALSE)  # no Diet*sex interaction
#' curepmany(delta, "TC", "TG", "Diet", "sex", cov="age+sex*age")  # with covariate adjustment
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
      message(ivar," ",namlist[ivar]," has different size ",
          length(repout)," not ",lenrep)
    repmat[ivar,] = repout
  }
  # print(repmat)
  if (is.null(repout)) message("No results. Only one visit?")
    else utils::write.csv(repmat,file=fname, row.names=F)
  if (normvisit > 0) {
    filenorm = paste(normway,".",fname,sep="")
    utils::write.csv(rmwall,file=filenorm, row.names=F)
    message("Transformed data in ",filenorm)
  }
}
