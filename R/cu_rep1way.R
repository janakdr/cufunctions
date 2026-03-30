#' Does repeated measures one-way anova and post-hoc tests (long-format - multiple lines for each subject)
#' @param ddep,dgp,dsub, depvar , groupvar, Subject  (3 required)
#' @param itrans type of transformation if any
#' @param minimal = FALSE (default), T or TRUE to suppress all output (for cuomics)
#' @param ebars =0 (default to be 1 or 4)/1/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR) or -N (nonparm if any norm fail)
#' @param dots =0 (default), 1 to display data on graph
#' @param barcolor for outline color, barfill for fill color. Use fill="colors" for colors by group.
#' @param barfill ="colors" (default) for colors by group ("lancet" for 2 factors, other journal options "aaas", "jco", "uchicago", "npg"
#' @param plot ="bar" (default) for bar graphs; "box" "violin" "rod" "no"
#' @param order =NULL (default)/c("...") to reorder bars
#' @param psigcld =0 (no letters)/x for CLD letters on bars (any shared letter means P>psigcld, 0 for no letters)
#' @param conf.int =0.95 (default)/x/0 for confidence interval width of contrast estimates (0 for none)
#' @param ddepn =NULL (default) for name of dependent variable difference
#' @param pnorm =0.05 (default) threshold for normality test
#' @param depname /g1name/title to override names of depvar, groupvar, title
#' @param legend ="top" (default), "bottom", "right", "left" to locate legend
#' @param suff =NULL (default) set to desired suffix
#' @param pvpairs="std" (default)/"all"/i/c(ij,ik,...) to show std or all or vs.i or pval's of i/j, i/k ...
#' @param pvypos=NULL (default)/position of pval lines
#' @param pvstinc=0.05 (default)/increment for pvypos from one pval line to next
#' @param pvlab="p"/"*" to display numerical or asterisks
#' @param pvprefix="p=" (default)/"" etc prefix to p-values
#' @param pvsize=NULL (default 7,3.5)/size of p-values or **
#' @param chpvref="ref" (default) char above i-th group bar when pvpairs=i
#' @param pvspill=F/T to not allow p-values to spill outside range or to allow
#' @param pnosig =0.2 (default)/x for threshold to show p-values even if nonsigificant
#' @param psignif =0.05 (default)/x for threshold to significance
#' @param p2stars =0.01 (default)/x for threshold to two stars
#' @param p3stars =0.001 (default)/x for threshold to three stars
#' @param p4stars =0.0001 (default)/x for threshold to four stars
#' @param pvnshide=T (default)/F to hide NS p-values or not
#' @param pvtipl=0.01 (default)/length of p-value line tips
#' @param refmean =NA (default)/ref.val to compare each group mean with
#' @param caption =NULL (default)/"yes"/"caption text" to get caption ("yes" to list n's)
#' @param linetype ="n" (default)/x for no connecting lines ("dashed" "dotted")
#' @param linecolor ="black" (default)/x for black lines ("red" etc)
#' @param linesize =1 (default)/x for line thickness
#' @param theme ="bw" (default)/x for white background (or "gray" or "classic" (no grid lines))
#' @param size numeric value (e.g. size=1), to change size of points and outlines
#' @param width numeric value between 0 and 1 specifying box width
#' @param yscale (default="none"), can be "log2", "log10", "sqrt"
#' @param xangle /yangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param orientation (default="vertical"), can change to "horizontal"
#' @param posd =NULL (default) set to values around 0.9 to fine-tune group2 bar spacing
#' @param binwfac =NULL (default 30) set to fraction of range within which points will be binned
#' @param dotsize =NULL (default 1) set to fraction of binwidth for dot size
#' @param dotshape =NULL (default)
#' @param dotcolor =NULL (default "white") set to dot color
#' @param fontmain =c(14,"bold","black") default, change for title, 0 for not title 
#' @param fontxname .fontyname,fontxticks,fontyticks = c(12,"plain","black") default, 0 to suppress
#' @param axiscolor ,tickcolor="black" (default)/x for axis/tick color
#' @param axisthick ,tickthick=0.5 (default)/x for axis/tick thickness
#' @param ticklength =1 (default)/x for tick length in mm
#' @param xticks.by ,yticks.by =NULL (default)/s for x/y tick spacing by s
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return one line of summary means/SDs and p-values
#' @examples
#' # Internal function, not exported; called via curepmeas()
#' \dontrun{
#' cu_rep1way(TG, Diet, ID)
#' }
cu_rep1way = function(ddep,dgp,dsub,depvar, groupvar, Subject, itrans, minimal=F,
                     ebars=0, dots=0, barcolor="black", barfill="colors", plot="bar",
                     order=NULL, psigcld=0, conf.int=0.95, ddepn=NULL, pnorm=0.05,
                     depname=NULL, g1name=NULL, title=NULL, legend="top", suff=NULL,
                     pvpairs="std", pvypos=NULL, pvstinc=0.05, pvlab="p", 
                     pvprefix="p=", pvsize=NULL, chpvref="ref", pvspill=F,
                     pnosig=0.2, psignif=0.05, p2stars=0.01, p3stars=0.001, p4stars=0.0001,
                     pvnshide=T, pvtipl=0.01, refmean=NA,
                     caption=NULL, linetype="n", linecolor="black", linesize=1, theme="bw",
                     size=NULL, width=NULL, yscale="none",
                     xangle=NULL, yangle=NULL, orientation="vertical",
                     posd=NULL, binwfac=30, dotsize=NULL, dotshape=NULL, dotcolor=NULL,
                     fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                     fontxticks=NULL, fontyticks=NULL,
                     axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                     ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                     ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                     fheight=NULL,dpi=300,remove=NULL) 
{
  cuf_apply_defaults(match.call(), environment())
  if(is.null(depname)) depname = deparse(substitute(depvar))
  if(is.null(g1name)) g1name = deparse(substitute(groupvar))
  if (is.character(groupvar)) groupvar = as.factor(groupvar)
  else if (is.character(depvar)) depvar = as.factor(depvar)
  ivar = deparse(substitute(Subject))
  levnams=levels(groupvar); nlev=nlevels(groupvar); ndiff = nlev*(nlev-1)/2
  zeros = nlev
  depnact = ifelse(itrans==2,substr(depname,7,nchar(depname)-1), depname)
  if(is.null(ddepn))
  ddepn = paste(depnact," difference",ifelse(itrans==2," %",""),sep="")
  # print(depvar); print(groupvar) #; options(warn = 1)
  dds = data.frame(ddep,dgp)
  ddfn = cu_table0(ddep, dgp, brief=F, doAll=F)
  ddf = ddfn[[1]]; normTF = ddfn[[2]]; pnormin = ddfn[[3]]
  # cat("\nnormTF, pnormin",normTF,pnormin)
  colnames(ddf)=levels(dgp)
  ds = data.frame(depvar,groupvar)
  df = cu_table0(depvar, groupvar, brief=F, doAll=F)[[1]]
  colnames(df)=levnams
  if (!minimal) {
    cat("\n",ddepn,"\n")
    print(ddf)
    cat(depname,"\n")
    print(df)
    cat("If normality testing is done, p-value is displayed only if it is <0.1\n")
    if (ebars !=4) {
      pnormin = 1; nfail = 0; nrowmax = 16
      for (j in 1:ndiff) { # logic from table0
        pvalch = ddf[nrowmax,j]
        if (!is.na(pvalch) && !is.null(pvalch) && pvalch!=" ") {
          if (regexpr("<",pvalch) > 0) 
          {pnormin = 0; nfail = nfail+1} # < means < 0.001
          else if ((pval=as.numeric(pvalch))<pnorm) 
          {pnormin = min(pnormin,pval); nfail = nfail+1}
          # cat ("\n",j," '",pvalch,"' ",pnormin,sep="")
        }
      }
      if (nfail > 0) { # concerned only with normality of within-subj changes
        cat("\nDATA FAIL NORMALITY TEST IN",nfail,"OF",ndiff,"GROUP Differences.",
            "SMALLEST P-VALUE",ifelse(pnormin==0,"<0.001",pnormin))
        if (ebars<=0) {ebars=4; cat("\nNONPARAMETRIC ANALYSIS WILL BE DONE.\n")}
        else cat("\nLOOK FOR DATA ERRORS IN 'Min' AND 'Max' VALUES.",
                 "\nIF DATA ARE NOT NORMAL,",
                 "\nYOU SHOULD USE THE NONPARAMETRIC DUNN TEST (ebars=4)\n")
      }
      else if (ebars<=0) ebars = min(3,max(1,-ebars))
    }
  }
  lenrep = 2+nlev*(3*nlev+1)/2
  if (!is.na(refmean)) {
    lenrep = lenrep+nlev; pval1ts = rep(-1,nlev)
    sumsqr = 0; ndft = 0
    for (ig1 in 1:nlev) {
      if ((ndfi <- as.numeric(df[1,ig1])-as.numeric(df[2,ig1])-1) > 0)
      {ndft = ndft+ndfi; sumsqr = sumsqr+ndfi*as.numeric(df[4,ig1])**2}
      # cat ("\n",ig1,ndfi,ndft,sumsqr)
    }
    poolsd = sqrt(sumsqr/ndft) #; cat("\npoolsd",poolsd)
    for (ig1 in 1:nlev) {
      if ((ni1 <- as.numeric(df[1,ig1])-as.numeric(df[2,ig1])) > 0) {
        tstat = abs(as.numeric(df[3,ig1])-refmean)*sqrt(ni1)/poolsd
        pval1ts[ig1] = 2 * stats::pt(-tstat,ndft)
        # cat ("\n",ig1,ni1,tstat)
      }
      else pval1ts[ig1] = NA
    }
    # print(pval1ts)
  }
  repout = rep(-1,lenrep); repn = rep("??",lenrep) #; print(repout)
  repout[1] = depname; repn[1] = "Variable"; jrepo = 2
  for (ig1 in 1:nlev) {
    repout[jrepo] = df[3,ig1]; repout[jrepo+1] = df[4,ig1] #mean sd
    repnam = levnams[ig1]
    repn[jrepo] = paste(repnam,"_mean",sep="")
    repn[jrepo+1] = paste(repnam,"_sd",sep="")
    jrepo = jrepo+2
  }
  iga = 1; igb = 2
  for (jdi in 1:ndiff) {
    repout[jrepo] = ddf[3,jdi]; repout[jrepo+1] = ddf[4,jdi] #mean sd
    repnam = paste(levnams[igb],"-",levnams[iga],sep="")
    repn[jrepo] = paste(repnam,"_mean",sep="")
    repn[jrepo+1] = paste(repnam,"_sd",sep="")
    igb = igb+1; if (igb > nlev) {iga = iga+1; igb = iga+1}
    jrepo = jrepo+2
  }
  if (!is.na(refmean)) for (ig1 in 1:nlev) {
    repout[jrepo] = ifelse (pval1ts[ig1]<=pnosig, round(pval1ts[ig1],3), " ")
    repn[jrepo] = paste(levnams[ig1]," v.",round(refmean,0),sep="")
    # cat("\n",repn[jrepo],repout[jrepo])
    jrepo = jrepo+1
  }
  # cat("\nrepout:\n"); print(repout)
  nlevm1 = nlev-1; pvalues = rep(-1,nlevm1*nlevm1); ipo = -1
  if (!minimal) cat("\n")
  if (ebars != 4) {
    fit = lme(depvar ~ groupvar, random=~1|Subject/groupvar, na.action=na.omit)
    coeffs = "(Intercept)"
    for (i in 2:nlev) {coeffnam = paste(g1name,levnams[i],sep=""); coeffs = c(coeffs,coeffnam)}
    # print(fit$coefficients$fixed); print(coeffs)
    names(fit$coefficients$fixed) = coeffs
    #names(fit$coefficients$random) = c(rvar,ivar)
    fit$call$fixed[2]=depname; fit$call$fixed[3]=g1name
    sumlm = summary(fit)
    if (!minimal) {
      cat("\nRepeated measures anova: lme(",depname," ~ ",g1name,
          ", random=~1|Subject/",g1name,", na.action=na.omit)\n",sep="")
      print(sumlm)
      cat("\n",depname,": ",g1name," comparisons\n\n",sep="")
    }
    for (i in 1:nlevm1) {
      for (j in (i+1):(nlev)) {
        vec=c(rep(0,zeros)); vec[j] = 1; if (i > 1) vec[i] = -1
        if (!minimal) cat(levnams[j], "minus", levnams[i])
        estpv = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
        pvalues[ipo+j] = estpv
        repout[jrepo] = ifelse (estpv <= pnosig, signif(estpv,3), " ")
        repn[jrepo] = paste("p(",levnams[i],"vs",levnams[j],")",sep="")
        jrepo = jrepo+1
      }
      ipo = ipo+nlevm1
    }
    repout[jrepo] = ifelse(pnormin<=pnorm,pnormin," "); repn[jrepo] = "pnormin"
    # cat("\nrepout:\n"); print(repout)
    #car::plot(Effect(c(g1name), fit))
    #car::plot(fit, resid(., type = "p") ~ fitted(.), abline = 0)
    if (!minimal) 
    cat ("\nSummary of p-values by repeated measures anova, using Fisher's LSD")
  }
  else {
    if (anyNA(depvar)) {
      cat("\nSome missing data, so no overall p-value by Friedman.")
      dss = data.frame(Subject, depvar,groupvar)
      # print(dss)
      rmw = stats::reshape(dss, direction="wide", idvar="Subject",timevar="groupvar", v.names="depvar")
      # print(rmw)
      for (i in 1:nlevm1) {
        for (j in (i+1):(nlev)) {
          #print(dss[,i+1]); cat("\nj"); print(dss[,j+1])
          pvalues[ipo+j] = stats::wilcox.test(rmw[,i+1], rmw[,j+1], paired=T, exact=F)$p.value
        }
        ipo = ipo+nlevm1
      }
    }
    else {
      fried = stats::friedman.test(depvar ~ groupvar | Subject)
      fried$data.name = paste(depname,"~",g1name, "| Subject")
      print (fried)
      for (i in 1:nlevm1) { #missing data: "'x' and 'y' must have the same length"
        for (j in (i+1):(nlev)) {
          pvalues[ipo+j] = stats::wilcox.test(depvar[groupvar==levnams[i]], 
            depvar[groupvar==levnams[j]], paired=T, exact=F)$p.value
        }
        ipo = ipo+nlevm1
      }
    }
    cat ("\nSummary of p-values by Wilcoxon signed rank test with continuity correction")
  }
  names(repout) = repn #; print(repout)
  if (minimal) return(repout)
  cat("\n",depname," compared across repeats of ",g1name,"\n",sep="")
  cu_pvalmatout(pvalues,levnams)
  if (!is.na(refmean)) cat ("\nComparing each group mean to reference mean of",
    refmean,"\n",pval1ts)
  #  cat("\n",order)
  mapord = cu_mapord(order,levnams,nlev)
  #  cat("\n",order)
  letbar = cu_letbar(pvalues,psigcld,mapord,nlev)
  cat("\n")
  #print(pvalues)
  pvstruc = cu_pvline(df,pvpairs,pvalues,nlev,1,pvlab,pnosig,psignif,p2stars,p3stars,p4stars,
                      letbar,pvypos,pvnshide,pvspill,chpvref,ebars,dots,yscale)
  #print(pvstruc)
  pvdf = pvstruc[[1]]; pvypos = pvstruc[[2]]; pvlab = pvstruc[[3]]
  yrange = pvstruc[[4]]; letbar = pvstruc[[5]]
  #cat("\npvdf,pvypos\n"); print(pvdf); print(pvypos)
  if (itrans==2) {
    #cat("\nddep:\n"); print(ddep)
    for (i in 1:length(depvar)) if (!is.na(depvar[i])) depvar[i] = 10**depvar[i]
    #cat("\nddep:\n"); print(ddep)
    yrange = 10**yrange; yscale = "log10"; ebars = 4  # to get median and iqr
  }
  else if (yscale=="log10") ebars = 4  # to get median and iqr
  if (is.null(pvtipl)) pvtipl = yrange*5e-5
  if (is.null(title)) title=paste(depnact, "at repeats of", g1name)
  dtitl = paste(ddepn,"s between repeats of ",g1name,sep="")
  p <- cu_plot1("curepmeas", ddep,dgp, order, g1name, ddepn, dtitl,
                plot=plot, linetype=linetype, linecolor=linecolor, linesize=linesize, theme=theme,
                letbar=letbar, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                pvdf=NULL,pvypos=pvypos,pvstinc=pvstinc,pvlab=pvlab,pvprefix=pvprefix,
                pvsize=pvsize, pvnshide=pvnshide,pvtipl=pvtipl,
                caption=caption, legend=legend, size=size, width=width,
                yscale="none",xangle=xangle, yangle=yangle, dotshape=dotshape, 
                orientation="vertical", binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
                fontxticks=fontxticks, fontyticks=fontyticks,
                axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
                ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
                suff=suff, ftype=ftype, fname=fname, fscale=fscale,
                fwidth=fwidth, fheight=fheight, dpi=dpi, remove=remove)
  p <- cu_plot1("curepmeas", depvar,groupvar, order, g1name, depnact, title,
                plot=plot, linetype=linetype, linecolor=linecolor, linesize=linesize, theme=theme,
                letbar=letbar, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                pvdf=pvdf,pvypos=pvypos,pvstinc=pvstinc,pvlab=pvlab,pvprefix=pvprefix,
                pvsize=pvsize, pvnshide=pvnshide,pvtipl=pvtipl,
                caption=caption, legend=legend, size=size, width=width,
                yscale=yscale,xangle=xangle, yangle=yangle, dotshape=dotshape, 
                orientation="vertical", binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
                fontxticks=fontxticks, fontyticks=fontyticks,
                axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
                ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
                suff=suff, ftype=ftype, fname=fname, fscale=fscale,
                fwidth=fwidth, fheight=fheight, dpi=dpi, remove=remove)
}
