#' Does one-way analysis of covariance (ancova), all contrasts, and a graph
#' @param depvar required: dep-var, covariate, grouping factor
#' @param covar see depvar
#' @param group1 see depvar
#' @param xs optional, c(x1,x2,..) to model with interaction and do contrasts at x1,x2,...
#' @param breakpt =F (default) or value at which covar slope changes
#' @param ebars =1 (default)/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR)
#' @param dosimpler =T (default) or F to do or not do cu1way first
#' @param partialF =TRUE (default), F or FALSE for no partial F vs simpler models
#' @param plotcons ="yes" (default), "plot" for just plot, "cons" for just console
#' @param depname to override names of depvar, covar, group1, title
#' @param covname see depname
#' @param g1name see depname
#' @param title see depname
#' @param caption ='' (default) or set to string to show at bottom right
#' @param g1order =NULL (default)/c("...") to reorder group1
#' @param conf.int =0.95 (default) for confidence interval width of contrast estimates (0 for none)
#' @param color ="color" (default) for color graph, "black" for black
#' @param padj ="none"(default), p-value adjustment for multiple comparisons, can be "bonferroni", "holm")
#' @param pbart =0.05 (default)/x for Bartlett test threshold for variance homogeneity
#' @param pnorm =0.05 (default)/x for normality test threshold
#' @param chariqr ="," (default)/x for character to separate quartiles ("-" etc)
#' @param legend (default="top"), can be "bottom", "right", "left"
#' @param xangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param yangle see xangle
#' @param orientation (default="vertical"), can change to "horizontal"
#' @param shape =16 (default closed circle) (see http://www.sthda.com/english/wiki/ggplot2-point-shapes)
#' @param dotcolor ="black" (default) for point symbol color (="red" ="blue" etc)
#' @param dotsize =2 (default) or x to set size of point symbols
#' @param linetype ="solid"(default)/x for solid line ("solid" "dashed" "dotted" "blank" "longdash" "dotdash" "twodash")
#' @param linesize =1 (default)/x for line thickness
#' @param linecolor ="red" (default)/x for line color
#' @param theme ="bw" (default)/x for white background ("classic" (no grid lines),"linedraw" "gray" "minimal" "void")
#' @param fontfamily ="sans" (default), can be "serif" "mono" 
#' @param xmin =NA (default) or value to start/end x/y-axis
#' @param xmax see xmin
#' @param ymin see xmin
#' @param ymax see xmin
#' @param fontmain =c(14,"bold","black") default, change for title, 0 for not title 
#' @param fontxname = c(12,"plain","black") default, 0 to suppress
#' @param fontyname see fontxname
#' @param fontxticks see fontxname
#' @param fontyticks see fontxname
#' @param axiscolor ="black" (default)/x for axis/tick color
#' @param tickcolor see axiscolor
#' @param axisthick =0.5 (default)/x for axis/tick thickness
#' @param tickthick see axisthick
#' @param ticklength =1 (default)/x for tick length in mm
#' @param xticks.by =NULL (default)/s for x/y tick spacing by s
#' @param yticks.by see xticks.by
#' @param titlejust ="center" (default) or "left" or "right"
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale =NULL(default) or set to numerical value
#' @param fwidth see fscale
#' @param fheight see fscale
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @param binwidth =NULL (default) for binwidth
#' @param axiscolor ="black" (default)/x for axis color
#' @param legheadsize =12 (default) for font size of legend heading
#' @param legtextsize =10 (default) for the font size of legend text
#' @return returns nothing
#' @examples
#' attach(NEJM)
#' cucov1way(tcstudy, tcpre, Diet)  # for regression, plot, contrasts (parallel lines, no interaction)
#' # for regression, plot, contrasts at tcpre=150,220 (non-parallel lines)
#' cucov1way(tcstudy, tcpre, Diet, c(150, 220))
#' detach(NEJM)
#' @export
cucov1way = function(depvar, covar, group1, xs=NULL, breakpt=F,
                     ebars=1, dosimpler=T, partialF=TRUE, plotcons="yes",
                     depname=NULL, covname=NULL, g1name=NULL, title=NULL, caption="",
                     g1order=NULL, conf.int=0.95, color="color", 
                     padj="none", pbart=.05, pnorm=.05, chariqr=",", 
                     legend="top", xangle=NULL, yangle=NULL, orientation="vertical", binwidth=NULL,
                     shape=16, dotcolor="black", dotsize=2, 
                     linetype="solid", linesize=1, linecolor="red",
                     theme="bw", fontfamily="sans",
                     xmin=NA, ymin=NA, xmax=NA, ymax=NA,
                     fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                     fontxticks=NULL, fontyticks=NULL,
                     axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                     ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                     titlejust="center", legheadsize=12, legtextsize = 10,
                     ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                     fheight=NULL, dpi=300, remove=NULL) {
  cuf_apply_defaults(match.call(), environment())
  toomanycov = function(namegp) {stop(namegp,
     " cannot be a group variable as numeric with ",nlev," levels.",
     "\nIf it is the continuous covariate, it should be the 2nd argument.",
     "\nWith >1 continuous covariate, use culinreg.")
  }
  partfout = function(fitlm,charstr) {
    ndf1 = fitlm$df; rse1 = stats::sigma(fitlm)
    pval = cupartialF(rse1,ndf1,rsez,ndfz)
    cat("\np=",signif(pval,3)," vs model with ",charstr,sep="")
    return(pval)
  }

  ifplot = T; ifcons = T
  if (plotcons != "yes") if (plotcons=="plot") ifcons = F
  else if (plotcons=="cons") ifplot = F
  else cat ("\nno good plotcons:",plotcons,". only 'yes' 'plot' or 'cons'")
  depsubdep = deparse(substitute(depvar)); depsubcov = deparse(substitute(covar))
  depsubg1 = deparse(substitute(group1))
  if (is.null(depname)) depname=depsubdep; if (is.null(covname)) covname = depsubcov
  if (is.null(g1name)) g1name=depsubg1; # must precede as.factor
  if (!is.numeric(depvar)) stop(depsubdep,
                                " cannot be the dependent variable, which should be numeric.",
                                "\n   If you mean it, try cu1way, cu2way or culogist")
  if (!is.numeric(covar)) stop(depsubcov,
                               " cannot be the continuous covariate, which should be numeric.")
  if (is.numeric(group1)) {
    nlev = nlevels(as.factor(group1)); if (nlev > 9) toomanycov(depsubg1)
  }
  if (!is.null(xs) && !is.numeric(xs)) stop("\n",deparse(substitute(xs)),
      "??? cucov1way is for only one factor. Did you intend cucov2way?\n")
  if (length(ebars)>1) stop("\n",deparse(substitute(ebars)),
    "??? cucov1way is for only one factor. Did you intend cucov2way?\n")
  if (length(depvar) != length(group1) || length(depvar) != length(covar)) {
    cat("\n#observations of dependent variable, covariate and group factors not equal:\n",
        length(depvar), length(covar), length(group1),"\n\n")
    stop("\nquitting")
  }
  if (is.null(fontmain)) fontmain=c(14,"bold","black")
  if (is.null(fontxname)) fontxname=c(12,"bold","black")
  if (is.null(fontyname)) fontyname=c(12,"bold","black") 
  if (is.null(fontxticks)) fontxticks=c(12,"bold","black")
  if (is.null(fontyticks)) fontyticks=c(12,"bold","black")
  if (is.null(axiscolor)) axiscolor="black"
  if (is.null(tickcolor)) tickcolor="black"
  if (is.null(axisthick)) axisthick=0.5
  if (is.null(tickthick)) tickthick=0.5
  if (is.null(ticklength)) ticklength=1
  group1 = cu_reorder(group1, g1order)
  nlev1=nlevels(group1)
  # if (nlev1 > nlevmax) stop(paste("# levels in factor 1 is",nlev1,". If serious, set nlevmax>=",nlev1))
  levnams=levels(group1)
  if (ifcons) if (dosimpler & !dosimpler) { # work on this later
    cat("\nAnalyzing (just as point of reference) without controlling for",covname,"\n")
    cutable1(depvar,group1,compare=T,brief=T,
             depname=depname,g1name=g1name,plot="no",doAll=F)
  }
  else {
    df = cu_table0(depvar, group1, brief=F, doAll=T)[[1]]
    colnames(df)=c(levels(group1),"All")
    if (ifcons) {cat(depname,"\n"); print(df, right = F); cat("\n")} # why ifcons?
  }
  if (is.null(title)) title = paste(depname,"vs",covname,
                                    "at",nlev1,"levels of", g1name)
  if (ifcons) cat(depname,"compared across",nlev1,g1name,
      "groups controlling for",covname,"\n\n")
  coeffs = c("(Intercept)",covname) 
  for (i in 2:nlev1) { # not used except if broken, bug if some i not in eqn
    coeffnam = paste(g1name,levnams[i],sep="")
    coeffs = c(coeffs,coeffnam)
  }
  
  if (is.numeric(breakpt)) if ((breakpt<=min(covar)) || (breakpt>=max(covar)))
    stop("\nbreakpt must be between ",min(covar)," and ",max(covar))
  else {
    covnew = covar-breakpt; ifhigh = as.character(covnew>0); broken = TRUE
#    print(ifhigh)
  }
  else broken = FALSE

  strcall = paste("lm(formula = ",depname," ~ ",covname,"+",g1name,sep="")
  if (broken) {
    strcall = paste(strcall," + ifhigh:",covname,sep="")
    coeffs[2] = paste(covname,"-",breakpt,sep="")
    coeffs = c(coeffs,paste("ifhigh:(",covname,"-",breakpt,")",sep=""))
  }
  if (is.null(xs)) {
    sets=1; zeros=nlev1+1
    if (broken) {
      zeros = zeros+1
      fit = stats::lm(depvar ~ covnew+group1+ifhigh:covnew, na.action="na.exclude")
    }
    else fit = stats::lm(depvar ~ covar+group1, na.action="na.exclude")
  }

  else {
    sets=length(xs); zeros=nlev1*2
    if (broken) {
      zeros = zeros+1
      fit = stats::lm(depvar ~ covnew+group1+ifhigh:covnew+covnew*group1, na.action="na.exclude")
    }
    else fit = stats::lm(depvar ~ covar+group1+covar*group1, na.action="na.exclude")
    strcall = paste(strcall," + ",covname,"*",g1name,sep="")
    for (i in 2:nlev1) {   # not used except if broken
      coeffnam = paste(covname,":",g1name,levnams[i],sep="")
      coeffs = c(coeffs,coeffnam)
    }
  }
  coeffs = c("(Intercept)",covname)
  coefp = paste(covname,":",g1name,sep="")
  for (i in 3:length(fit$coefficients)) {
    fitcnam = names(fit$coefficients)[i]
    coeffs = c(coeffs, ifelse (grepl(":",fitcnam), 
               paste(coefp,substr(fitcnam,13,20),sep=""),
               paste(g1name,substr(fitcnam,7,14),sep="")))
  }
  #print(fit); cat("\ncoeffs:",coeffs)
  fit$call = paste(strcall,")",sep="")
  names(fit$coefficients) = coeffs
  sumlm = summary(fit)
  nlev11 = nlev1-1; nlev11sq = nlev11*nlev11
  if (ifcons) {
    print(sumlm)
    if (!is.null(xs)) {
      cat("Testing Slopes within",g1name,"levels\n")
      p.values = rep(-1,nlev11sq)
      for (j in 1:nlev1) {
        vec=c(rep(0,zeros)); cat(levnams[j])
        vec[2] = 1; if (j>1) vec[nlev1+j] = 1
        cu_estout(fit,vec,conf.int=conf.int)
      }
      cat("\nComparing Slopes between",g1name,"levels\n")
      for (i in 1:nlev11) {
        for (j in (i+1):(nlev1)) {
          vec=c(rep(0,zeros)); vec[nlev1+j] = 1; 
          if (i>1) vec[nlev1+i] = -1
          cat(levnams[j], " minus ", levnams[i],sep="")
          p.values[(i-1)*nlev11+j-1] = cu_estout(fit,vec,conf.int=conf.int)
        }
      }
      cat ("\nSummary of p-values of slope comparisons\n")
      cu_pvalmatout(p.values,levnams); cat("\n")
    }
    cat("\n",g1name," comparisons\n\n",sep=""); p.values = rep(-1,sets*nlev11sq)
    for (k in 1:sets) {
      for (i in 1:nlev11) {
        for (j in (i+1):(nlev1)) {
          vec=c(rep(0,zeros)); vec[j+1] = 1; if (i>1) vec[i+1] = -1
          if (!is.null(xs)) {
            x=xs[k]; vec[j+nlev1] = x; if (i>1) vec[i+nlev1] = -x
          }
          cat(levnams[j], " minus ", levnams[i],sep="")
          if (!is.null(xs)) {cat(" ","at ",covname,"=", x,sep="")}
          p.values[(k-1)*nlev11sq+(i-1)*nlev11+j-1] = cu_estout(fit,vec,conf.int=conf.int)
        }
      }
    }
    cat ("\nSummary of p-values by ancova and post-hoc t\n")
    p.value = rep(-1,nlev11sq); ipvk = 0
    for (k in 1:sets) {
      for (ipv in 1:nlev11sq) {ipvk=ipvk+1; p.value[ipv]=p.values[ipvk]}
      if (!is.null(xs)) cat("\nAt ",covname," = ",xs[k],":\n",sep="")
      cu_pvalmatout(p.value,levnams)
    }
    cat("\nEach p-value compares group above vs to left (adjustment method: none)\n")
    if (partialF) {
      cat("\nPartial F-test vs simpler models:")
      ndfz = fit$df; rsez = stats::sigma(fit)
      if (is.null(xs)) pvalss = 0
      else {
        fitss = stats::lm(depvar ~ covar+group1, na.action="na.exclude")
        pvalss = partfout(fitss,paste("same slope - cucov1way(",
                                      depsubdep,",",depsubcov,",",depsubg1,")",sep=""))
      }
      fitg1 = stats::lm(depvar ~ group1, na.action="na.exclude")
      pval1 = partfout(fitg1, paste("just ",g1name," - cu1way(",depsubdep,",",
                                    depsubg1,")",sep=""))
      fitco = stats::lm(depvar ~ covar, na.action="na.exclude")
      pvalco = partfout(fitco, paste("just ",depsubcov," - lm(",depsubdep," ~ ",
                                     depsubcov,")",sep=""))
      if (pvalss > 0.05 || pval1 > 0.05|| pvalco > 0.05)
        cat("\nModel is overly complex. Consider a simpler model")
      cat("\n")
    }
  }
  if (ifplot) {
    ycalc = stats::predict(fit)
    # cat("\n5:",ycalc[5],"'")
    datf = stats::na.omit(data.frame(A=depvar, B=group1, C=covar, D=ycalc))
    #for (i in 1:nrow(datf)) ptshape[i] ifelse
    #  p <- ggplot(datf, aes(x=datf$C, y=datf$A, group=datf$B)) + 
    #    geom_point(aes(y=datf$A),shape=datf$B) + geom_line(aes(y=datf$D))
    if (color == "black")
      p <- ggplot(datf, aes(datf[,3],shape=datf[,2],linetype=datf[,2])) + 
      geom_point(aes(y=datf[,1]),color="black") + labs(shape=g1name) + 
      geom_line(aes(y=datf[,4])) + labs(linetype=g1name)
    else {
      if (is.logical(shape) && shape) shape=datf[,2]
      p <- ggplot(datf, aes(datf[,3],color=datf[,2])) + 
        geom_point(aes(y=datf[,1]),shape=shape,size=dotsize) + labs(colour=g1name) + 
        geom_line(aes(y=datf[,4]),linetype=linetype, linewidth=linesize)
    } # what follows is same as in cov2way but for name in cu_plout
    if (is.null(theme)) theme = "bw"
    if (!(theme %in% c("grey","gray","bw","linedraw","light","dark","minimal","classic","void"))) {
      cat("\ntheme='",theme,"' no good. Taken to be 'bw'",sep=""); theme = "bw"
    }
    funtheme = get(paste("theme_",theme,sep=""))
    p <- p + labs(x=covname, y=depname) + funtheme()
    # above must come before ggtitle, else jus is ineffective
    if (title!="") {
      if (is.null(titlejust)) jus = 0.5
      else if (is.numeric(titlejust)) jus = titlejust
      else if (titlejust=="left") jus = 0
      else if (titlejust=="right") jus = 1
      else jus = 0.5
      p <- p + ggtitle(title) + theme(plot.title = element_text(hjust=jus))
    }
    p <- p + 
      theme(axis.line = element_line(color=axiscolor,linewidth=axisthick),
            axis.ticks= element_line(color=tickcolor,linewidth=tickthick), # color ignored
            axis.ticks.length=unit(ticklength,"mm"),
            legend.title = element_text(size=legheadsize),
            legend.text = element_text(size=legtextsize))
    p <- ggpar(p, xlim=c(xmin,xmax), ylim=c(ymin,ymax), font.family=fontfamily,
               font.main = fontmain, font.x = fontxname, font.y = fontyname,
               font.xtickslab = fontxticks, font.ytickslab = fontyticks,
               xticks.by=xticks.by, yticks.by=yticks.by,
               x.text.angle=xangle, y.text.angle=yangle, caption=caption)
    if (ifcons) cu_plout(p,"cucov1way",ftype=ftype, fname=fname,scale=fscale,
             width=fwidth,height=fheight, dpi=dpi, remove=remove)
  }
  if (!ifcons) {
    slope1 = sumlm$coefficients[2,1]; slopes = c(slope1)  #; pvals = ??
    int1 = sumlm$coefficients[1,1]; ints = c(int1)
    if (nlev1>1) for (j in 2:nlev1) {  # bug if any level has no slope
      slopes = c(slopes,slope1+sumlm$coefficients[nlev1+j,1])
      ints = c(ints,int1+sumlm$coefficients[1+j,1])
    }
    #print(slopes,ints)
    retm = NULL; retm$A = p; retm$B = slopes; retm$C = ints
    retm$D = sumlm$adj.r.squared
    return(retm) 
  }
}
