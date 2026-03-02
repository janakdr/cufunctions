#' Does two-way analysis of covariance (ancova), all contrasts, and a graph
#' @param depvar ,covar, group1, group2 required: dep-var, covariate, grouping factors 1 & 2
#' @param xs optional, c(x1,x2,..) to model with interaction and do contrasts at x1,x2,...
#' @param interact =TRUE (default), F or FALSE for no group1*group2 interaction
#' @param dosimpler =T (default) or F to do or not do cu2way first
#' @param partialF =TRUE (default), F or FALSE for no partial F vs simpler models
#' @param depname /covname/g1name/g2name/title to override names of depvar, covar, group1, group2, title
#' @param caption ='' (default) or set to string to show at bottom right
#' @param g1order =NULL (default) to reorder group1 (1st Factor)
#' @param g2order =NULL (default) to reorder group2 (2nd Factor)
#' @param conf.int =0.95 (default) for confidence interval width of contrast estimates (0 for none)
#' @param shape (default=closed circle): numeric value from 0 to 25 [rest below for cu2way]
#' @param ebars =1 (default)/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR)
#' @param dots =0 (default), 1 to display data on graph
#' @param color ="color" (default) for color graph, "black" for black
#' @param padj ="none"(default), p-value adjustment for multiple comparisons, can be "bonferroni", "holm")
#' @param pbart =0.05 (default)/x for Bartlett test threshold for variance homogeneity
#' @param pnorm =0.05 (default)/x for normality test threshold
#' @param chariqr ="," (default)/x for character to separate quartiles ("-" etc)
#' @param legend (default="top"), can be "bottom", "right", "left"
#' @param xangle /yangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param orientation (default="vertical"), can change to "horizontal"
#' @param posd NULL (default) set to values around 0.9 to fine-tune group2 bar spacing
#' @param shape =16 (default closed circle) (see http://www.sthda.com/english/wiki/ggplot2-point-shapes)
#' @param dotcolor ="black" (default) for point symbol color (="red" ="blue" etc)
#' @param dotsize =2 (default) or x to set size of point symbols
#' @param linetype ="solid"(default)/x for solid line ("solid" "dashed" "dotted" "blank" "longdash" "dotdash" "twodash")
#' @param linesize =1 (default)/x for line thickness
#' @param linecolor ="red" (default)/x for line color
#' @param theme ="bw" (default)/x for white background ("classic" (no grid lines),"linedraw" "gray" "minimal" "void")
#' @param fontfamily ="sans" (default), can be "serif" "mono" 
#' @param xmin ,xmax,ymin,ymax =NA (default) or value to start/end x/y-axis 
#' @param fontmain =c(14,"bold","black") default, change for title, 0 for not title 
#' @param fontxname .fontyname,fontxticks,fontyticks = c(12,"plain","black") default, 0 to suppress
#' @param axiscolor ,tickcolor="black" (default)/x for axis/tick color
#' @param axisthick ,tickthick=0.5 (default)/x for axis/tick thickness
#' @param ticklength =1 (default)/x for tick length in mm
#' @param xticks.by ,yticks.by =NULL (default)/s for x/y tick spacing by s
#' @param titlejust ="center" (default) or "left" or "right"
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return returns nothing
#' @examples
#' \dontrun{
#' cucov2way (tcstudy, tcpre, Diet, sex)
#' cucov2way (tcstudy, tcpre, Diet, sex, interact=F)
#' cucov2way (tcstudy, tcpre, Diet, sex, c(150,220))
#' cucov2way (tcstudy, tcpre, Diet, sex, interact=F, c(150,220))
#' }
#' @export
cucov2way = function(depvar, covar, group1, group2, xs=NULL, 
                     interact=T, dosimpler=T, partialF=TRUE,
                     depname=NULL, covname=NULL, g1name=NULL, g2name=NULL, title=NULL, caption="",
                     g1order=NULL, g2order=NULL, conf.int=0.95,
                     ebars=1, dots=0, color="color", 
                     padj="none", pbart=.05, pnorm=.05, chariqr=",", 
                     legend="top", xangle=NULL, yangle=NULL, orientation="vertical", posd=NULL,
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
  macmap = function() {
    mapinto12 = c(rep(-1,nlevint)) #; map12toin = c(rep(-1,nlevboth12))
    for (i in 1:nlevboth12) {
      j12 = match(g12names[i], ginnames)
      if (is.na(j12)) stop ("Can't understand",g12names[i],"missing")
      mapinto12[j12] = i #; map12toin[i] = j12
    }
    i = 0; g12new = c("no")
    for (j12 in 1:nlevint) { #g12names redefined to go with ginnames
      if (mapinto12[j12]>0) # reset mapinto12 just to skip missing combins
      {i = i+1; g12new[i] = ginnames[j12]; mapinto12[j12] = i}
    }
    retm = NULL; retm$A = g12new; retm$B = mapinto12 #retm$C=map12toin
    return(retm) 
  }
  toomanycov = function(namegp) {stop(namegp,
     " cannot be a group variable as numeric with ",nlev," levels.",
     "\nIf it is the continuous covariate, it should be the 2nd argument.",
     "\nWith >1 continuous covariate, use cumreg.")
  }
  partfout = function(fitlm,charstr) {
    ndf1 = fitlm$df; rse1 = sigma(fitlm)
    pval = cupartialF(rse1,ndf1,rsez,ndfz)
    cat("\np=",signif(pval,3)," vs model with ",charstr,sep="")
    #cat(ndf1,rse1,rse1^2*ndf1)
    return(pval)
  }

  depsubdep = deparse(substitute(depvar)); depsubcov = deparse(substitute(covar))
  depsubg1 = deparse(substitute(group1)); depsubg2 = deparse(substitute(group2))
  # cat("\ngroup2:",depsubg2)
  if (is.null(depname)) depname=depsubdep; if (is.null(covname)) covname = depsubcov
  if (is.null(g1name)) g1name=depsubg1; # must precede as.factor
  if (is.null(g2name)) g2name=depsubg2
  if (!is.numeric(depvar)) stop(depsubdep,
      " cannot be the dependent variable, which should be numeric.",
      "\n   If you mean it, try cu1way, cu2way or culogist")
  if (!is.numeric(covar)) stop(depsubcov,
      " cannot be the continuous covariate, which should be numeric.")
  if (regexpr("c(",depsubg2,fixed=T) > 0) stop(depsubg2,
        " cannot be the second factor. Did you mean to run cucov1way?")
  if (is.numeric(group1))
    {nlev = nlevels(as.factor(group1)); if (nlev > 9) toomanycov(depsubg1)}
  if (is.numeric(group2)) 
    {nlev = nlevels(as.factor(group2)); if (nlev > 9) toomanycov(depsubg2)}
  # print(group1); print(group2) # need logic to test valid order's
  if (length(depvar) != length(group1) || length(depvar) != length(covar) 
      || length(depvar) != length(group2)) {
    cat("\n#observations of dependent variable, covariate and group factors not equal:\n",
        length(depvar), length(covar), length(group1), length(group2),"\n")
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
  group1 = cu_reorder(group1, g1order); g1order = NULL
  group2 = cu_reorder(group2, g2order); g2order = NULL
  g1names = levels(group1); g2names = levels(group2);
  nlev1 = nlevels(group1); nlev2 = nlevels(group2);
  levnams=levels(group1)
  anyxmiss = F  # getting rid of subjects with any missing, not like 1way
  for (i in 1:length(depvar)) { 
    if (is.na(group1[i]) || is.na(group2[i])|| is.na(depvar[i])|| is.na(covar[i])) {
      if (anyxmiss) cmiss = c(cmiss,i)
      else {cmiss = c(i); anyxmiss = T}
    }
  }
  if (anyxmiss) {
    cat("\nDropping observations",cmiss,"with missing data\n")
    group1 = group1[-cmiss]; group2 = group2[-cmiss]
    depvar = depvar[-cmiss]; covar  = covar[-cmiss]
  }
  groupint = interaction(group1, group2, lex.order=T, sep="&")
  groupvar = factor(paste(group1, group2, sep="&"))
  ginnames = levels(groupint); nlevint = nlevels(groupint)
  g12names = levels(groupvar); nlevboth12 = nlevels(groupvar)
  # print(ginnames); print(g12names)
  macret = macmap()
  g12names = macret$A; mapinto12 = macret$B # ; map12toin= macret$C
  # print(ginnames); print(g12names)
  groupvar = factor(groupvar, levels=g12names)
  # print(levels(groupvar))
  # cat("\n"); print(mapinto12) #; cat("\n"); print(map12toin)
  #print(group1); print(group2); print(groupvar)
  nameboth12 = paste(g1name,"&",g2name,sep="")
  if (dosimpler) {
    cat("\nAnalyzing (just as point of reference) without controlling for",covname)
    cu2way(depvar,group1,group2,interact=interact,minimal=T,
           depname=depname,g1name=g1name,g2name=g2name,pnorm=pnorm)
  }
  else {
    df = cu_table0(depvar, groupvar, brief=F, doAll=T)[[1]]
    colnames(df)=c(g12names,"All")
    cat(depname,"\n")
    print(df)
  }
  if (is.null(title)) title=paste(depname, " vs ",covname, 
      "for",nlevboth12,"levels of", nameboth12)
  cat("\n",depname,"compared across",nlevboth12,nameboth12,
      "groups controlling for",covname,"\n\n")
  nlev11 = nlev1-1; nlev21 = nlev2-1; ninter = nlev11*nlev21

  coeffs = c("(Intercept)",covname) # below loads g1names and g2names into coeffs
  # cat ("\n1,2",nlev1,nlev2,coeffs)
  for (i1 in 2:nlev1) {coeffs = c(coeffs,paste(g1name,g1names[i1],sep=""))}
  # cat ("\n1",coeffs)
  for (i2 in 2:nlev2) {coeffs = c(coeffs,paste(g2name,g2names[i2],sep=""))}
  # cat ("\n2",coeffs)
  strcall = paste("lm(",depname," ~ ",covname,"+",g1name,"+",g2name,sep="")
  zeros = nlev1+nlev2; end1v = zeros
  if (is.null(xs)) {
    sets=1;  endcov1f = end1v; endcov2f = end1v
    if (interact) {
      zeros = zeros + ninter; end2f = zeros
      strcall = paste(strcall,"+",g1name,"*",g2name,sep="")
      fit = lm(depvar ~ covar+group1+group2+group1*group2, na.action="na.exclude")
    }
    else fit = lm(depvar ~ covar+group1+group2, na.action="na.exclude")
  }
  else {
    sets=length(xs); zeros=zeros +nlev11; endcov1f = zeros; endcov2f = zeros
    strcall = paste(strcall," + ",covname,"*",g1name,sep="")
    for (i in 2:nlev1) {
      coeffnam = paste(covname,":",g1name,g1names[i],sep="")
      coeffs = c(coeffs,coeffnam)
    } # above loads covariate*g1names
    if (interact) {
      zeros = zeros + 2*ninter + nlev21
      endcov2f = endcov1f+nlev21; end2f = endcov2f + ninter
      strcall = paste(strcall,"*",g2name,sep="")
      for (j in 2:nlev2) {
        coeffnam = paste(covname,":",g2name,g2names[j],sep="")
        coeffs = c(coeffs,coeffnam)
      } # above loads covariate*g2names
      fit = lm(depvar ~ covar+group1+group2+covar*group1*group2, na.action="na.exclude")
    }
    else fit = lm(depvar ~ covar+group1+group2+covar*group1, na.action="na.exclude")
  }
  if (interact) {
    nse = 1; if (!is.null(xs)) nse = 2
    covnamecolon = ""
    for (k in 1:nse) {
      for (j in 2:nlev2) {
        coeffnam2 = paste(":",g2name,g2names[j],sep="")
        for (i in 2:nlev1) {
          coeffnam = paste(covnamecolon,g1name,g1names[i],coeffnam2,sep="")
          coeffs = c(coeffs,coeffnam)
        }
      }
      covnamecolon = paste(covname,":",sep="")
    }
  }
  fit$call = paste(strcall,")",sep="")
  # print(names(fit$coefficients)); print(coeffs)
  names(fit$coefficients) = coeffs
  sumlm = summary(fit)
  print(sumlm)

  nlev11sq = nlev11*nlev11
  if (!is.null(xs) && !interact) {
    cat("Testing Slopes within",g1name,"levels\n")
    p.values = rep(-1,nlev11sq)
    for (j in 1:nlev1) {
      vec=c(rep(0,zeros)); cat(levnams[j])
      vec[2] = 1; if (j>1) vec[nlev1+nlev21+j] = 1
      cu_estout(fit,vec,conf.int=conf.int)
    }
    cat("\nComparing Slopes between",g1name,"levels\n")
    for (i in 1:nlev11) {
      for (j in (i+1):(nlev1)) {
        vec=c(rep(0,zeros)); vec[nlev1+nlev21+j] = 1; 
        if (i>1) vec[nlev1+nlev21+i] = -1
        cat(levnams[j], " minus ", levnams[i],sep="")
        p.values[(i-1)*nlev11+j-1] = cu_estout(fit,vec,conf.int=conf.int)
      }
    }
    cat ("\nSummary of p-values of slope comparisons\n")
    cu_pvalmatout(p.values,levnams); cat("\n")
  }
  if (interact) {
    pvminint = 1
    cat(g1name," comparisons within each ",g2name," group\n",sep="")
    if (sets>1) cat("followed by ",g2name," comparisons within and between ",g1name," groups\n\n",sep="")
    for (k in 1:sets) {
      if (!is.null(xs)) x=xs[k]
      for (i2 in 1:nlev2) {  # factor 1 effects at each level of factor 2
        cat(g2names[i2],"\n");
        if (i2>1) fac2zi = endcov2f + (i2-2)*nlev11
        for (i1 in 1:nlev11) {  # comparing factor 1 levels
          for (j1 in (i1+1):nlev1) {
            vec = c(rep(0,zeros)); vec[j1+1] = 1; if (i1 > 1) vec[i1+1] = -1
            if (i2 > 1) {vec[fac2zi+j1-1] = 1; if (i1 > 1) vec[fac2zi+i1-1] = -1}
            cat(g1names[j1],"minus",g1names[i1])
            if (!is.null(xs)) {
              vec[end1v+j1-1] = x; if (i1 > 1) vec[end1v+i1-1] = -x
              if (i2 > 1) {vec[ninter+fac2zi+j1-1] = x; if (i1 > 1) vec[ninter+fac2zi+i1-1] = -x}
              cat(" at ",covname,"=", x,sep="")
            }
            cu_estout(fit, vec, conf.int=conf.int)
          }
        }
        cat("\n")
      }
      if (sets==1) cat(g2name," comparisons within and between ",g1name," groups\n\n",sep="")
      for (i2 in 1:nlev21) {  # comparing factor 2 levels
        if (i2>1) fac2zi = endcov2f + (i2-2)*nlev11
        for (j2 in (i2+1):nlev2) {
          cat(g2names[j2],"minus",g2names[i2],"\n")
          fac2zj = endcov2f + (j2-2)*nlev11
          for (i1 in 1:nlev1) {        # at each level of factor 1
            vec = c(rep(0,zeros)); vec[nlev1+j2] = 1; if (i2 > 1) vec[nlev1+i2] = -1
            if (i1>1) {vec[fac2zj+i1-1] = 1; if (i2 > 1) vec[fac2zi+i1-1] = -1}
            cat(g1names[i1])
            if (!is.null(xs)) {
              vec[endcov1f+j2-1] = x; if (i2 > 1) vec[endcov1f+i2-1] = -x
              if (i1 > 1) {vec[ninter+fac2zj+i1-1] = x; if (i2 > 1) vec[ninter+fac2zi+i1-1] = -x}
              cat(" at ",covname,"=", x,sep="")
            }
            cu_estout(fit, vec, conf.int=conf.int)
          }
          for (i1 in 1:nlev11) {  # comparing factor 1 levels
            for (j1 in (i1+1):nlev1) {
              vec = c(rep(0,zeros));
              vec[fac2zj+j1-1] = 1; if (i2 > 1) vec[fac2zi+j1-1] = -1
              if (i1>1) {vec[fac2zj+i1-1] = -1; if (i2 > 1) vec[fac2zi+i1-1] = 1}
              cat(g1names[j1],"minus",g1names[i1])
              if (!is.null(xs)) {
                vec[ninter+fac2zj+j1-1] = x; if (i1 > 1) vec[ninter+fac2zj+i1-1] = -x
                if (i2 > 1) {vec[ninter+fac2zi+j1-1] =-x; if (i1 > 1) vec[ninter+fac2zi+i1-1] =  x}
                cat(" at ",covname,"=", x,sep="")
              }
              pvminint = min(pvminint,cu_estout(fit, vec, conf.int=conf.int))
            }
          }
          cat("\n")
        }
      }
    }
    if (pvminint > 0.05) {
      cat("Smallest interaction p-value is",signif(pvminint,3),
          "\nYou may wish to try a non-interaction anova (interact=F)\n")
    }
  }  # end of interact=T

  else {
    cat(g1name," comparisons\n\n",sep="")
    nlev11sq = nlev11*nlev11; p.values = rep(-1,sets*nlev11sq)
    for (k in 1:sets) {
      for (i in 1:nlev11) {
        for (j in (i+1):nlev1) {
          vec=c(rep(0,zeros)); vec[j+1] = 1; if (i>1) vec[i+1] = -1
          if (!is.null(xs)) {
            x=xs[k]; vec[j+end1v-1]=x; if (i>1) vec[i+end1v-1]=-x
          }
          cat(g1names[j], " minus ", g1names[i],sep="")
          if (!is.null(xs)) {cat(" at ",covname,"=", x,sep="")}
          p.values[(k-1)*nlev11sq+(i-1)*nlev11+j-1] = cu_estout(fit,vec,conf.int=conf.int)
        }
      }
    }
    cat("\n",g2name," comparisons\n\n",sep="")
    p.value2 = rep(-1,nlev21*nlev21)
    for (i in 1:nlev21) {
      for (j in (i+1):(nlev2)) {
        vec=c(rep(0,zeros)); vec[j+nlev1] = 1; if (i>1) vec[i+nlev1] = -1
        cat(g2names[j], " minus ", g2names[i],sep="")
        p.value2[(i-1)*nlev21+j-1] = cu_estout(fit, vec, conf.int=conf.int)
      }
    }
    cat ("\nSummary of p-values by ancova and post-hoc t\n")
    cat("\n",g1name,"comparisons\n")
    p.value = rep(-1,nlev11sq); ipvk = 0
    for (k in 1:sets) {
      for (ipv in 1:nlev11sq) {ipvk=ipvk+1; p.value[ipv]=p.values[ipvk]}
      if (!is.null(xs)) cat("\nAt ",covname," = ",xs[k],":\n",sep="")
      cu_pvalmatout(p.value,g1names)
    }
    cat("\n",g2name,"comparisons\n")
    cu_pvalmatout(p.value2,g2names)
    cat("\nEach p-value compares group above vs to left (adjustment method: none)\n")
  }
  if (partialF) {
    cat("Partial F-test vs simpler models:")
    ndfz = fit$df; rsez = sigma(fit)
    #cat("\n",ndfz,rsez,rsez^2*ndfz)
    strintg2 = paste(" interaction - cucov2way(",
                     depsubdep,",",depsubcov,",",depsubg1,",",depsubg2,sep="")
    if (!is.null(xs) && interact) {
      fitssi = lm(depvar ~ covar+group1+group2+group1*group2, na.action="na.exclude")
      pvalssi = partfout(fitssi,paste("same slope with",strintg2,
                                      ",interact=T)",sep=""))
      fitvsni = lm(depvar ~ covar+group1+group2+covar*group1, na.action="na.exclude")
      pvalvsni = partfout(fitvsni,paste("variable slope, no",strintg2,
                                        ",c(...),interact=F)",sep=""))
    }
    else {pvalssi = 0; pvalvsni = 0}
    if (!is.null(xs) || interact) {
      fitssni = lm(depvar ~ covar+group1+group2, na.action="na.exclude")
      pvalssni = partfout(fitssni,paste("same slope, no",strintg2,
                                        ",interact=F)",sep=""))
    }
    else pvalssni = 0
    strcov = paste(" - cucov1way(",depsubdep,",",depsubcov,",",sep="")
    if (!is.null(xs)) {
      fitg1 = lm(depvar ~ covar+group1+covar*group1, na.action="na.exclude")
      pval1c = partfout(fitg1, paste("no ",depsubg2,strcov,depsubg1,",c(...))",sep=""))
    }
    fitg1 = lm(depvar ~ covar+group1, na.action="na.exclude")
    pvalt = partfout(fitg1, paste("no ",depsubg2,strcov,depsubg1,")",sep=""))
    if (is.null(xs)) pval1c = pvalt
    if (!is.null(xs) && interact) {
      fitg2 = lm(depvar ~ covar+group2+covar*group2, na.action="na.exclude")
      pval2c = partfout(fitg2, paste("no ",depsubg1,strcov,depsubg2,",c(...))",sep=""))
    }
    else {
      fitg2 = lm(depvar ~ covar+group2, na.action="na.exclude")
      pval2c = partfout(fitg2, paste("no ",depsubg1,strcov,depsubg2,")",sep=""))
    }
    if (interact) fitnoc = lm(depvar ~ group1*group2, na.action="na.exclude")
    else fitnoc = lm(depvar ~ group1+group2, na.action="na.exclude")
    pvalnoc = partfout(fitnoc,paste("no ",depsubcov," - cu2way(",depsubdep,",",
                                    depsubg1,",",depsubg2,",interact=",interact,")",sep=""))
    fitg1 = lm(depvar ~ group1, na.action="na.exclude")
    fitg2 = lm(depvar ~ group2, na.action="na.exclude")
    pval1 = partfout(fitg1, paste("just ",g1name," - cu1way(",depsubdep,",",
                                  depsubg1,")",sep=""))
    pval2 = partfout(fitg2, paste("just ",g2name," - cu1way(",depsubdep,",",
                                  depsubg2,")",sep=""))
    fitco = lm(depvar ~ covar, na.action="na.exclude")
    pvalco = partfout(fitco, paste("just ",depsubcov," - lm(",depsubdep," ~ ",
                                   depsubcov,")",sep=""))
    if (pvalssi > 0.05 || pvalvsni > 0.05 || pvalssni > 0.05 || 
        pval1c  > 0.05 || pval2c   > 0.05 || pvalnoc  > 0.05 || 
        pval1   > 0.05 || pval2    > 0.05 || pvalco   > 0.05)
    cat("\nModel is overly complex. Consider a simpler model")
    else if (interact && pvminint > 0.05)
      cat("\nbut smallest interaction p-value is",signif(pvminint,3),
          "\nYou have a dilemma with a significant partial F-test,",
          "\nbut without any interaction reaching significance.")
    cat("\n")
  }

  ycalc = predict(fit)
#  p <- ggplot(datf, aes(covar,color=groupvar)) + geom_point(aes(y=depvar),shape=shape) + geom_line(aes(y=ycalc))
#  p <- p + labs(x=covname) + labs(colour=nameboth12) + labs(y=depname) +
#    ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  datf = na.omit(data.frame(A=depvar, B=groupvar, C=covar, D=ycalc))
  if (color == "black")
    p <- ggplot(datf, aes(datf[,3],shape=datf[,2],linetype=datf[,2])) + 
    geom_point(aes(y=datf[,1]),color="black") + labs(shape=nameboth12) + 
    geom_line(aes(y=datf[,4])) + labs(linetype=nameboth12)
  else {
    if (is.logical(shape) && shape) shape=datf[,2]
    p <- ggplot(datf, aes(datf[,3],color=datf[,2])) + 
      geom_point(aes(y=datf[,1]),shape=shape,size=dotsize) + labs(colour=nameboth12) + 
      geom_line(aes(y=datf[,4]),linetype=linetype,size=linesize)
  } # what follows is same as in cov1way but for name in cu_plout
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
    theme(axis.line = element_line(color=axiscolor,size=axisthick),
          axis.ticks= element_line(color=tickcolor,size=tickthick), # color ignored
          axis.ticks.length=unit(ticklength,"mm"),
          legend.title = element_text(size=legheadsize),
          legend.text = element_text(size=legtextsize))
  p <- ggpar(p, xlim=c(xmin,xmax), ylim=c(ymin,ymax), font.family=fontfamily,
             font.main = fontmain, font.x = fontxname, font.y = fontyname,
             font.xtickslab = fontxticks, font.ytickslab = fontyticks,
             xticks.by=xticks.by, yticks.by=yticks.by,
             x.text.angle=xangle, y.text.angle=yangle, caption=caption)
  cu_plout(p,"cucov2way",ftype=ftype, fname=fname,scale=fscale,
           width=fwidth,height=fheight, dpi=dpi, remove=remove)
}
