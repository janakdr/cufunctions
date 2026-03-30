#' Does one-way anova/post-hoc tests or contingency tables, and makes a bar graph
#' @param depvar ,group1 required: variable being analyzed, grouping factor
#' @param ebars =0 (default to be 1 or 4)/1/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR) or -N (nonparm if any norm fail)
#' @param ordinal =NULL (default)/T/c("...") to treat variable as no/yes ordinal
#' @param plot ="bar" (default) for bar graphs; "box" "violin" "rod" "no"
#' @param ytrans ="none" (default) to transform depvar: "sqrt" "log"
#' @param scale ="frequency" (default) or "percent"
#' @param dots =0 (default), 1 to display data on graph
#' @param barcolor for outline color, barfill for fill color. Use fill="grey" ="black" etc for single color.
#' @param casecontrol =F (default)/T if depvar has 2 levels control and case
#' @param maxfor2 =5 (default)/x for max p-values on a line for categorical data
#' @param g1order =NULL (default)/c("...") to reorder bars
#' @param psigcld =0 (no letters)/x for CLD letters on bars (any shared letter means P>psigcld, 0 for no letters)
#' @param conf.int =0.95 (default)/x/0 for confidence interval width of contrast estimates (0 for none)
#' @param refmean =NA (default)/ref.val to compare each group mean with
#' @param depname /g1name/title to override names of depvar, group1, title
#' @param caption =NULL (default)/"yes"/"caption text" to get caption ("yes" to list n's)
#' @param minimal =F (default)/T for minimal console output and no graph
#' @param padj ="none"(default), p-value adjustment for multiple comparisons, can be "bonferroni", "holm")
#' @param pbart =0.05 (default)/x for Bartlett test threshold for variance homogeneity
#' @param pnorm =0.05 (default)/x for normality test threshold
#' @param chariqr =":" (default)/x for character to separate quartiles ("-" "," etc)
#' @param pvpairs="std" (default)/"all"/i/c(ij,ik,...) to show std or all or vs.i or pval's of i/j, i/k ...
#' @param pvypos=NULL (default)/position of pval lines
#' @param pvstinc=0.05 (default)/increment for pvypos from one pval line to next
#' @param pvlab="p"/"*" to display numerical or asterisks
#' @param pvprefix="p=" (default)/"" etc prefix to p-values
#' @param pvsize=NULL (default 3.5, 7)/size of p-values or **
#' @param chpvref="ref" (default) char above i-th group bar when pvpairs=i
#' @param pvspill=F/T to not allow p-values to spill outside range or to allow
#' @param pnosig =0.2 (default)/x for threshold to show p-values even if nonsigificant
#' @param psignif =0.05 (default)/x for threshold to significance
#' @param p2stars =0.01 (default)/x for threshold to two stars
#' @param p3stars =0.001 (default)/x for threshold to three stars
#' @param p4stars =0.0001 (default)/x for threshold to four stars
#' @param pvnshide=T (default)/F to hide NS p-values or not
#' @param pvtipl=0.01 (default)/length of p-value line tips
#' @param doAll =T (default) for "All" column; F for no; "I" in quotes for All first.
#' @param linetype ="n" (default)/x for no connecting lines ("solid" "dashed" "dotted" "blank" "longdash" "dotdash" "twodash")
#' @param linesize =1 (default)/x for line thickness
#' @param linecolor ="black" (default)/x for black lines ("red" etc)
#' @param theme ="bw" (default)/x for white background (or "gray" or "classic" (no grid lines))
#' @param legend (default="top"), can be "bottom", "right", "left"
#' @param size numeric value (e.g. size=1), to change size of points and outlines
#' @param width numeric value between 0 and 1 specifying box width
#' @param yscale (default="none"), can be "log2", "log10", "sqrt"
#' @param letleft =T (default) or F to letter the bars from the left (T) or from most similar (F)
#' @param nletbarmax =3 (default)/x for max consecutive letters on bar before using "-"
#' @param font.x =NULL (default)
#' @param xangle /yangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param orientation (default="vertical"), can be "horizontal" or "reverse"
#' @param dotsize =NULL (default 1) set to fraction of binwidth for dot size
#' @param dotshape =NULL (default)
#' @param dotcolor =NULL (default "white") set to dot color
#' @param posd =NULL (default) set to values around 0.9 to fine-tune group2 bar spacing
#' @param binwfac =30 (default) set to fraction of range within which points will be binned
#' @param fontfamily ="sans" (default), can be "serif" "mono" 
#' @param ymin, ymax =NA (default) or value to start/end y-axis 
#' @param fontmain =c(14,"bold","black") default, change for title, 0 for not title 
#' @param fontxname .fontyname,fontxticks,fontyticks = c(12,"plain","black") default, 0 to suppress
#' @param axiscolor ,tickcolor="black" (default)/x for axis/tick color
#' @param axisthick ,tickthick=0.5 (default)/x for axis/tick thickness
#' @param ticklength =1 (default)/x for tick length in mm
#' @param xticks.by ,yticks.by =NULL (default)/s for x/y tick spacing by s
#' @param titlejust ="center" (default) or "left" or "right"
#' @param legheadsize =12 (default) for font size of legend heading
#' @param legtextsize =10 (default) for the font size of legend text
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return returns nothing
#' @examples
#' attach(NEJM)
#' cu1way(tcchange, Diet)  # for one-way anova and post-hoc t-tests, graphing mean+/-sd
#' # cu1way(tcchange, Diet, ebars=N)  # N=0 for 1 or 4, 1 for SD, 2 for SE, 3 for CL
#' cu1way(tcchange, Diet, ebars=4)  # for nonparametric Dunn
#' detach(NEJM)
#' attach(Met)
#' cu1way(feel, WTCAT) # contingency tables (all possible 2x2s)
#' detach(Met)
#' @export
cu1way = function(depvar, group1, ebars=0, ordinal=NULL, plot="bar", ytrans="none",
                  scale="frequency", dots=0, barcolor="black", barfill="colors", casecontrol=F,
                  maxfor2=5, g1order=NULL, psigcld=0, conf.int=0.95, refmean=NA,
                  depname=NULL, g1name=NULL, title=NULL, caption=NULL, minimal=F,
                  padj="none", pbart=.01, pnorm=.01, chariqr=":",
                  pvpairs="std", pvypos=NULL, pvstinc=0.05, pvlab="p", 
                  pvprefix="p=", pvsize=NULL, chpvref="ref", pvspill=F,
                  pnosig=0.2, psignif=0.05, p2stars=0.01, p3stars=0.001, p4stars=0.0001,
                  pvnshide=T, pvtipl=0.01,
                  doAll=T, linetype="n", linesize=1, linecolor="red", theme="bw",
                  legend="top", size=NULL, width=NULL, yscale="none", letleft=T,nletbarmax=3,
                  font.x=NULL, xangle=NULL, yangle=NULL, orientation="vertical",
                  dotsize=NULL, dotshape=NULL, dotcolor=NULL, posd=NULL, binwfac=30, 
                  fontfamily="sans", ymin=NA, ymax=NA,
                  fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                  fontxticks=NULL, fontyticks=NULL,
                  axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                  ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                  titlejust="center", legheadsize=12, legtextsize = 10,
                  ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                  fheight=NULL, dpi=300, remove=NULL) 
{
  cuf_apply_defaults(match.call(), environment())
  notgroup = function(namegp) {stop(namegp,
     " cannot be a group variable as numeric with ",nlev," levels.",
     "\n    If it is the variable being analyzed, it should come first.",
     "\n    Or, did you mean to run cucov1way, cucov2way, cumreg?")
  }
  whatintend = function(namegp) {
    warning("ordinal ignored. Typo? Look at message at beginning")
    return(paste("\nordinal ignored. Did you intend to say g1order=",
      "\n     or did you intend cu1way(",g1name,",",depname,",...?\n",sep=""))
  }
  get2x2 = function(i,j) {
    m11 = sum(df2[id1:idep,i])
    m21 = ifelse(jdep>idep, df2[jdep,i], as.integer(df[1,i]) - m11)
    m12 = sum(df2[id1:idep,j])
    m22 = ifelse(jdep>idep, df2[jdep,j], as.integer(df[1,j]) - m12)
    return(matrix(data=c(m11,m12,m21,m22),nrow=2,ncol=2))
  }
  printdfAll = function(df,right=T) {
    # cat("\nprintdfAll:\n")
    if (Allfirst) print(df %>% relocate(All), right=right)
    else print(df, right=right)
  }
  pval9ns = function(pval) {
    return(ifelse(pval<0.001,sprintf("%.2e%1s",pval,""),
                  ifelse(pval<pnosig,sprintf("%-9.9s",signif(pval,3))," ns   ")))
  }
  # cat("\n1way:"); print(depvar)

  Allfirst = F
  if (!is.logical(doAll)) {
    if (doAll=="I") Allfirst = T
    else cat ("\ndoAll='",doAll,"' no good. Ignored",sep="")
    doAll = T
  }
  else if (!doAll) doAll = T # too much work to suppress All
  if (is.null(plot)) {plot = "no"}
  else if (plot %in% c("n","no","N","NO","No")) {plot="no"}
  else if (!(plot %in% c("bar","box","violin","rod"))) {
    cat("\nplot='",plot,"' no good. Taken to be 'bar'",sep=""); plot="bar"
  }
  if (minimal) {irmean = 2; pnorm = 0}  # not needed any more, unlike in 2way
  else {nrowmax=16; irmean=3; irmed=9} # constants
  anyzneg = function(depv) {
    izneg = 1
    for (j in 1:length(depv)) {
      depj = depv[j]
      if (!is.na(depj) && is.numeric(depj)) if (depj<0) {izneg = -1; break}
      else if (depj==0) izneg=0
    }
    #cat("\nj,izneg",j,izneg)
    return(izneg)
  }
  itrans = 0
  if (ytrans != "none") if (is.factor(depvar) || ebars <= 0 || ebars == 4)
    {cat("\nytrans ignored for contingency or ebars=4.\n"); ytrans="none"} # no need for transforms
  else {
    izneg = anyzneg(depvar)
    if (ytrans=="sqrt") if (izneg<0)
      {cat("\nNo sqrt if any negative values.\n"); ytrans="none"}
    else itrans=1
    else if (ytrans=="log" || ytrans=="log10") if (izneg<=0)
      {cat("\nNo log unless all positive values.\n"); ytrans="none"}
    else itrans=2
    else {cat("\nytrans no good:",ytrans,"\n"); ytrans="none"}
  }
  if (is.null(depname)) depname=deparse(substitute(depvar))
  depnact = depname
  if (itrans==1) {
    depname = paste("sqrt(",depname,")",sep="")
    for (j in 1:length(depvar)) if (!is.na(depvar[j])) depvar[j] = sqrt(depvar[j])
  }
  else if (itrans==2) {
    depname = paste("log10(",depname,")",sep="")
    for (j in 1:length(depvar)) if (!is.na(depvar[j])) depvar[j] = log10(depvar[j])
  }
  if (is.null(g1name)) g1name=deparse(substitute(group1))
  if (is.numeric(group1)) {
    nlev = nlevels(as.factor(group1))
    if (nlev > 9) notgroup(g1name)
  }
  if (length(ebars)>1) stop("\n",deparse(substitute(ebars)),
      "??? cu1way is for only one factor. Did you intend cu2way?\n")
  if (length(depvar) != length(group1)) {
    cat("\n\n#observations of dependent variable and group factor not equal:",
        length(depvar), length(group1),"\n")
    #print(depvar); print(group1)
    stop("\nquitting")
  }
  group1 = cu_reorder(group1, g1order); anyxmiss = F
  nlev = nlevels(group1)
  g1names = levels(group1)
  if (is.character(depvar)) depvar = as.factor(depvar)
  #if (is.factor(depvar) || !minimal) cat("\n",depname,"compared across",g1name,"groups\n\n")
  if (is.null(title)) title=paste(depnact, "at",nlev,"levels of", g1name)
  groupvm = as.character(group1)  # solely for table0 to have column for missing group1
  for (i in 1:length(depvar)) { 
    if (is.na(group1[i])) 
      {anyxmiss=T; groupvm[i] = "ZZZZZZZZZZZ"} # last column
  }
  if (anyxmiss && !is.null(g1order)) g1order = c(g1order,"ZZZZZZZZZZZ")
  groupvm = cu_reorder(groupvm, g1order); g1order = NULL
  nlem = nlevels(groupvm)
  if (nlem != nlev + ifelse(anyxmiss,1,0)) stop ("\nnlev,nlem",nlev,nlem)
  if (is.factor(depvar)) {
    nlevdep = nlevels(depvar)
    if (nlevdep<=1) stop (depname,' has only one level. No good')
    if (nlevdep>2 && casecontrol) {
      casecontrol=F; cat("\nCase-Control ignored: impossible with >2 levels\n")
    }
    if (is.null(ordinal)) ifordinal = F
    else if (nlevdep<=2) {
      if (!is.null(ordinal)) 
        cat("\nNo need for ordinal with only two dependent variable levels.",
            whatintend())
      ifordinal = F
    }
    else if (is.logical(ordinal)) ifordinal = ordinal
    else {
      ifordinal = T
      if (length(ordinal) != nlevdep) {
        i = 0; 
        cat("\nordinal must have exactly",nlevdep,depname,"names.",
            whatintend())
      }
      else for (i in (1:nlevdep)) {
        if (!(levels(depvar)[i] %in% ordinal)) {
          cat("\n",depname," '",levels(depvar)[i],"' not in ordinal. Typo?",
              whatintend(),sep="")
          i = 0; break
        } }
      if (i==0) ordinal = T  # why?
      else depvar = factor(depvar, levels=ordinal)
    } # reorder depvar before tables and barplot
    # cat("\n",ifordinal)
    if (scale != "frequency") if (scale != "percent") {
      cat("\n scale can only be frequency or percent, in quotes\n")
      scale="frequency"
    }
  }
  else { # continuous, not categorical, code below needed in 2 places
    dsnomiss = stats::na.omit(data.frame(A=depvar,B=group1))
    g <- factor(dsnomiss$B)
    avv <- tapply(dsnomiss$A, g, mean, na.rm = TRUE)
    sdv <- tapply(dsnomiss$A, g, sd, na.rm = TRUE)
    nsv <- tapply(!is.na(dsnomiss$A), g, sum)
    degf <- nsv - 1
    total.degf <- sum(degf)
    pooled.sd <- sqrt(sum(sdv^2 * degf)/total.degf)
  }
  # cat("\nchariqr,minimal",chariqr,"' ",minimal)
  if (minimal) {
    dfn = cutable1(depvar,groupvm,compare=T,brief=T, minimal=T, pnosig=pnosig,
              depname=depname,g1name=g1name,plot="no",doAll=F,chariqr=chariqr)
    repou = dfn[[1]]; normTF = dfn[[2]]; pnormin = dfn[[3]]
    lenrep = length(repou)+nlev+2 
    # repou has nlev + # contrasts, so add nlev for sd and for name, pnormin
    if (!is.na(refmean)) lenrep = lenrep+nlev # each group's p-val vs refmean
    repout = vector(mode="character",length=lenrep); repn = rep("??",lenrep)
    repout[1] = depname; repn[1] = "Variable"; jrepo = 2
    # print(repou)
    for (ig1 in 1:nlev) {
      repout[jrepo] = repou[2,ig1]; repout[jrepo+1] = repou[3,ig1] #mean/median sd/iqr
      repnam = names(repou)[ig1]
      repn[jrepo] = paste(repnam,"_M",sep="")
      repn[jrepo+1] = paste(repnam,"_sd/iqr",sep="")
      jrepo = jrepo+2
    }
    if (!is.na(refmean)) {
      chvref = paste(" v",round(refmean,1))
      for (ig1 in 1:nlev) {
        tstat = abs(avv[ig1]-refmean)*sqrt(nsv[ig1])/pooled.sd
        pv1t = 2 * stats::pt(-tstat, total.degf) # same as stats::pt(tstat,..., lower.tail=F)
        repout[jrepo] = ifelse (pv1t <= pnosig,
               ifelse (pv1t < 0.001, "<.001", round(pv1t,3)))
        repn[jrepo] = paste(names(repou)[ig1],chvref); jrepo = jrepo+1
      }
    }
    ig1 = nlev
    for (jrepo in jrepo:(lenrep-1)) {
      ig1 = ig1+1; repout[jrepo] = repou[2,ig1]; repn[jrepo] = names(repou)[ig1]
      if (repout[jrepo]==" ns   ") repout[jrepo] = ""
    }
    repout[lenrep] = ifelse(pnormin<=pnorm,pnormin," "); repn[lenrep] = "pnormin"
    names(repout) = repn
    # print(repout)
    return(repout) # no need to test on minimal further down
  }
  brief = ifelse(is.factor(depvar), F, minimal) # check this first
  dfn = cu_table0(depvar, groupvm, brief=brief, doAll=doAll, pnorm=pnorm, chariqr=chariqr)
  # print(dfn)
  df = dfn[[1]]; normTF = dfn[[2]]; pnormin = dfn[[3]]
  colnames(df)=c(levels(groupvm),"All")
  if (anyxmiss) colnames(df)[nlem] = "NA's"
  if (!is.null(caption) && caption=="yes") {
    caption = "N in grps:"
    for (j in 1:nlev) caption = paste(caption, as.integer(df[1,j])-as.integer(df[2,j]))  # nlem??
  }
  # cat("\ncaption:'",caption,"'",sep="")
  cat("\n",depname,"compared across",g1name,"groups\n")
  if (!is.factor(depvar) || !minimal) printdfAll(df)
  if (is.factor(depvar)) {
    #    desc = by(depvar, group1, summary.factor)
    df2 = data.frame(A = rep(0,nlevdep),stringsAsFactors = F)
    #    df2 = matrix(nrow=nlevels(depvar),ncol=nlev)
    # why can't it be a matrix? no colnames for a matrix?
    rownames(df2) = levels(depvar)
    for (j in 1:nlev) {  # leaving out NA in x
      for (i in 1:nlevdep) df2[i,j]= as.integer(substr(df[i+2,j],regexpr("%",df[i+2,j])+3,10))
    }
    colnames(df2) = levels(group1)
    chifish = cu_chi2fish(df2,""); # appropriate to include missing x?
    cat("\n"); print(df2)
    if (minimal) casecon = T
    else {
      cat ("[Note that outcome is in rows (unlike in biomath.net/stat)\n")
      casecon = casecontrol
      df2mat = as.matrix(df2); colsum = rep(0,nlev)
      if (scale == "percent") for (j in 1:nlev) {
        colsum[j] = sum(df2mat[,j])
        if (colsum[j]>0) df2mat[,j] = 100*df2mat[,j]/colsum[j]
      }
      plotobj <- graphics::barplot(df2mat, xlab=g1name, ylab=depname,
                         main=paste(depname,"distribution at different levels of",g1name),
                         col=grDevices::rainbow(nlevdep), legend = rownames(df2), 
                         args.legend = list(x = "topright", bty = "n"))
      if (scale == "percent") plotobj <- graphics::text(plotobj,102,labels=colsum,xpd=T)
      # ggbarplot? if (!is.null(caption)) plotobj <- ggpar(plotobj,caption=caption)
      plotobj # cu_plout with barplot obj prints out x vector or NULL
      #cu_plout(plotobj,"curm",emf,suff=suff) # must rephrase
    }
    cat("\n", ifelse(casecon,"Odds Ratios","Relative Risks"),
        " (95% confidence limits) p-values for ",depname," between levels of ",g1name,sep="")
    #if (min(df2)==0) cat("\n   (cell counts bumped by 0.5 if any calculation gives 0 or \u221E)")
    ifpairwy = F; chleq = ""
    if (nlevdep==2) ndeptodo = 1
    else if (ifordinal) {ndeptodo = nlevdep-1; chgtnot = ">"; deplevn = levels(depvar)[nlevdep]}
    else {ifpairwy = T;  ndeptodo = nlevdep;   chgtnot = "NOT"}
    # if ordinal, <= vs > for each level; else, vs NOT and vs every other
    for (idep in 1:ndeptodo) {
      id1 = ifelse(ifordinal,1,idep); deplevi = levels(depvar)[idep]
      for (jdep in idep:ifelse(ifpairwy,nlevdep,idep)) {
        cat("\n\n",ifelse(ifordinal&&(idep==nlevdep-1), paste("<",deplevn,"vs",deplevn),
                  paste(chleq,deplevi," vs ", ifelse(nlevdep==2, levels(depvar)[2],
                   ifelse(jdep>idep, levels(depvar)[jdep], paste(chgtnot,deplevi))),sep="")),
            "in:")
        for (i in 1:(nlev-1)) for (j in (i+1):nlev) cu_chi2fish(get2x2(i,j),
              paste(g1names[i]," vs ",g1names[j],":",sep=""), casecontrol=casecon)
      }
      if (ifordinal) chleq = "<="
    }
    cat("\n"); return(invisible())
  }
  if (!minimal) {
    cat("\nSkewness, Kurtosis & Normality testing not done if SD=0 or n<6 or n>4000 (or for All).",
        "\nNormality by Shapiro-Wilk, p-value shown if <0.1\n\n")
    if (ebars !=4) {
      pnormin = 1; nfail = 0
      for (j in 1:nlev) { # logic from table0
        pvalch = df[nrowmax,j]
        if (!is.na(pvalch) && !is.null(pvalch) && pvalch!=" ") {
          if (regexpr("<",pvalch) > 0) 
          {pnormin = 0; nfail = nfail+1} # < means < 0.001
          else if ((pval=as.numeric(pvalch))<pnorm) 
          {pnormin = min(pnormin,pval); nfail = nfail+1}
          # cat ("\n",j," '",pvalch,"' ",pnormin,sep="")
        }
      }
      if (nfail > 0) {
        cat("\nDATA FAIL NORMALITY TEST IN",nfail,"OF",nlev,"GROUPs.",
            "SMALLEST P-VALUE",ifelse(pnormin==0,"<0.001",pnormin))
        if (ebars<=0) {ebars=4; cat("\nNONPARAMETRIC ANALYSIS WILL BE DONE.\n")}
        else cat("\nLOOK FOR DATA ERRORS IN 'Min' AND 'Max' VALUES.",
                 "\nIF DATA ARE NOT NORMAL,",
                 "\nYOU SHOULD USE THE NONPARAMETRIC DUNN TEST (ebars=4)\n")
      }
      else if (ebars<=0) ebars = min(3,max(1,-ebars))
    }
    if (ebars !=4) {
      myaov = stats::aov(depvar ~ group1)
      cat("\none-way anova: aov(",depname," ~ ",g1name,")\n",sep="")
      print(summary(myaov))
      coeffs = c("(Intercept)") # below loads g1names into coeffs
      for (j in 2:nlev) {coeffs = c(coeffs,paste(g1name,g1names[j],sep=""))}
      strcall = paste("lm(",depname," ~ ",g1name,sep="")
    }
    else {
      myaov = stats::kruskal.test(depvar ~ group1)
      myaov$data.name = paste(depname,"by",g1name)
      print(myaov)
    }
  } # end of !minimal
  
  dsnomiss = stats::na.omit(data.frame(A=depvar,B=group1))
  if (ebars != 4) {
    pool.sd=T
    nlevact = 0; nmink = 0; chisq = 0; sumsq = 0; sumlnsq = 0; sumn1 = 0
    for (jg in 1:nlev) {
      njg1 = as.numeric(df[1,jg])-1
      if (njg1 > 0.9) {
        varg = as.numeric(df[irmean+1,jg])**2
        if (varg==0) chisq = 1
        else {
          nlevact = nlevact+1; nmink = nmink+njg1
          sumsq = sumsq + njg1*varg; sumlnsq = sumlnsq + njg1*log(varg); sumn1 = sumn1+1/njg1
        }
      }
    }
    if (nlevact==0) pval = 1  # no group with nonzero variance
    else if (chisq>0) pval = 0  # some group(s) with zero variance
    else if (nlevact>1) { # >1 group with at least 2
      chisq = (nmink*log(sumsq/nmink) - sumlnsq)/(1+(sumn1-1/nmink)/(3*(nlevact-1)))
      pval = stats::pchisq(chisq, df=nlevact-1, lower.tail=F)
    }
    #bt=bartlett.test(depvar,group1)
    #bt$data.name = paste(depname,"across",g1name,"groups")
    #if (abs(1-as.numeric(bt[3])/pval)> 0.05)
    #  cat("\nbt[3],pval,nlevact,nmink,sumsq,sumlnsq,sumn1,chisq",
    #      as.numeric(bt[3]),pval,nlevact,nmink,sumsq,sumlnsq,sumn1,chisq)
    if(pval<pbart) pool.sd=F
    if(pval<0.05 && !minimal) {
    #  print(bt)
      cat("\nData fail the Bartlett test for homogeneity of variances (p=",
          signif(pval,digits=3)," for chi-sq=",signif(chisq,digits=3),
          " with ",nlevact-1," df)", sep="")
      if (pool.sd) cat ("\nNevertheless, using pooled SD because pbart is set low. We trust you want that.")
      else cat("\nCOULD NOT USE POOLED SD DUE TO UNEQUAL VARIANCES.",
           "\nLOOK FOR DATA ERRORS, FOCUSING ON GROUP(S) WITH LARGE SD AND UNEXPECTED MIN/MAX IN SUMMARY ABOVE.",
           "\nIf you wish to pool variances despite failing Bartlett,", 
           "redo cu1way adding pbart=x, where x<",signif(pval,digits=3),"\n")
    }
    if (pool.sd) { # consider printing pval matrix first, as in 2way
      pairout = cupairwise.t(dsnomiss$A,dsnomiss$B,p.adjust=padj)
      #junk = pairout$p.value; cat("\njunk:"); print(junk)
      pairout$data.name = paste(depname,"compared across",nlem,g1name,
                          "groups",ifelse(padj=="none","using Fisher's LSD",""))
      pairout$method = paste(pairout$method,signif(pooled.sd,3))
      if (!minimal) {
        fit = stats::lm(depvar ~ group1)
        fit$call = paste(strcall,")",sep="")
        names(fit$coefficients) = coeffs
        print(summary(fit))
        cat("\n",g1name," comparisons of ",depname,"\n\n",sep="")
        for (i in 1:(nlev-1)) {
          for (j in (i+1):nlev) {
            vec=c(rep(0,nlev)); vec[j] = 1; if (i>1) vec[i] = -1
            cat(g1names[j], " minus ", g1names[i],sep="")
            cu_estout(fit, vec)
          }
        }
        pvalues = pairout$p.value
        if (!is.na(refmean)) for (ig1 in 1:nlev) {
          tstat = abs(avv[ig1]-refmean)*sqrt(nsv[ig1])/pooled.sd
          pv1t = 2 * stats::pt(-tstat, total.degf) # same as stats::pt(tstat,..., lower.tail=F)
          cat(g1names[ig1], " v ", round(refmean,1),": p=",
            ifelse (pv1t < 0.001, "<.001", round(pv1t,3)),"\n", sep="")
        }
      }
    }
    else {
      pairout = cupairwise.t(dsnomiss$A,dsnomiss$B,p.adjust=padj,pool.sd=F)
      pairout$data.name = paste(depname,"compared across",nlem,g1name,
                                "groups by Welch's t (unequal variances)")
      if (!minimal) {
        cat("\n",g1name," comparisons using unequal variances\n\n",sep="")
        desc = by(dsnomiss$A,dsnomiss$B,cusum.default)
        npts = c(0); means = c(0); sds = c(0)
        for (i in 1:nlev) { # df has limited precision, so rounding errors
          npts[i] = as.numeric(desc[[i]][1]) - as.numeric(desc[[i]][2])
          means[i] = as.numeric(desc[[i]][3]); sds[i] = as.numeric(desc[[i]][4])
        }
        #print(npts);print(means);print(sds); cat("\n")
        for (i in 1:(nlev-1)) if (npts[i]>0) {
          n1 = npts[i]; se1sq = sds[i]**2/n1
          for (j in (i+1):nlev) {
            n2 = npts[j]
            if (n2 > 1) {
              se2sq = sds[j]**2/n2; sedif = sqrt(se1sq+se2sq)
              mudif = means[j]-means[i]
              if (sedif==0) pvalt = ifelse(mudif==0,1,0)
              else {
                ndf = (se1sq+se2sq)**2/(se1sq**2/(n1-1)+se2sq**2/(n2-1))
                tstat = mudif/sedif; pvalt = 2 * stats::pt(-abs(tstat), ndf)
              }
              #cat(n1,n2,sds[i],sds[j],mudif,sedif,tstat,ndf,pvalt,"\n")
              cat(g1names[j], " minus ", g1names[i],": ",
                  signif(mudif,3)," \u00B1 ",signif(sedif,3),", p=",signif(pvalt,3),
                  ", CL=[",signif(mudif-1.96*sedif,3),",",
                  signif(mudif+1.96*sedif,3),"]\n",sep="")
            }
          }
        }
      }
      pvalues = pairout$p.value
    }
    #cat("\npairout:")
    print(pairout)
    #cat("\npvalues:")
    #print(pvalues)
  }

  if (ebars==4) {
#    oldw <- getOption("warn")
#    options(warn = -1)
#    pairout = kwAllPairsDunnTest(depvar,group1,p.adjust.method="none")
#    pairout$data.name = paste(depname,"compared across",g1name,"groups")
#    pvalues = pairout$p.value
#    options(warn = oldw)
    suppressMessages(utils::capture.output(dunout <- dunn.test(dsnomiss$A,dsnomiss$B, method=padj, altp=T, table=F, kw=F)))
    idu=0; nm1 = nlev-1; pvalues = matrix(c(rep(0,nm1*nm1)),nrow=nm1,ncol=nm1)
    for (i in 2:nlev) {
      for (j in 1:(i-1)) {idu=idu+1; pvalues[i-1,j] = dunout$altP[idu]}
    }
    if (!minimal) cat("\nPairwise comparisons using Dunn's all-pairs test",
        "\n      (generalizes Wilcoxon rank-sum)\n\n")
    maxnch = min(12,max(nchar(g1names)))
    if (maxnch>7) cat(rep(" ",maxnch-7),sep="")
    for (j in 1:nlev-1) {
      cat(substr(paste(g1names[j],"       "),1,7)," ",sep="")
    }
    for (i in 2:nlev) {
      cat("\n",substr(paste(g1names[i],"            "),1,maxnch)," ",sep="")
      for (j in 1:(i-1)) {
        pval = pvalues[i-1,j]
        if (pval<0.001) {cat(sprintf("%.1E",pval)," ",sep="")}
        else cat(sprintf("%.4f",pval)," ")
      }
    }
    if (!minimal) cat("\n\nP value adjustment method:", padj,"\n") # pairwise.t puts it in object
  }
  if (!minimal) cat("Each p-value compares groups above and to the left\n\n")
  if (!minimal && (plot!="no")) {
    mapord = cu_mapord(g1order,group1,nlev) # isn't g1order NULL by now?
    letbar = cu_letbar(pvalues,psigcld,mapord,nlev,letleft=letleft,nletbarmax=nletbarmax)
    for (i in 1:length(depvar)) # clumsy code to deal with missing group values
      if (is.na(group1[i])) {depvar[i]=NA; group1[i]=g1names[1]}
    #cat("\npvalues"); print(pvalues)
    pvstruc = cu_pvline(df,pvpairs,pvalues,nlev,1,pvlab,pnosig,psignif,p2stars,p3stars,p4stars,
                        letbar,pvypos,pvnshide,pvspill,chpvref,ebars,dots,yscale)
    # print(pvstruc)
    pvdf = pvstruc[[1]]; pvypos = pvstruc[[2]]; pvlab = pvstruc[[3]]
    yrange = pvstruc[[4]]; letbar = pvstruc[[5]]
    # cat("\npvdf,pvypos\n"); print(pvdf); print(pvypos)
    if (itrans==2) {
      #cat("\nddep:\n"); print(ddep)
      for (i in 1:length(depvar)) if (!is.na(depvar[i])) depvar[i] = 10**depvar[i]
      #cat("\nddep:\n"); print(ddep)
      yrange = 10**yrange; yscale = "log10"; ebars = 4  # to get median and iqr
    }
    else if (yscale=="log10") ebars = 4  # to get median and iqr
    if (is.null(pvtipl)) pvtipl = yrange*5e-5
    p <- cu_plot1("cu1way",depvar,group1, g1order, g1name, depnact, title,
                  plot=plot, linetype=linetype, linecolor=linecolor, linesize=linesize, 
                  letbar=letbar, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                  pvdf=pvdf,pvypos=pvypos,pvstinc=pvstinc,pvlab=pvlab,pvprefix=pvprefix,
                  pvsize=pvsize, pvnshide=pvnshide,pvtipl=pvtipl,
                  caption=caption, legend=legend, size=size, width=width, posd=posd,
                  xangle=xangle, yangle=yangle, orientation=orientation,
                  binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                  theme=theme, yscale=yscale, fontfamily=fontfamily,
                  yrange=yrange, ymin=ymin, ymax=ymax,
                  fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
                  fontxticks=fontxticks, fontyticks=fontyticks,
                  axiscolor=axiscolor,tickcolor=tickcolor,axisthick=axisthick,tickthick=tickthick,
                  ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
                  titlejust=titlejust, legheadsize=legheadsize, legtextsize = legtextsize,
                  ftype=ftype,fname=fname,fscale=fscale,fwidth=fwidth,
                  fheight=NULL, dpi=300, remove=remove)
  }
}
