#' Does two-way anova and post-hoc tests or contingency tables, and makes a bar graph
#' @param depvar required: variable being analyzed, grouping factors 1 & 2
#' @param group1 see depvar
#' @param group2 see depvar
#' @param interact =TRUE (default), F or FALSE for no interaction
#' @param dosimpler =F (default) or T not to do or to do 2 1ways first
#' @param partialF =TRUE (default), F or FALSE for no partial F vs simpler models
#' @param ordinal =NULL (default)/T/c("...") to treat variable as no/yes ordinal
#' @param scale ="frequency" (default) or "percent"
#' @param ebars =0 (default to be 1 or 4)/1/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR) or -N (nonparm if any norm fail)
#' @param dots =0 (default), 1 to display data on graph
#' @param plot ="bar" (default) for bar graphs; "box" "violin" "rod" "no"
#' @param ytrans ="none" (default) to transform depvar: "sqrt" "log"
#' @param barfill ="lancet" (default) for colors by group ("lancet" for 2 factors, other journal options "aaas", "jco", "uchicago", "npg"
#' @param casecontrol =F (default)/T if depvar has 2 levels control and case
#' @param maxfor2 =5 (default)/x for max p-values on a line for categorical data
#' @param g1order =NULL (default) to reorder bars (1st Factor)
#' @param g2order =NULL (default) to reorder bars (2nd Factor)
#' @param psigcld =0 (no letters)/x for CLD letters on bars (any shared letter means P>psigcld, 0 for no letters)
#' @param conf.int =0.95 (default)/x/0 for confidence interval width of contrast estimates (0 for none)
#' @param depname =NULL to override names of dependent/Factor1/Factor2/title
#' @param g1name see depname
#' @param g2name see depname
#' @param title see depname
#' @param caption =NULL (default)/"yes"/"caption text" to get caption ("yes" to list n's)
#' @param minimal =F (default)/T for minimal console output and no graph
#' @param padj ="none"(default), p-value adjustment for multiple comparisons, can be "bonferroni", "holm")
#' @param pbart =0.01 (default)/x for Bartlett test threshold for variance homogeneity
#' @param pnorm =0.01 (default)/x for normality test threshold
#' @param chariqr =":" (default)/x for character to separate quartiles ("-" "," etc)
#' @param charamp ="&" (default)/x for character to separate group1&2 levels ("-" etc)
#' @param pvpairs (default "std") (default)/"all"/i/c(ij,ik,...) to show std or all or vs.i or pval's of i/j, i/k ...
#' @param pvypos (default NULL) (default)/position of pval lines
#' @param pvstinc (default 0.05) (default)/increment for pvypos from one pval line to next
#' @param pvlab (default "p"/"*") to display numerical or asterisks
#' @param pvprefix (default "p=") (default)/"" etc prefix to p-values
#' @param pvsize (default NULL) (default 3.5,7)/size of p-values or **
#' @param chpvref (default "ref") (default) char above i-th group bar when pvpairs=i
#' @param pvspill (default F/T) to not allow p-values to spill outside range or to allow
#' @param pnosig =0.2 (default)/x for threshold to show p-values even if nonsigificant
#' @param psignif =0.05 (default)/x for threshold to significance
#' @param p2stars =0.01 (default)/x for threshold to two stars
#' @param p3stars =0.001 (default)/x for threshold to three stars
#' @param pvnshide (default T) (default)/F to hide NS p-values or not
#' @param pvtipl (default 0.01) (default)/length of p-value line tips
#' @param doAll =T (default) for "All" column; F for no; "I" in quotes for All first.
#' @param linetype ="n" (default)/x for no connecting lines ("solid" "dashed" "dotted" "blank" "longdash" "dotdash" "twodash")
#' @param linesize =1 (default)/x for line thickness
#' @param legend (default="top"), can be "bottom", "right", "left"
#' @param size numeric value (e.g. size=1), to change size of points and outlines
#' @param width numeric value between 0 and 1 specifying box width
#' @param yscale (default="none"), can be "log2", "log10", "sqrt"
#' @param letleft =T (default) or F to letter the bars from the left (T) or from most similar (F)
#' @param nletbarmax =3 (default)/x for max consecutive letters on bar before using "-"
#' @param xangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param yangle see xangle
#' @param orientation (default="vertical"), can be "horizontal" or "reverse"
#' @param posd =NULL (default) set to values around 0.9 to fine-tune group2 bar spacing
#' @param binwfac =30 (default) set to fraction of range within which points will be binned
#' @param dotsize =NULL (default 1) set to fraction of binwidth for dot size
#' @param theme ="bw" (default)/x for white background ("classic" (no grid lines),"linedraw" "gray" "minimal" "void")
#' @param fontfamily ="sans" (default), can be "serif" "mono" 
#' @param ymin =NA (default) or value to start/end y-axis
#' @param ymax see ymin
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
#' @param legheadsize =12 (default) for font size of legend heading
#' @param legtextsize =10 (default) for the font size of legend text
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale =NULL(default) or set to numerical value
#' @param fwidth see fscale
#' @param fheight see fscale
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @param barcolor ="black" (default) for bar outline color
#' @param p4stars =0.0001 (default)/x for threshold to four stars
#' @param linecolor ="black" (default)/x for line color
#' @param dotshape =NULL (default) for dot shape
#' @param fontmain =c(14,"bold","black") default, change for title, 0 for no title
#' @param axiscolor ="black" (default)/x for axis color
#' @return returns nothing
#' @examples
#' attach(NEJM)
#' cu2way(tcchange, Diet, sex)  # two-factor anova with interaction (all possible contrasts)
#' # two-factor anova with no interaction (all possible contrasts)
#' cu2way(tcchange, Diet, sex, interact=FALSE)
#' detach(NEJM)
#' attach(Met)
#' cu2way(feel,WTCAT,Sex) # contingency tables (all possible 2x2s)
#' detach(Met)
#' @importFrom rlang .data
#' @export
cu2way = function(depvar,group1,group2, interact=TRUE, dosimpler=F, partialF=TRUE, ordinal=NULL,
                  scale="frequency", ebars=0, dots=0, plot="bar", ytrans="none",
                  barcolor="black", barfill="colors", casecontrol=F,
                  maxfor2=5, g1order=NULL, g2order=NULL, psigcld=0, conf.int=0.95,
                  depname=NULL, g1name=NULL, g2name=NULL, title=NULL, caption=NULL, minimal=F,
                  padj="none", pbart=.01, pnorm=.01, chariqr=":", charamp="&",
                  pvpairs="std", pvypos=NULL, pvstinc=0.05, pvlab="p", 
                  pvprefix="p=", pvsize=NULL, chpvref="ref", pvspill=F,
                  pnosig=0.2, psignif=0.05, p2stars=0.01, p3stars=0.001, p4stars=0.0001,
                  pvnshide=T, pvtipl=0.01,
                  doAll=T, linetype="n", linesize=1, linecolor="red",
                  legend="top", size=NULL, width=NULL, yscale="none", letleft=T,nletbarmax=3,
                  xangle=NULL, yangle=NULL, orientation="vertical",
                  posd=NULL, binwfac=30, dotsize=NULL, dotshape=NULL,
                  theme="bw", fontfamily="sans", ymin=NA, ymax=NA,
                  fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                  fontxticks=NULL, fontyticks=NULL,
                  axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                  ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                  titlejust="center", legheadsize=12, legtextsize = 10,
                  ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                  fheight=NULL, dpi=300, remove=NULL) 
{
  cuf_apply_defaults(match.call(), environment())
  macmap = function() {
    mapinto12 = c(rep(-1,nlevint)) #; map12toin = c(rep(-1,nlevboth12))
    for (i in 1:nlevboth12) {
      j12 = match(g12names[i], ginnames)
      if (is.na(j12)) stop ("Can't understand:",g12names[i]," missing")
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
  notgroup = function(namegp) {stop(namegp,
     " cannot be a group variable as numeric with ",nlev," levels.",
     "\n    If it is the variable being analyzed, it should come first.",
     "\n    Or, did you mean to run cucov1way, cucov2way, culinreg?")
  }
  whatintend = function(namegp) {
    warning("ordinal ignored. Typo? Look at message at beginning")
    return(paste("\nordinal ignored. Did you intend to say g1order=",
                 "\n     or did you intend cu2way(",g1name,",",depname,",...?\n",sep=""))
  }
  get2x2 = function(i,j) {
    m11 = sum(df2[id1:idep,i])
    m21 = ifelse(jdep>idep, df2[jdep,i], as.integer(df[1,i]) - m11)
    m12 = sum(df2[id1:idep,j])
    m22 = ifelse(jdep>idep, df2[jdep,j], as.integer(df[1,j]) - m12)
    return(matrix(data=c(m11,m12,m21,m22),nrow=2,ncol=2))
  }
  estwelch = function(i1i2,j1i2) {
    n1 = npts[i1i2]; se1sq = sds[i1i2]**2/n1
    n2 = npts[j1i2]; se2sq = sds[j1i2]**2/n2
    mudif = means[j1i2]-means[i1i2]
    # cat(n1,n2,sds[i1i2],sds[j1i2],mudif)
    return(estwelout(mudif,se1sq,se2sq,n1-1,n2-1))
  }
  estwelout = function(mudif,se1sq,se2sq,ndf1,ndf2) {
    sedif = sqrt(se1sq+se2sq)
    if (sedif==0) {pvalt = ifelse(mudif==0,1,0); ndf=0}
    else {
      ndf = (se1sq+se2sq)**2/(se1sq**2/ndf1 +se2sq**2/ndf2)
      tstat = mudif/sedif; pvalt = 2 * stats::pt(-abs(tstat), ndf)
    }
    #cat(sedif,tstat,ndf,pvalt,"\n")
    cat(g1names[i1],": ", signif(mudif,3)," \u00B1 ",
        signif(sedif,3),", p=",signif(pvalt,3),
        ", CL=[",signif(mudif-1.96*sedif,3),",",
        signif(mudif+1.96*sedif,3),"]\n",sep="")
    return(c(pvalt,mudif,sedif,ndf))
  }
  partfout = function(fitlm,charstr) {
    ndf1 = fitlm$df; rse1 = stats::sigma(fitlm)
    pval = cupartialF(rse1,ndf1,rsez,ndfz)
    cat("\np=",signif(pval,3)," vs model with ",charstr,sep="")
    return(pval)
  }
  printdfAll = function(df,right=T) {
    if (Allfirst) print(df %>% relocate(.data$All), right=right)
    else print(df, right=right)
  }
  pvalstr = function(pval) {
    return(ifelse (pval > pnosig, "",
           ifelse (pval < 0.001, "<.001", round(pval,3))))
  }
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
  nrowmax = 16; irmean=3; irmed=9 # constants
  # minimal needs SD to do Bartlett and pairwise.t
  depsubdep = deparse(substitute(depvar))
  depsubg1 = deparse(substitute(group1)); depsubg2 = deparse(substitute(group2));
  if (is.numeric(group1)) {
    nlev = nlevels(as.factor(group1))
    if (nlev > 9) notgroup(depsubg1)
  }
  if (is.numeric(group2)) {
    nlev = nlevels(as.factor(group2))
    if (nlev > 9) notgroup(depsubg2)
  }
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
  if (ytrans != "none") if (is.factor(depvar) || ebars == 4)
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
  if (is.null(g1name)) g1name=depsubg1; if (is.null(g2name)) g2name=depsubg2
  if (is.character(depvar)) depvar = as.factor(depvar)
  # print(group1); print(group2) # need logic to test valid order's
  if (length(depvar) != length(group1) || length(depvar) != length(group2)) {
    cat("\n#observations of dependent variable and group factors not equal:",
        length(depvar), length(group1), length(group2),"\n")
    stop("\nquitting")
  }
  group1 = cu_reorder(group1, g1order); g1order = NULL
  group2 = cu_reorder(group2, g2order); g2order = NULL
  g1names = levels(group1); g2names = levels(group2);
  nlev1 = nlevels(group1); nlev2 = nlevels(group2);
  nlev11 = nlev1-1; nlev21 = nlev2-1;
  anyxmiss = F  # getting rid of subjects with any missing, not like 1way
  for (i in 1:length(depvar)) { 
    if (is.na(group1[i]) || is.na(group2[i])|| is.na(depvar[i])) {
      if (anyxmiss) cmiss = c(cmiss,i)
      else {cmiss = c(i); anyxmiss = T}
    }
  }
  if (anyxmiss) {
    cat("\nDropping observations",cmiss,"with missing data\n")
    group1 = group1[-cmiss]; group2 = group2[-cmiss]; depvar = depvar[-cmiss]; 
  }
  # find a way to deal with missing combinations of levels
  groupint = interaction(group1, group2, lex.order=T, sep=charamp)
  groupvar = factor(paste(group1, group2, sep=charamp))
  ginnames = levels(groupint); nlevint = nlevels(groupint) #used only in macmap
  g12names = levels(groupvar); nlevboth12 = nlevels(groupvar)
  # print(ginnames); print(g12names)
  macret = macmap()
  g12names = macret$A; mapinto12 = macret$B # ; map12toin= macret$C
  # print(ginnames); print(g12names)
  groupvar = factor(groupvar, levels=g12names)
  # print(levels(groupvar))
  # cat("\n"); print(mapinto12) #; cat("\n"); print(map12toin)
  #print(group1); print(group2); print(groupvar)
  nameboth12 = paste(g1name,charamp,g2name,sep="")
  if (is.null(title)) 
    title=paste(depnact, "at",nlevboth12,"levels of", nameboth12)
  if (!minimal) cat("\n",depname,"compared across",nlevboth12,nameboth12,"groups\n\n")
  # move cat( to later as in 1way?
  # don't have code as in 1way to show missing x's
  nlem = nlevboth12 # nlem = nlevels(groupvm)
  #cat("\n1,2,both",nlev1,nlev2,nlem,g1names,g2names,levels(groupvar))
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
  #df = cu_table0(depvar, groupvm, brief=brief, doAll=T, pnorm=pnorm, chariqr=chariqr)
  #colnames(df)=c(levels(groupvm),"All")
  brief = ifelse(is.factor(depvar), F, F) # check this first
  dfn = cu_table0(depvar, groupvar, brief=brief, doAll=doAll, pnorm=pnorm, chariqr=chariqr)
  df = dfn[[1]]; normTF = dfn[[2]]; pnormin = dfn[[3]]
  colnames(df)=c(g12names,"All")
  if (minimal) { # assume not factor, no missing groups
    lenrep = 2 + nlevboth12 # *(nlevboth12+1) # 2 for name, pnormin
    if (interact) lenrep = lenrep+ nlevboth12*(nlev1+nlev2-2)
    # n2*(g1 combns) + n1*(g2 combns) = n1n2*(n1+n2-1)  times 2
    else lenrep = lenrep + nlev1*(nlev1-1) + nlev2*(nlev2-1)
    # g1 combns + g2 combns) the whole thing times 2
    
    # if (!is.na(refmean)) lenrep = lenrep+nlevboth12
                   # each group's p-val vs refmean
    repout = vector(mode="character",length=lenrep); repn = rep("??",lenrep)
    repout[1] = depname; repn[1] = "Variable"; jrepo = 2
    # print(repou)
    dfnbr = cu_table0(depvar, groupvar, brief=T, doAll=doAll, pnorm=pnorm, chariqr=chariqr)
    dfbr = dfnbr[[1]] 
    for (ig1 in 1:nlevboth12) { #mean/median sd/iqr
      repout[jrepo] = dfbr[2,ig1]; repout[jrepo+1] = dfbr[3,ig1] 
      repnam = names(dfbr)[ig1]
      repn[jrepo] = paste(repnam,"_M",sep="")
      repn[jrepo+1] = paste(repnam,"_sd/iqr",sep="")
      # cat("\n",jrepo,repout[jrepo],repout[jrepo+1])
      jrepo = jrepo+2
    }
    # cat ("\nlenrep,jrepo:",lenrep,jrepo)
  }    
  if (!is.null(caption) && caption=="yes") {
    caption = "N in grps:"
    for (j in 1:nlevboth12) caption = paste(caption, as.integer(df[1,j])-as.integer(df[2,j]))  # nlem??
  }
  #cat("\ncaption:'",caption,"'",sep="")
  # print(depvar); print(groupvar); print(group1); print(group2)
  dsnomiss = stats::na.omit(data.frame(A=depvar,B=groupvar,C=group1,D=group2))
  if (nlevboth12 == nlevels(dsnomiss$B)) {
    if (!minimal)
     {cat(depname,"\n"); if (!is.factor(depvar)) printdfAll(df)}
  }
  else { #deal with no data for some combin(s); will screw up repout
    groupvar = dsnomiss$B
    g12names = levels(groupvar); nlevboth12 = nlevels(groupvar)
    # print(levels(groupint)); print(levels(groupvar))
    macret = macmap()  # check some time why this code is repeated
    g12names = macret$A; mapinto12 = macret$B # ; map12toin= macret$C
    groupvar = factor(groupvar, levels=g12names)
    # print(levels(groupvar))
    # cat("\n"); print(mapinto12) #; cat("\n"); print(map12toin)
    dfn = cu_table0(depvar, groupvar, brief=F, doAll=doAll, pnorm=pnorm, chariqr=chariqr)
    df = dfn[[1]]; normTF = ddfn[[2]]; pnormin = ddfn[[3]]
    colnames(df)=c(g12names,"All")
    if (!minimal) {
      cat(depname,"\n")
      printdfAll(df, right = F) # code should be checked - duplication?
    }
  }
  if (is.factor(depvar)) {
    #    desc = by(depvar, groupvar, summary.factor)
    df2 = data.frame(A = rep(0,nlevdep),stringsAsFactors = F)
    #    df2 = matrix(nrow=nlevels(depvar),ncol=nlevboth12)
    # why can't it be a matrix? no colnames for a matrix?
    rownames(df2) = levels(depvar)
    for (j in 1:nlem) {
      for (i in 1:nlevdep) df2[i,j]= as.integer(substr(df[i+2,j],regexpr("%",df[i+2,j])+3,10))
    }
    colnames(df2) = levels(groupvar); nlev = nlevboth12 #need?
    chifish = cu_chi2fish(df2,"");
    cat("\n"); print(df2)   # needed?
    if (minimal) casecon = T
    else {
      cat ("[Note that outcome is in rows (unlike in biomath.net/stat)\n")
      casecon = casecontrol
      df2mat = as.matrix(df2); colsum = rep(0,nlem)
      if (scale == "percent") for (j in 1:nlem) {
        colsum[j] = sum(df2mat[,j])
        if (colsum[j]>0) df2mat[,j] = 100*df2mat[,j]/colsum[j]
      }
      plotobj <- graphics::barplot(df2mat, xlab=nameboth12, ylab=depname,
                         main=paste(depname,"distribution at different levels of",nameboth12),
                         col=grDevices::rainbow(nlevdep), legend = rownames(df2),
                         args.legend = list(x = "topright", bty = "n"))
      if (scale == "percent") plotobj <- graphics::text(plotobj,102,labels=colsum,xpd=T)
      plotobj # cu_plout with barplot obj prints out x vector or NULL
      #cu_plout(plotobj,"curm",emf,suff=suff)
    }
    cat("\n", ifelse(casecontrol,"Odds Ratios","Relative Risks"),
        " (95% confidence limits) p-values for ",depname,sep="")
    #if (min(df2)==0) cat("\n   (cell counts bumped by 0.5 if any calculation gives 0 or \u221E)")
    ifpairwy = F; chleq = ""; pvminint = 1 # need pvminint here??
    if (nlevdep==2) ndeptodo = 1
    else if (ifordinal) {ndeptodo = nlevdep-1; chgtnot = ">"; deplevn = levels(depvar)[nlevdep]}
    else {ifpairwy = T;  ndeptodo = nlevdep;   chgtnot = "NOT"}
    # if ordinal, <= vs > for each level; else, vs NOT and vs every other
    for (idep in 1:ndeptodo) {
      id1 = ifelse(ifordinal,1,idep); deplevi = levels(depvar)[idep]
      for (jdep in idep:ifelse(ifpairwy,nlevdep,idep)) {
        cat("\n\n",ifelse(ifordinal&&(idep==nlevdep-1), paste("<",deplevn," vs ",deplevn,sep=""),
                  paste(chleq,deplevi," vs ", ifelse(nlevdep==2, levels(depvar)[2],
                   ifelse(jdep>idep, levels(depvar)[jdep], paste(chgtnot,deplevi))),sep="")),
            " in:",sep="")
        for (i2 in 1:nlev2) {  # factor 1 effects at each level of factor 2
          cat("\n ",g2names[i2])
          for (i1 in 1:nlev11) {  # comparing factor 1 levels
            iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
            if (i1i2 > 0) for (j1 in (i1+1):nlev1) {   # j1+i2 minus i1+i2
              jin = (j1-1)*nlev2 + i2; j1i2 = mapinto12[jin]
              #cat ("\ni2,i1,j1i2,i1i2",i2,i1,j1i2,i1i2)
              if (j1i2 > 0) cu_chi2fish(get2x2(i1i2,j1i2),
                  paste(g1names[i1]," vs ",g1names[j1],":",sep=""), casecontrol=casecontrol)
            }
          }
        }
        cat("\n ",g2name," comparisons within and between ",g1name," groups",sep="")
        for (i2 in 1:nlev21) {  # comparing factor 2 levels
          for (j2 in (i2+1):nlev2) {
            cat("\n ",g2names[i2],"vs",g2names[j2])
            for (i1 in 1:nlev1) { # at each factor 1 level, i1+j2 vs i1+i2
              iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
              jin = (i1-1)*nlev2 + j2; i1j2 = mapinto12[jin]
              #cat ("\ni2,i1,i1j2,i1i2",i2,i1,i1j2,i1i2)
              if (i1i2 > 0 && i1j2 > 0) cu_chi2fish(get2x2(i1i2,i1j2),
                       paste(g1names[i1],":",sep=""), casecontrol=casecontrol)
            }
            for (i1 in 1:nlev11) {  # between factor 1 levels
              iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
              jin = (i1-1)*nlev2 + j2; i1j2 = mapinto12[jin]
              if (i1i2 > 0 && i1j2 > 0) for (j1 in (i1+1):nlev1) {
                # (j1+j2 minus j1+i2)vs(i1+j2 minus i1+i2)
                iin = (j1-1)*nlev2 + i2; j1i2 = mapinto12[iin]
                jin = (j1-1)*nlev2 + j2; j1j2 = mapinto12[jin]
                if (j1i2 > 0 && j1j2 > 0) {
                  #cat ("\ni2,i1,j1,i1j2,i1i2,j1j2,j1i2",i2,i1,j1,i1i2,i1j2,j1i2,j1j2)
                  chi2f1 = cu_chi2fish(get2x2(i1i2,i1j2),"",casecontrol=casecontrol)
                  chi2f2 = cu_chi2fish(get2x2(j1i2,j1j2),"",casecontrol=casecontrol)
                  rror1 = chi2f1[3]; se1 = chi2f1[6]
                  rror2 = chi2f2[3]; se2 = chi2f2[6]
                  if (rror1 > 0 && rror2 > 0) {
                    difflog = log(rror1/rror2); sediff = sqrt(se1^2+se2^2)
                    pval = 2*(1-pnorm(abs(difflog),0,sediff))
                    cat("\n   ",g1names[i1]," vs ",g1names[j1],
                        ": log(",signif(rror1,3),")\u00B1",signif(se1,3),
                        " vs log(",signif(rror2,3),")\u00B1",signif(se2,3),
                        " p=",signif(pval,3),sep="")
                  }
                }
              }
            }
          }
        }
        if (ifordinal) chleq = "<="
      }
    }
    cat ("\n"); return(invisible())
  }  

  nlevnom = nlevels(dsnomiss$B) # not correct with missing combinations
  if (!minimal) {
    cat("\nSkewness, Kurtosis & Normality testing not done if SD=0 or n<6 or n>4000 (or for All).",
        "\nNormality by Shapiro-Wilk, p-value shown if <0.1\n\n")
    if (ebars !=4) {
      pnormin = 1; nfail = 0 #; print(df)
      for (j in 1:nlevboth12) { # logic from table0. should exclude missings
        pvalch = df[nrowmax,j] # ; cat ("\n",j," '",pvalch," '")
        if (!is.na(pvalch) && !is.null(pvalch) && pvalch!=" ") {
          if (regexpr("<",pvalch) > 0) 
          {pnormin = 0; nfail = nfail+1} # < means < 0.001
          else if ((pval=as.numeric(pvalch))<0.05) 
          {pnormin = min(pnormin,pval); nfail = nfail+1}
          # cat ("\n",j," '",pvalch,"' ",pnormin,sep="")
        }
      }
      if (nfail > 0) {
        cat("\nDATA FAIL NORMALITY TEST IN",nfail,"OF",nlevboth12,"GROUPs.",
            "SMALLEST P-VALUE",ifelse(pnormin==0,"<0.001",pnormin))
        if (ebars<=0) {ebars=4; cat("\nNONPARAMETRIC ANALYSIS WILL BE DONE.\n")}
        else cat("\nLOOK FOR DATA ERRORS IN 'Min' AND 'Max' VALUES.",
                 "\nIF DATA ARE NOT NORMAL,",
                 "\nYOU SHOULD USE THE NONPARAMETRIC DUNN TEST (ebars=4)\n")
      } # code parallels cu1way
      else if (ebars<=0) ebars = min(3,max(1,-ebars))
    } # end of ebars != 4
  } # end of !minimal
  if (ebars != 4) {
    pool.sd=T
    nlevact = 0; nmink = 0; chisq = 0; sumsq = 0; sumlnsq = 0; sumn1 = 0
    for (jg in 1:nlevboth12) {
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
    #bt=bartlett.test(depvar,groupvar)
    #bt$data.name = paste(depname,"across",nameboth12,"groups")
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
           "redo cu2way adding pbart=x, where x<",signif(pval,digits=3),"\n")
    }
    if (pool.sd) {
      pairout = cupairwise.t(dsnomiss$A,dsnomiss$B,p.adjust.method=padj)
      g <- factor(dsnomiss$B)
      s <- tapply(dsnomiss$A, g, stats::sd, na.rm = TRUE)
      n <- tapply(!is.na(dsnomiss$A), g, sum)
      degf <- n - 1
      total.degf <- sum(degf)
      pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
      pairout$data.name = paste(depname,"compared across",nlevnom,nameboth12,
        "groups",ifelse(padj=="none","using Fisher's LSD",""))
      pairout$method = paste(pairout$method,signif(pooled.sd,3))
    }
    else {
      pairout = cupairwise.t(dsnomiss$A,dsnomiss$B,p.adjust.method=padj,pool.sd=F)
      pairout$data.name = paste(depname,"compared across",nlevnom,
                                "groups by Welch's t (unequal variances)")
    }
    if (!minimal) {
      print(pairout)
      cat("Each p-value compares groups above and to the left\n")
    }
    coeffs = c("(Intercept)") # below loads g1names and g2names into coeffs
    if (interact) {
      zeros = nlevboth12; pvminint = 1
      #      for (j in 2:nlevboth12) {coeffs = c(coeffs,g12names[j])}
      fit = stats::lm(depvar ~ groupvar, na.action="na.exclude")
      fit$call = paste("lm(",depname," ~ ",g1name,"*",g2name,")",sep="")
      names(fit$coefficients) = c("(Intercept)",g12names[2:nlevboth12])
    }
    else {
      fit = stats::lm(depvar ~ group1 + group2)
      fit$call = paste("lm(",depname," ~ ",g1name,"+",g2name,")",sep="")
      for (i1 in 2:nlev1) {coeffs = c(coeffs,paste(g1name,g1names[i1],sep=""))}
      for (i2 in 2:nlev2) {coeffs = c(coeffs,paste(g2name,g2names[i2],sep=""))}
      names(fit$coefficients) = coeffs
      zeros = nlev1+nlev2-1; endg01 = 0; endg11 = endg01+nlev11; endg21 = zeros-1
    }
    if (!minimal) print(summary(fit))
    if (pool.sd) if (interact) { #ok to use regression for contrasts
      if (!minimal) cat(g1name," comparisons of ",depname,
              " within each ",g2name," group\n\n",sep="")
      for (i2 in 1:nlev2) {  # factor 1 effects at each level of factor 2
        if (!minimal) cat(g2names[i2],"\n")
        for (i1 in 1:nlev11) {  # comparing factor 1 levels
          iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
          if (i1i2 > 0) for (j1 in (i1+1):nlev1) {   # j1+i2 minus i1+i2
            jin = (j1-1)*nlev2 + i2; j1i2 = mapinto12[jin]
            if (j1i2 > 0) {
              vec = c(rep(0,zeros)); if (i1i2 > 1) vec[i1i2] = -1
              if (j1i2 > 1) vec[j1i2] = 1
              if (!minimal) cat(g1names[j1],"minus",g1names[i1])
              pvrepo = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
              if (minimal) {
                repn[jrepo] = paste(g2names[i2],":",
                          g1names[j1]," v ",g1names[i1],sep="")
                repout[jrepo] = pvalstr(pvrepo); jrepo = jrepo+1
              }
            }
          }
        }
        if (!minimal) cat("\n")
      }
      if (!minimal) cat("\n",g2name," comparisons of ",depname,
          " within and between ",g1name," groups\n\n",sep="")
      for (i2 in 1:nlev21) {  # comparing factor 2 levels
        for (j2 in (i2+1):nlev2) {
          if (!minimal) cat(g2names[j2],"minus",g2names[i2],"\n")
          for (i1 in 1:nlev1) {        # at each level of factor 1
            iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
            jin = (i1-1)*nlev2 + j2; i1j2 = mapinto12[jin]
            if (i1i2 > 0 && i1j2 > 0) { # i1+j2 vs i1+i2
              vec = c(rep(0,zeros)); if (i1i2 > 1) vec[i1i2] = -1
              if (i1j2 > 1) vec[i1j2] = 1
              if (!minimal) cat(g1names[i1])
              pvrepo = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
              if (minimal) {
                repn[jrepo] = paste(g1names[i1],":",
                         g2names[j2]," v ",g2names[i2],sep="")
                repout[jrepo] = pvalstr(pvrepo); jrepo = jrepo+1
              }
            }
          }
          for (i1 in 1:nlev11) {  # between factor 1 levels
            iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
            jin = (i1-1)*nlev2 + j2; i1j2 = mapinto12[jin]
            if (i1i2 > 0 && i1j2 > 0) for (j1 in (i1+1):nlev1) {
              # (j1+j2 minus j1+i2)vs(i1+j2 minus i1+i2)
              iin = (j1-1)*nlev2 + i2; j1i2 = mapinto12[iin]
              jin = (j1-1)*nlev2 + j2; j1j2 = mapinto12[jin]
              if (j1i2 > 0 && j1j2 > 0) {
                vec = c(rep(0,zeros)); if (i1i2 > 1) vec[i1i2] = 1
                if (i1j2 > 1) vec[i1j2] = -1
                if (j1i2 > 1) vec[j1i2] = -1
                if (j1j2 > 1) vec[j1j2] = 1
                if (!minimal) cat(g1names[j1],"minus",g1names[i1])
                pvrepo = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
                if (minimal) {
                  repn[jrepo] = paste(g2names[j2]," v ",g2names[i2],
                                  ":",g1names[j1], "-", g1names[i1],sep="")
                  repout[jrepo] = pvalstr(pvrepo); jrepo = jrepo+1
                }
                pvminint = min(pvminint, pvrepo)
              }
            }
          }
          if (!minimal) cat("\n")
        }
      }
      if (!minimal) {
        cat("Note:",g2name,"differences between",g1name,
          "groups are same as\n     ",g1name,"differences between",g2name,
          "groups\ne.g.,",g2names[nlev2],"minus",g2names[nlev21],"between",
          g1names[nlev1],"and",g1names[nlev11],"is identical to\n     ",
          g1names[nlev1],"minus",g1names[nlev11],"between",
          g2names[nlev2],"and",g2names[nlev21],"\n")
        if (pvminint > 0.05) 
        cat("Smallest interaction p-value is",signif(pvminint,3),
            "\nYou may wish to try a non-interaction anova (interact=F)\n")
      }
    }  # end of interact=T
    else {
      if (!minimal) cat(g1name," comparisons of ",
                        depname,"\n\n",sep="")
      for (i in 1:nlev11) {
        for (j in (i+1):nlev1) {
          vec=c(rep(0,zeros)); vec[j+endg01] = 1; if (i>1) vec[i+endg01] = -1
          if (!minimal) cat(g1names[j], " minus ", 
                            g1names[i],sep="")
          pvrepo = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
          if (minimal) {
            repn[jrepo] = paste(
                         g1names[j]," v ",g1names[i],sep="")
            repout[jrepo] = pvalstr(pvrepo); jrepo = jrepo+1
          }
        }
      }
      if (!minimal) cat("\n",g2name," comparisons of ",
                        depname,"\n\n",sep="")
      for (i in 1:nlev21) {
        for (j in (i+1):(nlev2)) {
          vec=c(rep(0,zeros)); vec[j+endg11] = 1; if (i>1) vec[i+endg11] = -1
          if (!minimal) cat(g2names[j], " minus ",
                            g2names[i],sep="")
          pvrepo = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
          if (minimal) {
            repn[jrepo] = paste(
              g2names[j]," v ",g2names[i],sep="")
            repout[jrepo] = pvalstr(pvrepo); jrepo = jrepo+1
          }
        }
      }
      if (!minimal) cat("\n")
    }  # end of interact=F
    else { # unequal variances, no pooled.sd
      cat(nameboth12," comparisons of ",depname,
          " using Welch's t for unequal variances\n\n",sep="")
      cat(g1name," comparisons of ",depname,
          " within each ",g2name," group\n\n",sep="")
      desc = by(dsnomiss$A,dsnomiss$B,cusum.default)
      npts = c(0); means = c(0); sds = c(0)
      for (i in 1:nlevnom) { # df has limited precision, so rounding errors
        npts[i] = as.numeric(desc[[i]][1]) - as.numeric(desc[[i]][2])
        means[i] = as.numeric(desc[[i]][3]); sds[i] = as.numeric(desc[[i]][4])
      }
      #print(npts);print(means);print(sds); cat("\n")
      pvminint = 1
      for (i2 in 1:nlev2) {  # factor 1 effects at each level of factor 2
        cat(g2names[i2],"\n")
        for (i1 in 1:nlev11) {  # comparing factor 1 levels
          iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
          if (i1i2 > 0) for (j1 in (i1+1):nlev1) {   # j1+i2 minus i1+i2
            jin = (j1-1)*nlev2 + i2; j1i2 = mapinto12[jin]
            if (j1i2 > 0) {
              cat(g1names[j1], " minus "); estwelch(i1i2,j1i2)
            }
          }
        }
        if (!minimal) cat("\n")
      }
      savmud = c(0); savsed = c(0); savndf = c(0)
      cat("\n",g2name," comparisons of ",depname,
          " within and between ",g1name," groups\n\n",sep="")
      for (i2 in 1:nlev21) {  # comparing factor 2 levels
        for (j2 in (i2+1):nlev2) {
          cat(g2names[j2],"minus",g2names[i2],"\n")
          for (i1 in 1:nlev1) {        # at each level of factor 1
            iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
            jin = (i1-1)*nlev2 + j2; i1j2 = mapinto12[jin]
            if (i1i2 > 0 && i1j2 > 0) { # i1+j2 vs i1+i2
              welret = estwelch(i1i2,i1j2); savmud[i1] = welret[2]
              savsed[i1] = welret[3]; savndf[i1] = welret[4]
            }
          }
          for (i1 in 1:nlev11) {  # between factor 1 levels
            iin = (i1-1)*nlev2 + i2; i1i2 = mapinto12[iin]
            jin = (i1-1)*nlev2 + j2; i1j2 = mapinto12[jin]
            if (i1i2 > 0 && i1j2 > 0) for (j1 in (i1+1):nlev1) {
              # (j1+j2 minus j1+i2)vs(i1+j2 minus i1+i2)
              iin = (j1-1)*nlev2 + i2; j1i2 = mapinto12[iin]
              jin = (j1-1)*nlev2 + j2; j1j2 = mapinto12[jin]
              if (j1i2 > 0 && j1j2 > 0) {
                se1sq = savsed[i1]**2; se2sq = savsed[j1]**2
                mudif = savmud[j1]-savmud[i1]
                cat(g1names[j1], " minus ")
                welret = estwelout(mudif,se1sq,se2sq,savndf[i1],savndf[j1])
                pvminint = min(pvminint,welret[1])
              }
            }
          }
          if (!minimal) cat("\n")
        }
      }
      if (interact && pvminint > 0.05)
        cat("Smallest interaction p-value is",signif(pvminint,3),
            "\nYou may wish to try a non-interaction anova (interact=F)\n")
    } # end unequal variances
    if (minimal) {
      repout[jrepo] = ifelse(pnormin<=pnorm,pnormin," ")
      repn[jrepo] = "pnormin"
      # cat("\nlenrep,jrepo,repn,repout:",lenrep, jrepo,"\n")
      names(repout) = repn  #; print(repout)
      return(repout)
    }
    pvalues = pairout$p.value
    if (partialF) {
      cat("Partial F-test vs simpler models:")
      ndfz = fit$df; rsez = stats::sigma(fit)
      if (interact) {
        fitni = stats::lm(depvar ~ group1+group2, na.action="na.exclude")
        pvalni = partfout(fitni, paste("no interaction - cu2way(",depsubdep,",",
                                       depsubg1,",",depsubg2,",interact=F)",sep=""))
      }
      else pvalni = 0
      fitg1 = stats::lm(depvar ~ group1, na.action="na.exclude")
      fitg2 = stats::lm(depvar ~ group2, na.action="na.exclude")
      pval1 = partfout(fitg1, paste("just ",g1name," - cu1way(",depsubdep,",",
                                    depsubg1,")",sep=""))
      pval2 = partfout(fitg2, paste("just ",g2name," - cu1way(",depsubdep,",",
                                    depsubg2,")",sep=""))
      if (pvalni > 0.05 || pval1 > 0.05 || pval2 > 0.05)
        cat("\nModel is overly complex. Consider a simpler model")
      else if (interact && pvminint > 0.05)
        cat("\nbut smallest interaction p-value is",signif(pvminint,3),
            "\nYou have a dilemma with a significant partial F-test,",
            "\nbut without any interaction reaching significance.")
      cat("\n")
    }
  }
  else {  # ebars==4
#    oldw <- getOption("warn")
#    options(warn = -1)
#    pairout = kwAllPairsDunnTest(depvar,groupvar,p.adjust.method="none")
#    pairout$data.name = paste(depname,"compared across",nameboth12,"groups")
#    print(pairout)
#    options(warn = oldw)
#    pvalues = pairout$p.value
    suppressMessages(utils::capture.output(dunout <- dunn.test(dsnomiss$A,dsnomiss$B, method=padj, altp=T, table=F, kw=F)))
    idu=0; nm1 = nlevnom-1; pvalues = matrix(c(rep(0,nm1*nm1)),nrow=nm1,ncol=nm1)
    for (i in 2:nlevnom) {  # 2/25 check that the logic is correct
      for (j in 1:(i-1)) {idu=idu+1; pvalues[i-1,j] = dunout$altP[idu]}
    }
    if (!minimal) cat("\n         Pairwise comparisons of ",depname,
                      " using Dunn's all-pairs test\n\n")
    g12names = levels(dsnomiss$B) # used in Dunn to print out p-values
    maxnch = min(12,max(nchar(g12names)))
    if (maxnch>7) cat(rep(" ",maxnch-7),sep="")
    for (j in 1:nlevnom-1) {
      cat(substr(paste(g12names[j],"       "),1,7)," ",sep="")
    }
    for (i in 2:nlevnom) {
      cat("\n",substr(paste(g12names[i],"            "),1,maxnch)," ",sep="")
      for (j in 1:(i-1)) {
        pval = pvalues[i-1,j]
        if (pval<0.001) {cat(sprintf("%.1E",pval)," ",sep="")}
        else cat(sprintf("%.4f",pval)," ")
      }
    }
    if (!minimal) cat("\n\nP value adjustment method:", padj,
        "\nEach p-value compares groups above and to the left\n\n")
  }
  if (!minimal && (plot!="no")) {
    mapord = cu_mapor2(g1order,g1names,nlev1,nlev2,nlevnom)
    #cat ("\npreletbar",nrow(pvalues),nlevnom)
    letbar = cu_letbar(pvalues,psigcld,mapord,nrow(pvalues)+1,letleft=letleft,nletbarmax=nletbarmax)
    #noneed: if (psigcld > 0) letbar = cu_letbugfix(letbar,g1name,nlev1,nlev2)
    # cat ("\npostletbar")
    pvstruc = cu_pvline(df,pvpairs,pvalues,nlev1,nlev2,pvlab,pnosig,psignif,p2stars,p3stars,p4stars,
                        letbar,pvypos,pvnshide,pvspill,chpvref,ebars,dots,yscale)
    # no good if nlevnom < nlev1*nlev2 due to missing combinations
    # have cutable0 put in columns for missing groups
    #print(pvstruc)
    pvdf = pvstruc[[1]]; pvypos = pvstruc[[2]]; pvlab = pvstruc[[3]]
    yrange = pvstruc[[4]]; letbar = pvstruc[[5]]
    #cat("\npvypos,pvdf\n"); print(pvdf); print(pvypos)
    if (itrans==2) {
      #cat("\nddep:\n"); print(ddep)
      for (i in 1:length(depvar)) if (!is.na(depvar[i])) depvar[i] = 10**depvar[i]
      #cat("\nddep:\n"); print(ddep)
      yrange = 10**yrange; yscale = "log10"; ebars = 4  # to get median and iqr
    }
    else if (yscale=="log10") ebars = 4  # to get median and iqr
    if (is.null(pvtipl)) pvtipl = yrange*1e-5
    p <- cu_plot2("cu2way",depvar,group1,group2,nlev1,nlev2, g1order, g1name, depnact, g2name, title,
                  plot=plot, linetype=linetype, linesize=linesize,
                  letbar=letbar, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                  pvdf=pvdf,pvypos=pvypos,pvstinc=pvstinc,pvlab=pvlab,pvprefix=pvprefix,
                  pvsize=pvsize, pvnshide=pvnshide,pvtipl=pvtipl,
                  caption=caption, legend=legend, size=size, width=width, posd=posd,
                  xangle=xangle, yangle=yangle, 
                  orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotshape=dotshape,
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
