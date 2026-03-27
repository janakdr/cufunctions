#' Does repeated measures one- or two-way anova and post-hoc tests (format wide or long - 1/many lines for each subject)
#' @param dsgiven ,dnam,repnam required: dataset, char-strings of names of variable being analyzed, repeated grouping factor
#' @param fac2 char-string of name of 2nd, non-repeated grouping factor (default NULL)
#' @param idnam (to indicate long format) =char-string of name of ID/Subject variable (default NULL)
#' @param minimal = FALSE (default), T or TRUE to suppress all output (for cuomics)
#' @param interact =TRUE (default), F or FALSE for no interaction
#' @param partialF =TRUE (default), F or FALSE for no partial F vs simpler models
#' @param cov =char-string of covariate model (default NULL)
#' @param ytrans ="none" (default) to transform depvar: "sqrt" "log"
#' @param maxlines =0 (default) for no line plot, 1 for line plots, n to limit #lines on each to n
#' @param mainx =1 (default), 2 to interchange the two factors in bar graph
#' @param sumdiff =1 (default) or -1 for difference summary and bar graph, 0 for not (1 for rest-Igroup, -1 for Igroup-rest)
#' @param scale ="frequency" (default) or "percent"
#' @param scatter = "yes" (default) for scatter plots of repeated measures, "no" for none
#' @param ebars =0 (default to be 1 or 4)/1/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR) or -N (nonparm if any norm fail)
#' @param dots =0 (default), 1 to display data on graph
#' @param barcolor for outline color, barfill for fill color. Use barfill="colors" for colors by group.
#' @param plot ="bar" (default) for bar graphs; "box" "violin" "rod" "no"
#' @param ordinal =NULL (default)/c("...") to reorder dependent variable
#' @param g1order ,g2order, difforder=NULL (defaults)/c("...") to reorder bars (I Factor, Differences)
#' @param depname /g1name/g2name/title to override names of depvar, groupvar, fac2, title
#' @param pnorm =0.05 (default) threshold for normality test
#' @param psigcld =0 (no letters)/x for CLD letters on bars (any shared letter means P>psigcld, 0 for no letters)
#' @param conf.int =0.95 (default)/x/0 for confidence interval width of contrast estimates (0 for none)
#' @param ddepn =NULL (default) for name of dependent variable difference
#' @param shape =16 (default)/n for shape of dots in regression plot
#' @param caption =NULL (default)/"yes"/"caption text" to get caption ("yes" to list n's)
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
#' @param ifdeb =F (default)/T to suppress debug output or not
#' @param refmean =NA (default)/ref.val to compare each group mean with
#' @param normvisit =0 (default)/i to specify the i-th visit as the reference
#' @param normway ="fold" (default), "sub","pct" to indicate how data get normalized to normvisit value
#' @param rmwall =NULL set to dataset to be updated for writing out by cuomics
#' @param needvar =0/the number of other variables to include in rmwall
#' @param linetype ="n" (default)/x for no connecting lines ("solid" "dashed" "dotted" "blank" "longdash" "dotdash" "twodash")
#' @param linecolor ="black" (default)/x for black lines ("red" etc) (only for single factor)
#' @param linesize =1 (default)/x for line thickness
#' @param theme ="bw" (default)/x for white background (or "gray" or "classic" (no grid lines))
#' @param legend ="top" (default), "bottom", "right", "left" to locate legend
#' @param size numeric value (e.g. size=1), to change size of points and outlines
#' @param width numeric value between 0 and 1 specifying box width
#' @param yscale (default="none"), can be "log2", "log10", "sqrt"
#' @param xangle /yangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param orientation (default="vertical"), can be "horizontal" or "reverse"
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
#' @param dodredge =T (default) to get (T) or not get (F) all possible models
#' @param usemod =1 (default) to use the usemod-th best found by dredge
#' @param nmodshow =16 (default) to show top nmodshow models by dredge
#' @param nverscat,nhorscat =2 (default) #scatterplots vert,horiz
#' @param m.min and m.max= to set smallest and largest model sizes in dreedge
#' @param fixed to specify variables that must be in model
#' @param subset logical expression describing models to keep in dredge
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return one line of summary means/SDs and p-values, data if normvisit
#' @examples
#' \dontrun{
#' curepmeas(delta,"TG","Diet")  # one line per subject; TG.xx TG.yy etc as names of TG data
#' curepmeas(delta,"TG","Diet", "sex")  # with sex as second factor, Diet*sex interaction in model
#' curepmeas(delta,"TG","Diet", "sex", interact=F)  # no Diet*sex interaction
#' curepmeas(delta,"TG","Diet", "sex", cov="age+sex*age")  # with covariate adjustment
#' }
#' @export
curepmeas = function(dsgiven, dnam, repnam, fac2=NULL, idnam=NULL, minimal=F,
                     interact=TRUE, partialF=TRUE, cov=NULL, ytrans="none",
                     maxlines=0, mainx=1, sumdiff=1, scale="frequency", scatter="yes",
                     ebars=0, dots=0, barcolor="black", barfill=NULL, plot="bar",
                     ordinal=NULL, g1order=NULL, g2order=NULL, difforder=NULL, 
                     depname=NULL, g1name=NULL, g2name=NULL, title=NULL, pnorm=0.05,
                     psigcld=0, conf.int=0.95, ddepn=NULL, shape=16, caption=NULL,
                     pvpairs="std", pvypos=NULL, pvstinc=0.05, pvlab="p", 
                     pvprefix="p=", pvsize=NULL, chpvref="ref", pvspill=F,
                     pnosig=0.2, psignif=0.05, p2stars=0.01, p3stars=0.001, p4stars=0.0001,
                     pvnshide=T, pvtipl=0.01, ifdeb=F, refmean=NA,
                     normvisit=0, normway="fold", rmwall=NULL, needvar=0,
                     linetype="n", linecolor="black", linesize=1, theme="bw",
                     legend="top", size=NULL, width=NULL, yscale="none",
                     xangle=NULL, yangle=NULL, orientation="vertical",
                     posd=NULL, binwfac=30, dotsize=NULL, dotshape=NULL, dotcolor=NULL,
                     fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                     fontxticks=NULL, fontyticks=NULL,
                     axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                     ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                     dodredge=T, usemod=1, nmodshow=16,
                     nverscat=2, nhorscat=2,
                     m.min=1, m.max=99, fixed=NULL, subset=as.expression(1),
                     ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                     fheight=NULL,dpi=300,remove=NULL) 
{
  cuf_apply_defaults(match.call(), environment())
  if (is.null(plot)) {plot = "no"}
  else if (plot %in% c("n","no","N","NO","No")) {plot="no"}
  else if (!(plot %in% c("bar","box","violin","rod"))) {
    cat("\nplot='",plot,"' no good. Taken to be 'bar'",sep=""); plot="bar"
  }
  dolines = function(xvar, yvar, id, title) {
    nsubj = length(xvar)/nleva; nsdone = 0; ide = 0; cat("\nnsubj:",nsubj)
    while (nsdone < nsubj) {
      ideo = ide; nlines = nleva*min(maxlines,nsubj-nsdone)
      cat ("\nideo,nsdone,nlines", ideo,nsdone,nlines)
      dfpl = data.frame(A=c((nlines)*0), stringsAsFactors = F); j=0; nid=0
      for (i in 1:length(xvar)) { # id's assumed monotonic up the first time through
        idi = id[i]
        if (idi>=ideo) {
          if (idi>ide && nid<maxlines) {
            if (ideo == ide) idb = idi # starting id for the graph
            ide = idi; nid = nid+1
          }
          if (idi<=ide) {
            j = j+1; dfpl[j,1] = xvar[i]; dfpl[j,2] = yvar[i]; dfpl[j,3] = idi
          }
        }
      }
      nsdone = nsdone+nid; colnames(dfpl) = c(g1name,depname,"id")
      cat("\nlines dfpl:\n")
      # if (!is.null(g1order)) dfpl[,1] = factor(dfpl[,1], levels=g1order)
      print(dfpl)
      titl = paste(title," (",idb,"-",ide,")",sep="")
      p <- ggplot(dfpl) + aes(dfpl[,1],dfpl[,2],group=id)+geom_point()+geom_line()
      p <- p +labs(title=titl,x=g1name,y=depname) +
           theme(plot.title = element_text(hjust=0.5)) # labs must come at end
      cu_plout(p,"curepmeas-lines",suff=title,seq=idb, ftype=ftype, fname=fname,
               scale=fscale, width=fwidth, height=fheight, dpi=dpi,remove=remove)
      
    }
  }  # end dolines
  anyzneg = function(depv) {
    izneg = 1
    for (j in 1:length(depv)) {
      depj = depv[j]
      if (!is.na(depj) && is.numeric(depj)) if (depj<0) {izneg = -1; break}
      else if (depj==0) izneg=0
    }
    #cat("\nj,izneg",j,izneg)
    return(izneg)
  }  # end anyzneg
  nsubdrop = 0
  dvardot = paste(dnam,".",sep=""); dvarnc1 = nchar(dnam)+1; varlist = c()
  if (is.null(idnam)) { # wide format input
    rmw = dsgiven; dslenw = length(rmw)  # number of columns
    for (i in 1:dslenw) {
      if (substr(names(rmw[i]),1,dvarnc1)==dvardot) {
        if (is.null(varlist)) iffacdep = is.character(rmw[,i]) || is.factor(rmw[,i])
        if (iffacdep) if (is.character(rmw[,i])) rmw[,i] = as.factor(rmw[,i])
        varlist=c(varlist,i) #; cat("\ni,iffacdep",i,iffacdep)
      }
    }
    if (is.null(varlist)) {
      stop("\n",dnam,":No variable names begin with <",dvardot,
           "> Check and re-run\n", sep=""); return()
    }
  }
  else { # long format input
    rml = dsgiven; dnum = 0; rnum = 0; inum = 0
    for (i in 1:length(rml)) {
      nam = names(rml[i])
      if (nam==dnam) {
        dnum=i
        iffacdep = is.character(rml[,i]) || is.factor(rml[,i])
        if (is.character(rml[,i])) rml[,i] = as.factor(rml[,i]) # needed?
      }
      if (nam==repnam) rnum=i
      if (nam==idnam)  inum=i
    }
    if (dnum==0) stop("\n",dnam,":No variable name <",dnam,"> Check and re-run\n", sep="")
    if (rnum==0) stop("\n",dnam,":No variable name <",repnam,"> Check and re-run\n", sep="")
    if (inum==0) stop("\n",dnam,":No variable name <",idnam,"> Check and re-run\n", sep="")
    if (ifdeb) {cat ("\nlong rml:"); print (rml)}
    oldw <- getOption("warn"); options(warn = -1)
    rmw = reshape(dsgiven,timevar=repnam,idvar=idnam,direction="wide",v.names=dnam)
    options(warn = oldw)
    if (ifdeb) {cat("\nlong rmw:"); str(rmw); cat("\n"); print(rmw)}
    for (i in 1:length(rmw)) {
      if (substr(names(rmw[i]),1,dvarnc1)==dvardot) {
        varlist=c(varlist,i) #; cat("\ni,iffacdep",i,iffacdep)
      }
    }
  }
  nleva = length(varlist); if (ifdeb) cat("\nvarlist:",varlist)
  if (nleva <= 1) {
    stop("\n",dnam,":",normvisit,
         "no good. Repeating factor has only one level (visit)")
    return()
  }
  if (normvisit > 0) if (normvisit > nleva) {
    stop("\n",dnam,":normvisit",normvisit,
         "no good. Repeating factor has only",nleva,"levels (visits)")
    return()
  }
  else {
    normloc = varlist[normvisit]; if (ifdeb) cat("\nvarlist:",varlist)
    varlist <- varlist[-normvisit]; nleva = nleva-1
    if (ifdeb) cat("\nvarlist:",varlist)
    if (normway=="sub") {chdpf = "D"; refmean = 0} # expression(Delta)
    else if (normway=="pct") {chdpf = "D%"; refmean = 0}
    else {chdpf = "fold"; refmean = 1} # "??" is "backslash u0394"
    if (!is.null(rmwall)) ncolall = ncol(rmwall) # not first time
    else {ncolall = max(needvar,1); rmwall = rmw[,1:ncolall]}
    if (ifdeb) {
      cat("\nchdpf,refmean,ncolall,nrmwall:",chdpf,refmean,ncolall,"\n")
      print(rmwall)
    }
    for (jv in 1:nleva) {
      jloc = varlist[jv]; colnam = paste(names(rmw)[jloc], chdpf,sep="")
      if (ifdeb) cat("\ncolumn",jloc,"'",colnam,"'")
      names(rmw)[jloc] = colnam
      rmwall = cbind(rmwall,NA); names(rmwall)[ncolall+jv] = colnam
    }
    if (ifdeb) {cat("\nafter colnames, rmw:\n"); print(rmw) }
    nsubj = nrow(rmw); rowsdrop = NULL
    if (nsubj != nrow(rmwall)) {
      stop("\n",dnam,":subject count not from prev:",nsubj,nrow(rmwall))
      return()
    }
    for (isubj in 1:nsubj) {
      normval = rmw[isubj,normloc]
      if (ifdeb) if (isubj<10) cat ("\nsubj,norm,vals",isubj,normval)
      if (is.na(normval) || ((normval==0) && (normway!="sub"))) 
        {nsubdrop = nsubdrop+1; rowsdrop = c(rowsdrop,isubj) }
      else for (jv in 1:nleva) {
        jloc = varlist[jv]; val = rmw[isubj,jloc]
        valm = ifelse(normway=="sub",val-normval,
               ifelse(normway=="pct",100*(val/normval-1), val/normval))
        rmw[isubj,jloc] = valm; rmwall[isubj,ncolall+jv] = valm
        if (ifdeb) if (isubj<10) cat(jloc,val,valm)
      }
    }
    if (ifdeb) {cat("\nnormvisit,nleva,nrmwall:",normvisit,nleva,"\n"); print(rmwall) }
    if (normvisit>0) if (nleva <= 1) # 1 data visit, just write out
      return(list(NULL,rmwall))
    if (ifdeb) cat ("\nstill not returned??\n")
    if (nsubdrop > 0) {
      # cat("\nrowsdrop:",rowsdrop,"\nrmw before drop:\n"); print(rmw)
      rmw = rmw[-rowsdrop, ]
      # cat("\nrmw after drop:\n"); print(rmw)
    }
  }
  if (is.null(idnam) || normvisit>0) { # wide format input or normalizn
    rml = reshape(rmw, varying=c(varlist), timevar=repnam, direction="long")
    for (i in 1:length(rml)) {if (names(rml[i])=="id") inum=i}
  }
  nsubj = nrow(rmw); dslenl = length(rml) # length is number of columns
  if (ifdeb) {
    cat("\nafter reshape, nsubj,dslenl,nrowrml,inum,refmean:",
        nsubj,dslenl,nrow(rml),inum,refmean,"\nrmw:\n")
    print(rmw); cat("\nrml:\n"); print(rml)
  }
  # cat ("\nafter refmean:\n")
  dnum = 0; g2col = 0; g2cow = 0 # rml and rmw may have 2nd factor in diff cols
  for (i in 1:dslenl) {
    nam = names(rml[i])
    if (nam==repnam) {fctr = cu_reorder(rml[,i], g1order); g1order = NULL}
    else if (nam==dnam) dnum=i
    else if (!is.null(fac2)) if (nam==fac2) 
      {g2col=i; rml[,i] = cu_reorder(rml[,i], g2order)} #later: g2order = NULL
  }
  if (!is.null(fac2)) {
    if (g2col==0) stop("\nNo variable name <",fac2,"> Check and re-run\n", sep="")
    for (i in 1:length(rmw)) {if (names(rmw[i])==fac2) g2cow = i}
    if (g2cow==0) stop("How come",fac2," is not in rmw??")
  }
  itrans = 0 # unnecessary for factor but need while setting depname
  if (iffacdep) {
    nlevdep = nlevels(rml[,dnum]); levsdep = levels(rml[,dnum])
    # cat("\nsubj,nlevdep",nsubj,nlevdep); print(levsdep)
    if (!is.null(ordinal)&&!is.logical(ordinal)) {
      if (length(ordinal) != nlevdep) {
        i = 0; 
        cat("\nordinal must have exactly",nlevdep,depname,"names. ordinal ignored\n")
        warning("ordinal must have exactly ",nlevdep," ",depname," names. ordinal ignored")
      }
      else for (i in (1:nlevdep)) {
        if (!(levsdep[i] %in% ordinal)) {
          cat("\n",depname," '",levsdep[i],"' not in ordinal. ordinal ignored\n",sep="")
          warning(depname," '",levsdep[i],"' not in ordinal. ordinal ignored")
          i = 0; break
      } }
      if (i>0) {rml[,dnum] = factor(rml[,dnum], levels=ordinal); levsdep=ordinal}
    } # reorder depvar before tables and barplot
    if (scale != "frequency") if (scale != "percent") {
      cat("\n scale can only be frequency or percent, in quotes\n")
      scale="frequency"
    }
    chyparr = "->"
  }
  else {
    chyparr = "-"
    # cat("\nrml[,dnum] before:",rml[,dnum])
    if (ytrans != "none") if (ebars == 4)
      {cat("\nytrans ignored for contingency or ebars=4.\n"); ytrans="none"} # no need for transforms
    else {
      izneg = anyzneg(rml[,dnum])
      if (ytrans=="sqrt") if (izneg<0)
        {cat("\nNo sqrt if any negative values.\n"); ytrans="none"}
      else itrans=1
      else if (ytrans=="log" || ytrans=="log10") if (izneg<=0)
        {cat("\nNo log unless all positive values.\n"); ytrans="none"}
      else itrans=2
      else {cat("\nytrans no good:",ytrans,"\n"); ytrans="none"}
      if (itrans>0) for (j in 1:nrow(rml)) {
        depj = rml[j,dnum]
        if (!is.na(depj)) rml[j,dnum] = ifelse(itrans==1,sqrt(depj),log10(depj))
      }
    }
    # cat ("\nbefore scatter\n")
    # cat("\nrml[,dnum] after:",rml[,dnum])
    if (!minimal && scatter != "no") {
      #cat("\nlevels(fctr)"); print(levels(fctr))
      #cat("\nvarlist,colnames:"); print(varlist); print(colnames(rmw))
      varlism = varlist; nrepm = length(varlist)
      for (ix in 1:nrepm) {
        dvtest = paste(dvardot,levels(fctr)[ix],sep="")
        for (j in 1:nrepm) {
          jj = varlism[j]
          if (dvtest == colnames(rmw)[jj]) {varlist[ix] = jj; break}
        }
      }
      #cat("\nvarlist:"); print(varlist)
      cat("\nScatter plots of repeated measures should have slopes close to 1.")
      if (g2cow>0) cat("\n   (separate slopes for",fac2,"=",levels(as.factor(rmw[,g2cow])),")")
      nscat = 0
      for (i in 1:(nrepm-1)) for (j in (i+1):nrepm) {
        ix = varlist[i]; iy = varlist[j] #; cat("\nix,iy",ix,iy)
        yname=colnames(rmw)[iy]; xname=colnames(rmw)[ix]
        scatitle = paste(substr(yname,dvarnc1+1,nchar(yname)),
                         "vs",substr(xname,dvarnc1+1,nchar(xname)))
        if (g2cow == 0) scaout = cuscatter(rmw[,iy], rmw[,ix], minimal=T,
                             yname=yname, xname=xname, title=scatitle)
        else scaout = cucov1way(rmw[,iy], rmw[,ix], as.factor(rmw[,g2cow]),
             c(1), plotcons="plot", g1order=g2order,
             depname=yname,covname=xname,g1name=fac2, title=scatitle)
        cat("\n",yname,"vs",xname,":",signif(scaout$B,3))
        nscat = nscat+1
        if (nscat==1) plist = list(scaout$A)
        else plist[[nscat]] = scaout$A
      }
      cat("\n")
      pout = ggarrange(plotlist=plist,nrow=nverscat,ncol=nhorscat)
      cu_plout(pout,"curepmeas-scat", ftype=ftype, fname=fname,
        scale=fscale, width=fwidth, height=fheight, dpi=dpi,remove=remove)
    }
  }
  nleva = nlevels(fctr); if (maxlines==1) maxlines = 100 # for line plots
  if (is.null(depname)) depname = ifelse(itrans==0,dnam,
                          ifelse(itrans==1,paste("sqrt(",dnam,")",sep=""),
                                           paste("log10(",dnam,")",sep="")))
  if (is.null(g1name)) g1name = repnam
  if (is.null(g2name)) g2name = fac2
  # cat ("\nbefore if sumdiff\n")
  if (sumdiff !=0) {
    f1levels = levels(fctr); diffct = 0; diffnams = c(); map1w = c(nleva*0)
    if (length(varlist) != nleva) {
      cat("\nFatal error for",depname,"\nvarlist:"); print(varlist)
      cat("\nfctr:"); print(fctr)
      stop("\nTell Sekhar: For ",depname,
       ", length(varlist) != nleva ",length(varlist)," ",nleva,
       "\nDoes ",depname," appear in more than one column?")
    }
    for (j in 1:nleva) { # number of levels of fctr
      im = varlist[j]; depdotrep = names(rmw[im])
      fctri = substr(depdotrep, dvarnc1+1, nchar(depdotrep))
      i = match(fctri,f1levels)
      if (is.na(i)) stop ("\nTell Sekhar no match for ",fctri," ",im)
      map1w[i] = im # locn of i-th level of fctr in dsgiven
    }
    # print(map1w)
    dummyv = vector(length=nrow(rmw))
    dfw = data.frame(dummyv); if (iffacdep) dfx = data.frame(dummyv)
    for (i in 1:(nleva-1)) {
      if (sumdiff > 0) {ii = i; im = map1w[i]} else {jj = i; jm = map1w[i]}
      for (j in (i+1):nleva) {
        diffct = diffct+1
        if (sumdiff > 0) {jj = j; jm = map1w[j]} else {ii = j; im = map1w[j]}
        diffn = paste(f1levels[jj],chyparr,f1levels[ii],sep="")
        if (iffacdep) {
          for (is in 1:nsubj) {
            depfro = rmw[is,im]; depto = rmw[is,jm]
            dfw[is,diffct] = paste(depfro,"->",depto, sep="")
            deldep = which(levsdep == depto) - which(levsdep == depfro)
            # if (deldep < 0) {deldep = -deldep; updn = "dn"} else updn = "up"
            dfx[is,diffct] = deldep # ifelse(deldep==0,"same",paste(updn,deldep))
            # cat ("\ni,j,is,diffct,wx",i,j,is,diffct,dfw[is,diffct],dfx[is,diffct])
          }
        }
        else for (is in 1:nsubj) {
          depfro = rmw[is,im]; depto = rmw[is,jm]
          dfw[is,diffct] = ifelse(is.na(depfro)||is.na(depto),NA,
            ifelse(itrans==0,depto-depfro,
            ifelse(itrans==1,sqrt(depto)-sqrt(depfro), 100*(depto/depfro-1))))
        }
        diffnams = c(diffnams,diffn)
        colnames(dfw)[diffct] = paste(dvardot,diffn,sep="")
        #cat("\ndiffct,paste(dvardot,diffn):",diffct,paste(dvardot,diffn,sep=""))
      }
    }
    if (g2cow > 0) {
      dfw[,diffct+1] = as.factor(rmw[,g2cow]); colnames(dfw)[diffct+1] = fac2
      if (iffacdep) dfx[,diffct+1] = dfw[,diffct+1]
      locf1 = 2
    }
    else locf1 = 1
    #print(diffnams)
    dfl = reshape(dfw, varying=1:diffct, timevar=repnam, direction="long")
    dfl[,locf1] = factor(dfl[,locf1], levels=diffnams)
    #cat("levels,dfl\n"); print(levels(dfl[,locf1])); print(dfl)
    if (iffacdep) {
      levsdiff = c("xx"); ij = 0
      for (i in 1:nlevdep) for (j in 1:nlevdep) {
        ij=ij+1; levsdiff[ij] = paste(levsdep[i],"->",levsdep[j], sep="")
      }
      dfl[,locf1+1] = factor(dfl[,locf1+1], levels=levsdiff)
      colnames(dfx)=colnames(dfw)
      dfm = reshape(dfx, varying=1:diffct, timevar=repnam, direction="long")
      dfm[,locf1] = factor(dfm[,locf1], levels=diffnams)
      #cat("dfw\n"); print(dfw); cat("dfx\n"); print(dfx)
      #cat("dfl\n");  print(dfl); cat("dfm\n"); print(dfm)
      #cat("\nfactor",as.factor("x"),dfl[1,1],as.factor(dfl[1,1]))
    }
    if (g2cow > 0) {dfl[,1] = cu_reorder(dfl[,1], g2order); g2order = NULL}
  } # end sumdiff != 0
  # cat("\nscale in rep:'",scale,"'")
  # cat("\nrml cols 1-3 typeof:",typeof(rml[1,1]),typeof(rml[1,2]),typeof(rml[1,3]))
  # cat ("\nafter typeof\n")
  # cat ("\nbefore if g2col\n")
  if (g2col==0) {
    if (maxlines > 0) dolines(fctr, rml[,dnum], rml[,inum], "")
    if (iffacdep) {
      cu_rep1fac(rml[,dnum],as.factor(fctr),rml[,inum], scale=scale,
                ebars=ebars, dots=dots,barcolor=barcolor, barfill=barfill,
                order=g1order, psigcld=psigcld, conf.int=conf.int,
                depname=depname, g1name=g1name, title=title,
                suff="dist", legend=legend,
                plot=plot, caption=caption, linetype=linetype, linecolor=linecolor, linesize=linesize, theme=theme,
                size=size, width=width, xangle=xangle, yangle=yangle, 
                orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
      # print(as.factor(dfl[,1]))
      cu_rep1fac(dfl[,2],dfl[,locf1],dfl$id, scale=scale,
                ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                depname=paste(depname,"\u0394 count"), g1name=g1name,
                size=size, width=width, suff="del", legend=legend, yscale=yscale,
                plot=plot, caption=caption, linetype=linetype, linecolor=linecolor, linesize=linesize, theme=theme,
                xangle=xangle, yangle=yangle, 
                orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                order=difforder, psigcld=-3,
                ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
      cu_rep1fac(dfm[,2],dfm[,locf1],dfm$id, scale=scale,
                ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                depname=paste(depname,"updn"), g1name=g1name,
                size=size, width=width, suff="updn", legend=legend, yscale=yscale,
                plot=plot, caption=caption, linetype=linetype, linecolor=linecolor, linesize=linesize, theme=theme,
                xangle=xangle, yangle=yangle, 
                orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                order=difforder, psigcld=-5,
                ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
    }
    else {
      # cat ("\ndfl struc:\n"); str(dfl); cat ("\nrml struc:\n"); str(rml)
      # cat("\nlocf1,dnum,inum",locf1,dnum,inum,"\n")
      repout = cu_rep1way(dfl[,2],dfl[,locf1],dfl$id, 
          rml[,dnum],as.factor(fctr),rml[,inum], itrans, minimal=minimal,
          ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
          order=g1order, psigcld=psigcld, conf.int=conf.int, ddepn=ddepn,
          depname=depname, g1name=g1name, title=title, suff="lev", legend=legend,
          pvpairs=pvpairs, pvypos=pvypos, pvstinc=pvstinc, pvlab=pvlab, pnorm=pnorm,
          pvprefix=pvprefix, pvsize=pvsize, chpvref=chpvref, pvspill=pvspill,
          pnosig=pnosig, psignif=psignif, p2stars=p2stars, p3stars=p3stars, p4stars=p4stars,
          pvnshide=pvnshide, pvtipl=pvtipl, refmean=refmean,
          plot=plot, caption=caption, linetype=linetype, linecolor=linecolor, linesize=linesize, theme=theme,
          size=size, width=width, yscale=yscale,
          xangle=xangle, yangle=yangle, dotshape=dotshape, 
          orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
          fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
          fontxticks=fontxticks, fontyticks=fontyticks,
          axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
          ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
          ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
    }
    if (!is.null(fac2)) {  # needed? See earlier test on fac2 and g2col
      warning("Second factor name <",fac2,"> not recognized - ignored");
      cat("\nSecond factor name <",fac2,"> not recognized - ignored\n",sep="")
    }
  }
  else { # second factor given
    if (maxlines > 0) {
      f2levels = levels(as.factor(rml[,g2col]))
      nlev2 = nlevels(as.factor(rml[,g2col]))
      for (j2 in 1:nlev2) {
        fac2j2 = f2levels[j2]
        nrj2 = length(which(rml[,g2col]==fac2j2)); irj2=0
        df2 = data.frame(A = rep(0,nrj2),stringsAsFactors = F)
        for (ir in 1:nrow(rml)) {
          if (rml[ir,g2col]==fac2j2) {
            irj2=irj2+1; df2[irj2,1] = rml[ir,dnum]
            df2[irj2,2] = fctr[ir]; df2[irj2,3] = rml[ir,inum]
          }
        }
        colnames(df2) = c(depname,g1name,"id")
        dolines(df2[,2], df2[,1], df2$id, paste(g2name,fac2j2))
      }
    }
    if (iffacdep) {
      cu_rep2fac(rml[,dnum],as.factor(fctr),as.factor(rml[,g2col]),rml[,inum],
                scale=scale, ebars=ebars, dots=dots, barfill=barfill,
                order=g1order, psigcld=psigcld, conf.int=conf.int,
                depname=depname, g1name=g1name,
                g2name=g2name, title=title, suff="lev", legend=legend,
                size=size, width=width, yscale=yscale, 
                plot=plot, linetype=linetype, linesize=linesize, theme=theme,
                caption=caption, posd=posd, xangle=xangle, yangle=yangle, 
                orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
      cu_rep2fac(dfl[,3],dfl[,locf1],dfl[,1],dfl$id, scale=scale,
                ebars=ebars, dots=dots, barfill=barfill,
                depname=paste(depname,"\u0394 count"), g1name=g1name,
                size=size, width=width, suff="del", legend=legend, yscale=yscale,
                plot=plot, linetype=linetype, linesize=linesize, theme=theme,
                caption=caption, posd=posd, xangle=xangle, yangle=yangle, 
                orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                order=difforder, psigcld=-3,
                ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
      cu_rep2fac(dfm[,3],dfm[,locf1],dfm[,1],dfm$id, scale=scale,
                ebars=ebars, dots=dots, barfill=barfill,
                depname=paste(depname,"updn"), g1name=g1name,
                size=size, width=width, suff="updn", legend=legend, yscale=yscale, 
                plot=plot, linetype=linetype, linesize=linesize, theme=theme,
                caption=caption, posd=posd, xangle=xangle, yangle=yangle, 
                orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                order=difforder, psigcld=-5,
                ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
    }
    else {
      # cat ("\ndfl struc:\n"); str(dfl); cat ("\nrml struc:\n"); str(rml)
      # cat("\nlocf1,dnum,inum",locf1,dnum,inum,"\n")
      repout =
      cu_rep2way(dfl[,3],dfl[,locf1],as.factor(dfl[,1]),dfl$id,
       rml[,dnum],as.factor(fctr),as.factor(rml[,g2col]),rml[,inum],
       interact=interact, partialF=partialF, itrans, minimal=minimal,
       ebars=ebars, dots=dots, barfill=barfill,
       depname=depname, g1name=g1name, pnorm=pnorm,
       g2name=g2name, title=title, mainx=mainx,
       size=size, width=width, suff="lev", legend=legend, yscale=yscale, 
       pvpairs=pvpairs, pvypos=pvypos, pvstinc=pvstinc, pvlab=pvlab, 
       pvprefix=pvprefix, pvsize=pvsize, chpvref=chpvref, pvspill=pvspill,
       pnosig=pnosig, psignif=psignif, p2stars=p2stars, p3stars=p3stars, p4stars=p4stars,
       pvnshide=pvnshide, pvtipl=pvtipl, refmean=refmean,
       plot=plot, linetype=linetype, linesize=linesize, theme=theme,
       caption=caption, posd=posd, xangle=xangle, yangle=yangle, 
       orientation=orientation, binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
       dotshape=dotshape, order=g1order, psigcld=psigcld, conf.int=conf.int, ddepn=ddepn,
       fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
       fontxticks=fontxticks, fontyticks=fontyticks,
       axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
       ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
       ftype=ftype,fname=fname, fscale=fscale,fwidth=fwidth,fheight=fheight,dpi=dpi,remove=remove)
      if (!interact) 
        cat("\nIn repeated measures analysis, with each subject as their own control,",
            "\nit is pointless to have a second factor without interaction.",
            "\nEffects and p-values are the same as without the second factor.")
    }
  }
  # cat ("\nafter rep1 rep2\n")
  if (!is.null(cov) && ebars!=4) {
    formach = paste(dnam, "~", repnam)
    if (!is.null(fac2)) {
      formach = paste(formach,"+",fac2)
      if (interact) formach = paste(formach,"+",repnam,"*",fac2)
    }
    formach = paste(formach,"+",cov); formla = as.formula(formach)
    forrach = paste("~1|id/",repnam); forra = as.formula(forrach)
    if (dodredge) {
      dsnomiss = na.omit(rml)
      fitlm = lme(formla, random=forra, data=dsnomiss) # no need for na.action
      fitlm$call[[1]] <- quote(nlme::lme.formula) # enable dredge to find lme.formula
      fitlm$call[[2]] <- formla # assign formula object instead of string
      print(summary(fitlm))
      oldw <- getOption("warn")
      options(warn = -1)
      if (m.min>m.max) m.min=1
      dredobj = dredge(fitlm, m.lim=c(m.min, m.max), fixed=fixed, subset=subset)
      options(warn = oldw)
      nvar = length(dredobj)-6 #always 6?
      printmax = min(nrow(dredobj), nmodshow)
      print(dredobj[1:printmax,])
      if (usemod>printmax) usemod=1
      npz = dredobj[usemod,nvar+2]; loglz = dredobj[usemod,nvar+3]
      nonesimp = T
      for (ir in 1:printmax) {
        np1 = dredobj[ir,nvar+2]; logl1 = dredobj[ir,nvar+3]
        if (np1 < npz) {
          if (nonesimp) {
            nonesimp = F
            cat("\n\nLikelihood ratio (LR) tests of chosen ",ifelse(usemod==1,"top",
                                                                    ifelse(usemod==2,"2nd",ifelse(usemod==3,"3rd",paste(usemod,"th best",sep="")))),
                " model #",rownames(dredobj[usemod]), " (assuming nesting of smaller models)",sep="")
          }
          twolr = 2*(loglz-logl1); ndf = npz-np1; pvalchi = 1-pchisq(twolr,ndf)
          cat("\nvs #",rownames(dredobj[ir]),": LR, degf, \u03C7\u00B2 p-value",
              signif(twolr,3),ndf,cu_pval9(pvalchi))
        }
      }
      dredvars = names(dredobj[usemod,2:(1+nvar)])
      for (i in length(dredvars):1) {
        if (is.na(dredobj[usemod,(i+1)])) {
          dredvars = dredvars[-i]
        }
      }
      if (length(dredvars)>0) {
        formach = paste(dnam, "~", paste(dredvars, collapse="+"))
        formla = as.formula(formach)
      }
    }
    fitlm = lme(formla, random=forra, data=rml, na.action = "na.omit")
    fitlm$call[[1]] <- quote(nlme::lme.formula) # enable dredge to find lme.formula
    fitlm$call[[2]] <- formla
    print(summary(fitlm))
    conum = 0
    for (i in 1:length(rml)) {if (names(rml[i])==cov) conum=i}
    if (conum < 0) { # need to figure how to remove random effect
      if (is.null(fac2)) {groupvar = fctr; nameboth12 = g1name}
      else {
        groupvar = factor(paste(rml[,rnum], rml[,g2col], sep="&"))
        nameboth12 = paste(g1name,"&",g2name,sep="")
      }
      ycalc = predict(fitlm)
      datf = data.frame(rml[,dnum], groupvar, rml[,conum], ycalc)
      print(datf)
      p <- ggplot(datf, aes(rml[,conum],color=groupvar)) + geom_point(aes(y=rml[,dnum]),shape=shape) + geom_line(aes(y=ycalc))
      p <- p + labs(x=cov) + labs(colour=nameboth12) + labs(y=depname) +
        ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
      cu_plout(p,"curepmeas-regr", ftype=ftype, fname=fname,
               scale=fscale, width=fwidth, height=fheight, dpi=dpi,remove=remove)
    }
  }
  if (minimal) {
    repout = c(repout,ifelse(nsubdrop>0,nsubdrop,""))
    names(repout)[length(repout)] = "any drop"
    # cat ("\nend of repmeas\n")
    if (ifdeb) {cat("\nrepout\n"); print(repout)}
    return(list(repout,rmwall))
  }
  # else write out rmwall??
}
