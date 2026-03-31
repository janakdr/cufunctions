#' Table 1 with optional box or violin plots
#'
#' @param ds variable or data frame to be summarized (required)
#' @param group1 optional categorical factor to summarize for each level.
#' @param group2 optional second factor to summarize for each level.
#' @param doAll =T (default) for "All" column; F for no; "I" in quotes for All first.
#' @param brief =F (default) to get full summary; T to get just mean&SD or median&IQR.
#' @param pnorm =value (default 0.05) to use median&IQR when brief=T if any norm? < value.
#' @param sdsamerow =T (default) for SD/IQR on same row when brief=T; F for separate row.
#' @param compare =F (default) not to compare levels of groupvar; T to compare.
#' @param minimal = FALSE (default), T or TRUE to suppress all output (for cuomics)
#' @param docorr =F (default) to not correlate all continuous vars; T to do it.
#' @param plot ="box" (default) for boxplots; or "bar" "violin" "rod" "no"
#' @param ebars =NULL (default)/1/2/3/4 (1-4 for SD/SE/CL/IQR; 4 for median)
#' @param dots =1 (default), 0 to not display data on graph
#' @param width numeric value between 0 and 1 specifying box width
#' @param barcolor for outline color, barfill for fill color. Use fill="grey" ="black" etc for single color.
#' @param barfill ="lancet" (default) for colors by group ("lancet" for 2 factors, other journal options "aaas", "jco", "uchicago", "npg"
#' @param depname /g1name/g2name/title=NULL to override names of dependent/Factor1/Factor2/title
#' @param g1order =NULL (default) to reorder bars (1st Factor)
#' @param g2order =NULL (default) to reorder bars (2nd Factor)
#' @param caption =NULL (default)/"yes"/"caption text" to get caption ("yes" to list n's)
#' @param xlab ="label" for different x-axis label than name of second argument.
#' @param ylab ="label" for different y-axis label than name of first argument.
#' @param chariqr ="," (default) for IQR separator
#' @param charamp ="&" (default)/x for character to separate group1&2 levels ("-" etc)
#' @param nlevmax =9 (default) for maximum number of levels for char-variable
#' @param pvpairs="std" (default)/"all"/i/c(ij,ik,...) to show std or all or vs.i or pval's of i/j, i/k ...
#' @param pvypos=NULL (default)/position of pval lines
#' @param pvsi=0.05 (default)/increment for pvypos from one pval line to next
#' @param pvlab="p"/"*" to display numerical or asterisks
#' @param pvprefix="p=" (default)/"" etc prefix to p-values
#' @param pvsize=3.5 (default)/size of p-values
#' @param chpvref="ref" (default) char above i-th group bar when pvpairs=i
#' @param pvspill=F/T to not allow p-values to spill outside range or to allow
#' @param pnosig =0.2 (default)/x for threshold to show p-values even if nonsigificant
#' @param psignif =0.05 (default)/x for threshold to significance
#' @param p2stars =0.01 (default)/x for threshold to two stars
#' @param p3stars =0.001 (default)/x for threshold to three stars
#' @param p4stars =0.0001 (default)/x for threshold to four stars
#' @param pvnshide=T (default)/F to hide NS p-values or not
#' @param pvtipl=0.01 (default)/length of p-value line tips
#' @param linetype ="n" (default)/x for no connecting lines ("solid" "dashed" "dotted" "blank" "longdash" "dotdash" "twodash")
#' @param linecolor ="black" (default)/x for black lines ("red" etc)
#' @param linesize =1 (default)/x for line thickness
#' @param theme ="bw" (default)/x for white background (or "gray" or "classic" (no grid lines))
#' @param legend (default="top"), can be "bottom", "right", "left"
#' @param size numeric value (e.g. size=1), to change size of points and outlines
#' @param yscale (default="none"), can be "log2", "log10", "sqrt"
#' @param xangle /yangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param orientation (default="vertical"), can be "horizontal" or "reverse"
#' @param dotsize =NULL (default 1) set to fraction of binwidth for dot size
#' @param dotshape =NULL (default)
#' @param dotcolor =NULL (default "white") set to dot color
#' @param posd =NULL (default) set to values around 0.9 to fine-tune group2 bar spacing
#' @param binwfac =NULL (default) set to values around 30 to fine-tune horiz.space for dots
#' @param fontfamily ="sans" (default), can be "serif" "mono" 
#' @param ymin, ymax =NA (default) or value to start/end y-axis 
#' @param fontmain =c(14,"bold","black") default, change for title, 0 for not title 
#' @param fontxname .fontyname,fontxticks,fontyticks = c(12,"plain","black") default, 0 to suppress
#' @param axiscolor ,tickcolor="black" (default)/x for axis/tick color
#' @param axisthick ,tickthick=0.5 (default)/x for axis/tick thickness
#' @param ticklength =1 (default)/x for tick length in mm
#' @param xticks.by ,yticks.by =NULL (default)/s for x/y tick spacing by s
#' @param titlejust ="center" (default) or "left" or "right"
#' @param legheadsize =12 (default) for the font size of legend heading
#' @param legtextsize =10 (default) for the font size of legend text
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return returns summary dataframe (if one variable, like table0)
#' @examples
#' cutable1(NEJM, plot="violin")  # single table all variables, violin plots
#' attach(NEJM)
#' cutable1(tgpre, Diet)
#' cutable1(NEJM, Diet, plot="no")  # separate tables all variables, no plots
#' cutable1(NEJM, Diet, brief=TRUE)  # single table for all variables by Diet columns
#' cutable1(NEJM, Diet, brief=TRUE, sdsamerow=FALSE)  # to put SD/IQR on separate line
#' cutable1(NEJM, Diet, brief=TRUE, pnorm=0)  # to get only mean&SD
#' cutable1(NEJM, Diet, brief=TRUE, pnorm=1)  # to get only median&IQR
#' detach(NEJM)
#' @importFrom rlang .data
#' @export
cutable1 = function(ds, group1=NULL, group2=NULL, doAll=T, brief=F, 
              pnorm=.05, sdsamerow=T, compare=F, minimal=F, docorr=F,
              plot="box", ebars=NULL, dots=1, width=NULL, barcolor="black", barfill="colors",
              depname=NULL, g1name=NULL, g2name=NULL, title=NULL,
              g1order=NULL, g2order=NULL, caption=NULL,
              xlab=NULL, ylab=NULL, chariqr=":", charamp ="&", nlevmax=9,
              pvpairs="std", pvypos=NULL, pvsi=0.05, pvlab="p", 
              pvprefix="p=", pvsize=3.5, chpvref="ref", pvspill=F,
              pnosig=0.2, psignif=0.05, p2stars=0.01, p3stars=0.001, p4stars=0.0001,
              pvnshide=T, pvtipl=0.01,
              linetype="n", linecolor="red", linesize=1, theme="bw",
              legend="top", size=NULL, yscale="none", letleft=T,nletbarmax=3,
              font.x=NULL, xangle=NULL, yangle=NULL, orientation="vertical",
              dotsize=NULL, dotshape=NULL, dotcolor=NULL, posd=NULL, binwfac=30, 
              fontfamily="sans", ymin=NA, ymax=NA,
              fontmain=c(14,"bold","black"), fontxname=c(12,"plain","black"), fontyname=c(12,"plain","black"), 
              fontxticks=c(12,"plain","black"), fontyticks=c(12,"plain","black"),
              axiscolor="black",tickcolor="black",axisthick=0.5,tickthick=0.5,
              ticklength=1, xticks.by=NULL, yticks.by=NULL, 
              titlejust="center", legheadsize=12, legtextsize = 10,
              ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
              fheight=NULL, dpi=300, remove=NULL) 
{
  cuf_apply_defaults(match.call(), environment())
  getcolnam = function() {
    if (is.null(groupvar)) return(c("All"))
    else return(c(levels(groupvar),chall,compnamv))
  }
  # crashes if factor variable has too many levels
  pval9ns = function(pval) {
    return(ifelse(pval<0.001,sprintf("%.2e%1s",pval,""),
      ifelse(pval<pnosig,sprintf("%-9.9s",signif(pval,3))," ns   ")))
  }
  tab0comp = function(depv,ylab) {
    dfnor = cu_table0(depv, groupvar, brief=brief, sdsamerow=sdsamerow,
      doAll=doAll, pnorm=pnorm, chariqr=chariqr, nlevmax=nlevmax)
    df = dfnor[[1]]; normTF = dfnor[[2]]; pnormin= dfnor[[3]]
    pvdf = NULL; letbar = NULL; yrange = NULL
    #cat ("\nafter table0 normTF pnormin:",normTF, pnormin)
    if (compare) {
      ncols = endloop; for (i in 1:ncomp) {ncols=ncols+1; df[,ncols]=""}
      #print(df)
      #cat("\ngetting p-values")
      nlevm1 = nlev-1
      if (is.factor(depv)||is.logical(depv)) { #is.character(depv)||
        xtab = table(as.factor(depv),groupvar,useNA="no")
        contab = matrix(data=c(1,1,1,1),nrow=2,ncol=2)
        #cat(nrow(xtab),length(xtab))
        cot = as.numeric(df[1,]); ire = ifelse(nrow(xtab)==2,1,nrow(xtab))
        for (ir in 1:ire) {
          ic = endloop
          for (i in 1:nlevm1) {
            for (j in (i+1):nlev) {
              contab[1,1] = xtab[ir,i]; contab[1,2] = xtab[ir,j]
              contab[2,1] = cot[i]-xtab[ir,i]; contab[2,2] = cot[j]-xtab[ir,j]
              minrowsum = min(contab[1,1]+contab[1,2],contab[2,1]+contab[2,2])
              minexp = minrowsum*min(cot[i],cot[j])/(cot[i]+cot[j])
              #print(contab); cat(chisq.test(contab)$p.value,
              #                   fisher.test(contab,simulate.p.value=TRUE)$p.value)
              pval = ifelse((minexp>10)&&(min(contab)>0), stats::chisq.test(contab)$p.value,
                            stats::fisher.test(contab,simulate.p.value=TRUE)$p.value)
              ic = ic+1; df[ir+1,ic] = pval9ns(pval)
            }
          }
        }
      }
      else {
        #cat("\nbefore cupairwise.t")
        if (normTF) pvalues = cupairwise.t(depv, groupvar,p.adjust.method="none")$p.value
        else {
          #cat(", dunn")
          pvalues = matrix(nrow=nlevm1,ncol=nlevm1)
          suppressMessages(utils::capture.output(pvalvec <- dunn.test(depv, groupvar, altp=T, table=F, kw=F)$altP))
          ic = 0
          for (i in 2:nlev) for (j in 1:(i-1)) {
            ic = ic+1; pvalues[i-1,j] = pvalvec[ic]
          }
          #cat("\npvalvec:"); print(pvalvec); cat("\npvalues:"); print(pvalues)
        }
        ic = endloop
        for (j in 1:nlevm1) for (i in j:nlevm1) {
          ic = ic+1; df[2,ic] = pval9ns(pvalues[i,j])
        }
        if (plot != "no") {
          #cat("\nplot not no"); print(pvalues)
          plot = "bar"
          dffnor = cu_table0(depv, groupvar, brief=F, nlevmax=nlevmax, pnorm=pnorm)
          dff = dffnor[[1]];
          #cat ("before pvline")
          pvstruc = cu_pvline(dff,"all",pvalues,nlev,1,pvlab,pnosig,psignif,p2stars,p3stars,p4stars,
                              NULL,NULL,T,F,"ref",3,1,"none")
          #cat(" after pvline")
          #print(pvstruc)
          pvdf = pvstruc[[1]]; pvypos = pvstruc[[2]]; pvlab = pvstruc[[3]]
          yrange = pvstruc[[4]]; letbar = pvstruc[[5]]
          #cat("\npvypos,yrange",pvypos,yrange)
        }
      }
      #print(df)
    }
    #("\npre-is.factor")
    if (!is.factor(depv) && !is.character(depv) && plot!="no") 
      if (is.null(groupvar)) graphics::boxplot(depv, ylab=ylab)
    else {
      title=paste(depname, "at levels of", namgrvar)
      #cat("\ntitle:",title)
      ebars = ifelse(normTF,1,4)
      #cat("\npre-plot1:",title)
      p <- cu_plot1("cutable1",depv, groupvar, NULL, namgrvar, depname, title,
                    plot=plot, linetype=linetype, linecolor=linecolor, linesize=linesize,
                    letbar=letbar, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                    pvdf=pvdf,pvypos=pvypos,pvlab=pvlab,pvprefix="p=",pvsize=pvsize,
                    pvnshide=pvnshide,pvtipl=pvtipl,
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
      #cat("\npost-plot1")
    }
    #cat("\npre-return")
    return (list(df, normTF, pnormin))
  }
  notgroup = function(namegp) {stop(namegp,
    " cannot be a group variable as numeric with ",nlev," levels.",
    "\n    If it is the variable being analyzed, it should come first.",
    "\n    Or, did you mean to run cucov1way, cucov2way, culinreg?")
  }
  printdfAll = function(df,right=T) {
    if (Allfirst) print(df %>% relocate(.data$All), right=right)
    else print(df, right=right)
  }
  
  Allfirst = F; if (minimal) sdsamerow=F
  if (!is.logical(doAll)) {
    if (doAll=="I") Allfirst = T
    else cat ("\ndoAll='",doAll,"' no good. Ignored\n",sep="")
    doAll = T
  }
  if (is.null(plot)) {plot = "no"}
  else if (plot %in% c("n","no","N","NO","No")) {plot="no"}
  else if (!(plot %in% c("bar","box","violin"))) {
    cat("\nplot='",plot,"' no good. Taken to be 'box'",sep=""); plot="box"
  }
  nrowmax = 16; yesfoot = T; chall = NULL; if (doAll) chall = "All"
  if (is.null(ylab)) ylab=deparse(substitute(ds))
  if (is.null(group1)) {
    groupvar = NULL; nlev = 0; namgrvar = ""
    if (compare) {cat("\nno groupvar; compare ignored.\n"); compare=F}
  }
  else {
    depsubg1 = deparse(substitute(group1)); if (is.null(g1name)) g1name=depsubg1
    if (is.numeric(group1)) {
      nlev = nlevels(as.factor(group1))
      if (nlev > 9) notgroup(depsubg1)
    }
    group1 = as.factor(group1)
    group1 = cu_reorder(group1, g1order); g1order = NULL; g1names = levels(group1)
    nlev1 = nlevels(group1); nlev11 = nlev1-1
    if (is.null(group2)) {
      groupvar = group1; g12names = g1names; namgrvar = g1name; nlev = nlev1
    }
    else {
      if (compare&&!brief) {cat("\ncompare=T, so brief set to T.\n"); brief=T}
      depsubg2 = deparse(substitute(group2)); if (is.null(g2name)) g2name=depsubg2
      if (is.numeric(group2)) {
        nlev = nlevels(as.factor(group2))
        if (nlev > 9) notgroup(depsubg2)
      }
      group2 = cu_reorder(group2, g2order); g2order = NULL; g2names = levels(group2);
      nlev2 = nlevels(group2); nlev21 = nlev2-1;
      groupvar = factor(paste(group1, group2, sep=charamp))
      g12names = levels(groupvar); nlev = nlevels(groupvar)
      namgrvar = paste(g1name,charamp,g2name,sep="")
    } 
    if (is.null(xlab)) xlab=namgrvar
  }
  endloop = nlev + ifelse (doAll, 1, 0) # copied from cu_table0
  if (compare) {
    ncomp = (nlev*(nlev-1))/2; blvec = rep("",ncomp); compnamv = blvec
    i = 1; j = 1
    for (ic in 1:ncomp) {
      if (j<nlev) {j = j+1} else {i=i+1; j=i+1}
      compnamv[ic] = ifelse(minimal,paste(g12names[i],"vs",g12names[j],sep=""),
                            paste(i,"vs",j))
      #cat ("\ni,j,ic:",i,j,ic,compnamv[ic])
    }
    #print(compnamv)
  }
  else {ncomp = 0; compnamv = NULL}
  any2x2 = F; anyt = F
  if (!is.data.frame(ds)) {
    # cat("\nok, not dataframe")
    if (is.null(depname)) depname=deparse(substitute(ds))
    # cat("\ndepname:",depname)
    dfn = tab0comp(ds, ylab)
    df = dfn[[1]]; normTF = dfn[[2]]; pnormin = dfn[[3]]
    # cat("\nafter tab0comp, normTF,pnormin,df:",normTF,pnormin,"\n")
    # print(df)
    colnames(df)=getcolnam()
    if (minimal) return (list(df, normTF, pnormin))
    cat(depname,ifelse(is.null(groupvar),"\n",paste("across",namgrvar,"levels\n")))
    printdfAll(df)
    cat("\n"); if (is.character(ds) || is.logical(ds)) yesfoot = F
    if (is.factor(ds)||is.character(ds)||is.logical(ds)) any2x2 = T
    else anyt = T
  }
  else { #is dataframe
    cat("Entire dataset",ylab,
        ifelse(is.null(groupvar),"\n",paste("across",namgrvar,"levels\n")))
    rowsnewdf = 0
    for (ivar in 1:ncol(ds)) { 
      namivar = names(ds[ivar])
      if (namivar != namgrvar && !is.character(ds[[ivar]])) {
        # cat("\nivar,names",ivar,names(ds[ivar]),namgrvar)
        depname = namivar; dfn = tab0comp(ds[[ivar]], namivar)
        df = dfn[[1]]; normTF = dfn[[2]]; pnormin = dfn[[3]]
        # cat("\nafter tab0comp, normTF,pnormin,df:",normTF,pnormin,"\n")
        # print(df)
        # normTF, pnormin not used for dataframe?  why?
        # cat("\nivar",ivar); print(df)
        if (brief) { #1/2 rows for each column in ds
          unldf = unlist(df[1,])
          nvec = as.numeric(unldf)
          #if (ivar==1) cat("\nunldf,nvec",unldf,nvec)
          # cat("\npost-unlist")
          if (rowsnewdf==0) {
            newdf = data.frame(matrix(ncol=endloop+ncomp, nrow=1),stringsAsFactors = F)
            #if (ivar==1) {cat("\nnewdf",nrow(newdf),ncol(newdf)); print(newdf)}
            newdf[1,] = unldf; colnames(newdf) = getcolnam()
            #if (ivar==1) {cat("\nnewdf",nrow(newdf),ncol(newdf)); print(newdf)}
            ifnch = TRUE; nvecpre = nvec
            # print(newdf)
          }
          else {
            ifnch = FALSE
            # print(nvec); print(nvecpre)
            for (j in 1:endloop) {
              if (nvec[j] != nvecpre[j]) ifnch = TRUE
            }
            # cat("\nifnch",ifnch)
            if (ifnch) newdf = rbind(newdf, unlist(df[1,]));
            # print(newdf)
          }
          if (ifnch) {
            nstr = paste("N from ",namivar," on:",sep=""); nvecpre = nvec
            if (rowsnewdf==0) rnams = nstr
            else rnams = c(rnams,nstr)
          }
          #cat("\npost-ifnch rnams,newdf,df"); print(rnams); print(newdf); print(df)
          if (is.factor(ds[[ivar]])||is.character(ds[[ivar]])||is.logical(ds[[ivar]])) {
            any2x2 = T # is.character(depv)|| not possible
            #print(rownames(df))
            for (ilev in 2:nrow(df)) {
              rnams = c(rnams,paste(namivar,"-",rownames(df)[ilev],sep=""))
            }
          }
          else {
            anyt = T
            # cat("\nrnams,name", names(ds[ivar])); print(rnams)
            rnams = c(rnams, paste(namivar,"-",rownames(df)[2],sep=""))
            if (!sdsamerow) rnams = c(rnams,
                 paste(namivar,"-",rownames(df)[3],sep=""))
          }
          #print(rnams)
          #newdf = rbind(newdf, df[2:nrow(df),]) # fails if no groupvar. why? how about unlist?
          for (i in 2:nrow(df)) {
            #print(i); print(newdf)
            newdf = rbind(newdf,unlist(df[i,]))
            #print(newdf)
          }
          #cat("\npost-rbind"); print(newdf)
          rowsnewdf = nrow(newdf)
          # cat("\nrowsnewdf",rowsnewdf); print(newdf)
        }
        else if (is.null(groupvar)) { # !brief and no groupvar
          if (rowsnewdf==0) {
            newdf = data.frame(A=rep(0,nrowmax),stringsAsFactors = F) # max possible rows
            rownames(newdf)=c("N(tot)","NA's","Mean","SD","CV%","SEM","Min.","1st Q","Median","3rd Q","Max.","Skew","Kurt.","Skew p","Kurt p","norm ?")
          }
          for (i in 1:nrow(df)) { # is.character not possible here???
            if (is.factor(ds[[ivar]])||is.character(ds[[ivar]])) {
              newdf[i,ivar] = paste(rownames(df)[i],":",df[i,1],sep="")
            }
            else {newdf[i,ivar] = df[i,1]; anyt = T}
          }
          if (nrow(df) < nrowmax) {for (i in (nrow(df)+1):nrowmax) newdf[i,ivar] = " "}
          rowsnewdf = max(rowsnewdf,nrow(df))
        }
        else { # !brief with groupvar
          colnames(df)=getcolnam(); anyt = T # assume some continuous var there
          cat(colnames(ds)[[ivar]],"\n"); printdfAll(df); cat("\n")
        }
      } # if not groupvar
    } # end for ivar
    # cat(nrow(newdf),ncol(newdf),"\n"); # print(newdf)
    if (brief) 
      {colnames(newdf)=getcolnam(); rownames(newdf)=rnams; printdfAll(newdf)}
    else if (is.null(groupvar)) {
      colnames(newdf) = names(ds);
      if (rowsnewdf < nrowmax) newdf = newdf[-c((rowsnewdf+1):nrowmax),]
      printdfAll(newdf)
    }
  } # if dataframe
  #if (yesfoot) {
  chnotall = ifelse(doAll&&!is.null(groupvar)," (or for All)","")
  if (brief) {
    if (anyt) cat("Shapiro-Wilk Normality testing not done if SD=0 or n<6 or n>4000",
                  chnotall,
                  ".\nMean\u00B1SD if normal, Median(25th %ile, 75th %ile) if not",sep="")
    if (compare) {
      if (anyt) cat("\nComparisons by Fisher LSD if normal, by Dunn if not")
      if (any2x2) cat("\nComparisons by Fisher exact if any expected<11 or any 0")
    }
  }
  else if (anyt) cat("Skewness, Kurtosis & Normality testing not done if SD=0 or n<6 or n>4000",
                     chnotall,
                     ".\nNormality by Shapiro-Wilk, p-value shown if <0.1",sep="")
  if (anyt || any2x2) cat("\n\n")
  #}
  if (is.data.frame(ds)) {
    if (docorr) {
      ncateg = 0 
      for (ivar in 1:ncol(ds)) { # is.character not possible here???
        if (is.factor(ds[[ivar]])||is.character(ds[[ivar]])||is.logical(ds[[ivar]])) {
          if (ncateg==0) {delist <- c(ivar)} else delist <- c(delist,ivar)  
          # no good: ifelse (ncateg==0,c(ivar),c(delist,ivar))
          ncateg= ncateg+1
        }
      }
      if (ncateg==0) {dsnum = ds} else dsnum = ds[,-delist]
      cormat = rcorr(as.matrix(dsnum))
      cat("\nCorrelation matrix, followed by p-value matrix\n")
      print(cormat)
    }
  }
 }
