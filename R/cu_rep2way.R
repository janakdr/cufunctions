#' Does repeated measures anova and post-hoc tests when there is a second, non-repeated factor (long-format - multiple lines for each subject)
#' @param depvar (4 required)
#' @param group1 see depvar
#' @param group2 see depvar
#' @param Subject see depvar
#' @param group1 repeated grouping factor
#' @param interact =TRUE (default), F or FALSE for no interaction
#' @param partialF =FALSE (default), T or TRUE for partial F vs simpler models
#' @param mainx =1 (default), 2 to interchange the two factors in bar graph
#' @param itrans type of transformation if any
#' @param minimal = FALSE (default), T or TRUE to suppress all output (for cuomics)
#' @param ebars =0 (default to be 1 or 4)/1/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR) or -N (nonparm if any norm fail)
#' @param dots =0 (default), 1 to display data on graph
#' @param barcolor for outline color
#' @param barfill for fill color. Use barfill="colors" for colors by group.
#' @param plot ="bar" (default) for bar graphs; "box" "violin" "rod" "no"
#' @param order =NULL (default)/c("...") to reorder bars
#' @param psigcld =0 (no letters)/x for CLD letters on bars (any shared letter means P>psigcld, 0 for no letters)
#' @param conf.int =0.95 (default)/x/0 for confidence interval width of contrast estimates (0 for none)
#' @param ddepn =NULL (default) for name of dependent variable difference
#' @param depname to override names of depvar, groupvar, title
#' @param g1name see depname
#' @param g2name see depname
#' @param title see depname
#' @param legend ="top" (default), "bottom", "right", "left" to locate legend
#' @param pnorm =0.05 (default) threshold for normality test
#' @param pvpairs (default "std") (default)/"all"/i/c(ij,ik,...) to show std or all or vs.i or pval's of i/j, i/k ...
#' @param pvypos (default NULL) (default)/position of pval lines
#' @param pvstinc (default 0.05) (default)/increment for pvypos from one pval line to next
#' @param pvlab (default "p"/"*") to display numerical or asterisks
#' @param pvprefix (default "p=") (default)/"" etc prefix to p-values
#' @param pvsize (default NULL) (default 3.5, 7)/size of p-values or **
#' @param chpvref (default "ref") (default) char above i-th group bar when pvpairs=i
#' @param pvspill (default F/T) to not allow p-values to spill outside range or to allow
#' @param pnosig =0.2 (default)/x for threshold to show p-values even if nonsigificant
#' @param psignif =0.05 (default)/x for threshold to significance
#' @param p2stars =0.01 (default)/x for threshold to two stars
#' @param p3stars =0.001 (default)/x for threshold to three stars
#' @param p4stars =0.0001 (default)/x for threshold to four stars
#' @param pvnshide (default T) (default)/F to hide NS p-values or not
#' @param pvtipl (default 0.01) (default)/length of p-value line tips
#' @param refmean =NA (default)/ref.val to compare each group mean with
#' @param legend ="top" (default), "bottom", "right", "left" to locate legend
#' @param suff =NULL (default) set to desired suffix
#' @param caption =NULL (default)/"yes"/"caption text" to get caption ("yes" to list n's)
#' @param linetype ="n" (default)/x for no connecting lines ("dashed" "dotted")
#' @param linecolor ="black" (default)/x for black lines ("red" etc) (only for single factor)
#' @param linesize =1 (default)/x for line thickness
#' @param theme ="bw" (default)/x for white background (or "gray" or "classic" (no grid lines))
#' @param size numeric value (e.g. size=1), to change size of points and outlines
#' @param width numeric value between 0 and 1 specifying box width
#' @param yscale (default="none"), can be "log2", "log10", "sqrt"
#' @param xangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param yangle see xangle
#' @param orientation (default="vertical"), can change to "horizontal"
#' @param posd =NULL (default) set to values around 0.9 to fine-tune group2 bar spacing
#' @param binwfac =NULL (default 30) set to fraction of range within which points will be binned
#' @param dotsize =NULL (default 1) set to fraction of binwidth for dot size
#' @param dotshape =NULL (default)
#' @param dotcolor =NULL (default "white") set to dot color
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
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale =NULL(default) or set to numerical value
#' @param fwidth see fscale
#' @param fheight see fscale
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @param ddep char-string of name of dependent variable
#' @param dgp1 char-string of name of first grouping factor
#' @param dgp2 char-string of name of second grouping factor
#' @param dsub char-string of name of subject identifier
#' @param plot ="bar" (default) for bar graphs; "box" "violin" "rod" "no"
#' @param axiscolor ="black" (default)/x for axis color
#' @return one line of summary means/SDs and p-values
#' @examples
#' # Internal function, not exported; called via curepmeas()
#' \dontrun{
#' cu_rep2way(TG, Diet, sex, ID)  # with interaction between the two fixed factors
#' # with no interaction (pointless, same result as without 2nd factor)
#' cu_rep2way(TG, Diet, sex, ID, interact=FALSE)
#' }
cu_rep2way = function(ddep,dgp1,dgp2,dsub,depvar, group1, group2, Subject,
                     interact=TRUE, partialF=FALSE, mainx=1, itrans, minimal=F,
                     ebars=0, dots=0, barcolor="black", barfill="colors", plot="bar",
                     order=NULL, psigcld=0, conf.int=0.95, ddepn=NULL,
                     depname=NULL, g1name=NULL, g2name=NULL, pnorm=0.05,
                     pvpairs="std", pvypos=NULL, pvstinc=0.05, pvlab="p", 
                     pvprefix="p=", pvsize=NULL, chpvref="ref", pvspill=F,
                     pnosig=0.2, psignif=0.05, p2stars=0.01, p3stars=0.001, p4stars=0.0001,
                     pvnshide=T, pvtipl=0.01, refmean=NA,
                     title=NULL, legend="top", suff=NULL, caption=NULL,
                     linetype="n", linecolor="black", linesize=1, theme="bw",
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
  partfout = function(fitlm,charstr) {
    ndf1 = fitlm$fixDF$X[1]; rse1 = stats::sigma(fitlm)
    #print(fitlm$fixDF$terms); cat("\nndf,rse",ndf1,rse1)
    pval = cupartialF(rse1,ndf1,rsez,ndfz)
    cat("\np=",signif(pval,3)," vs model with ",charstr,sep="")
    return(pval)
  }
  
  if(is.null(depname)) depname = deparse(substitute(depvar))
  if(is.null(g1name)) g1name = deparse(substitute(group1))
  if(is.null(g2name)) g2name = deparse(substitute(group2))
  if (psigcld < -3) {chslamp = "/"; depvar = as.factor(depvar)}
  else {chslamp = "&"; if (is.character(depvar)) depvar = as.factor(depvar)}
  nameboth12 = paste(g1name,chslamp,g2name,sep="")
  # cat("\ng2name,nameboth12:",g2name,nameboth12)
  groupvar = interaction(group1, group2, lex.order=T, sep=chslamp) 
                  #factor(paste(group1, group2, sep="&"))
  ivar = deparse(substitute(Subject))
  gp1 = nlevels(group1); gp2 = nlevels(group2)
  g1names = levels(group1); g2names = levels(group2)
  nlevboth12=nlevels(groupvar); bothnames = levels(groupvar);
  # print(g1names); print(g2names); print(bothnames)
  depnact = ifelse(itrans==2,substr(depname,7,nchar(depname)-1), depname)
  if (is.null(ddepn))
  ddepn = paste(depnact," difference",ifelse(itrans==2," %",""),sep="")
  # print(depvar); print(group1); print(group2); print(groupvar) 
  dgpvar = interaction(dgp1, dgp2, lex.order=T, sep=chslamp) 
  dds = data.frame(ddep,dgp1,dgp2,dgpvar)
  ddfn = cu_table0(ddep, dgpvar, brief=F, doAll=F)
  ddf = ddfn[[1]]; normTF = ddfn[[2]]; pnormin = ddfn[[3]]
  colnames(ddf)=levels(dgpvar)
  ds = data.frame(depvar, group1, group2, groupvar)
  df = cu_table0(depvar, groupvar, brief=F, doAll=F)[[1]]
  colnames(df)=bothnames
  if (!minimal) {
    cat("\n",ddepn,"\n")
    print(ddf)
    cat(depname,"\n")
    print(df)
    cat("If normality testing is done, p-value is displayed only if it is <0.1\n")
    if (ebars !=4) {
      pnormin = 1; nfail = 0; nrowmax = 16; ndifft = gp2 * gp1 * (gp1-1)/2
      for (j in 1:ndifft) { # logic from table0
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
        cat("\nDATA FAIL NORMALITY TEST IN",nfail,"OF",ndifft,"GROUP Differences.",
            "SMALLEST P-VALUE",ifelse(pnormin==0,"<0.001",pnormin))
        if (ebars<=0) {ebars=4; cat("\nNONPARAMETRIC ANALYSIS WILL BE DONE.\n")}
        else cat("\nLOOK FOR DATA ERRORS IN 'Min' AND 'Max' VALUES.",
                 "\nIF DATA ARE NOT NORMAL,",
                 "\nYOU SHOULD USE THE NONPARAMETRIC DUNN TEST (ebars=4)\n")
      }
      else if (ebars<=0) ebars = min(3,max(1,-ebars))
    }
  }
  lenrep = 2 + gp1*gp2*(4*gp1+4 +gp1*gp2+gp1+gp2-3)/4
  if (!is.na(refmean)) {
    lenrep = lenrep+nlevboth12; pval1ts = rep(-1,nlevboth12)
    sumsqr = 0; ndft = 0
    for (ig1 in 1:nlevboth12) {
      if ((ndfi <- as.numeric(df[1,ig1])-1) > 0)
      {ndft = ndft+ndfi; sumsqr = sumsqr+ndfi*as.numeric(df[4,ig1])**2}
      # cat ("\n",ig1,ndfi,ndft,sumsqr)
    }
    poolsd = sqrt(sumsqr/ndft) # ; cat("\npoolsd",poolsd)
    for (ig1 in 1:nlevboth12) {
      if ((ni1 <- as.numeric(df[1,ig1])) > 0) {
        tstat = abs(as.numeric(df[3,ig1])-refmean)*sqrt(ni1)/poolsd
        pval1ts[ig1] = 2 * stats::pt(-tstat,ndft)
        # cat ("\n",ig1,ni1,tstat)
      }
      else pval1ts[ig1] = NA
    }
    # print(pval1ts)
  }
  repout = rep(-1,lenrep); repn = rep("??",lenrep) #; print(repout)
  repout[1] = depname; repn[1] = "Variable"
  jrepo = 2; ndiff = gp1*(gp1-1)/2
  for (jg2 in 1:gp2) {
    jdf = jg2
    for (ig1 in 1:gp1) {
      repout[jrepo] = df[3,jdf]; repout[jrepo+1] = df[4,jdf] #mean sd
      repnam = paste(g2names[jg2],":",g1names[ig1],sep="")
      repn[jrepo] = paste(repnam,"_mean",sep="")
      repn[jrepo+1] = paste(repnam,"_sd",sep="")
      jrepo = jrepo+2; jdf = jdf+gp2
    }
    jdf = jg2; iga = 1; igb = 2
    for (jdi in 1:ndiff) {
      repout[jrepo] = ddf[3,jdf]; repout[jrepo+1] = ddf[4,jdf] #mean sd
      repnam = paste(g2names[jg2],":",g1names[igb],"-",g1names[iga],sep="")
      repn[jrepo] = paste(repnam,"_mean",sep="")
      repn[jrepo+1] = paste(repnam,"_sd",sep="")
      igb = igb+1; if (igb > gp1) {iga = iga+1; igb = iga+1}
      jrepo = jrepo+2; jdf = jdf+gp2
    }
  }
  if (!is.na(refmean)) for (ig1 in 1:nlevboth12) {
    repout[jrepo] = ifelse (pval1ts[ig1]<=pnosig, round(pval1ts[ig1],3), " ")
    repn[jrepo] = paste(bothnames[ig1]," v.",round(refmean,0),sep="")
    # cat("\n",repn[jrepo],repout[jrepo])
    jrepo = jrepo+1
  }
  # cat("\nrepout:\n"); print(repout)
  if (psigcld < -1) { # nice to copy/move group2 comparisons between group1 here
    letbar = NULL; pvdf = NULL  
    # but what is CLD convention to indicate no comparisons between groups?
    if (is.null(title)) title=paste(depname, "across repeats of", g1name)
    if (ebars==4) {
      if (!minimal) 
      cat ("\nComparing each",g1name,"difference among",g2name,
           "groups\n     by Wilcoxon rank sum test with continuity correction\n")
      for (i1 in 1:gp1) {
        if (!minimal) cat("\n",g1names[i1],"\n")
        nlevm1 = gp2-1; pvalues = rep(-1,nlevm1*nlevm1); ipo = -1; i1b=gp2*(i1-1)
        for (i2 in 1:nlevm1) {
          for (j2 in (i2+1):gp2) {
            pvalues[ipo+j2] =
              stats::wilcox.test(depvar[groupvar==bothnames[i1b+i2]], 
                depvar[groupvar==bothnames[i1b+j2]], paired=F, exact=F)$p.value
          }
          ipo = ipo+nlevm1
        }
        if (!minimal) cu_pvalmatout(pvalues,g2names)
      }
    }
  }
  # else { # continuous outcome, not first call with differences
  gp11 = gp1-1; gp21 = gp2-1; ninter = gp11*gp21
  zeros = gp1+gp2-1; endg01 = 0; endg11 = endg01+gp11; endg21 = zeros-1
  pvminint = 1
  nlev12m1 = nlevboth12-1; pvalues = rep(-1,nlev12m1*nlev12m1); ipo = -1;
  if (ebars != 4) {
    coeffs = c("(Intercept)") # below loads g1names and g2names into coeffs
    for (i1 in 2:gp1) {coeffs = c(coeffs,paste(g1name,g1names[i1],sep=""))}
    for (i2 in 2:gp2) {coeffs = c(coeffs,paste(g2name,g2names[i2],sep=""))}
    if (interact) {
      zeros = zeros + ninter
      fit = lme(depvar ~ group1+group2+group1*group2, random=~1|Subject/group1,
                na.action=stats::na.omit)
      for (j in 2:gp2) {
        coeffnam2 = paste(":",g2name,g2names[j],sep="")
        for (i in 2:gp1) {
          coeffnam = paste(g1name,g1names[i],coeffnam2,sep="")
          coeffs = c(coeffs,coeffnam)
        }
      }
      names(fit$coefficients$fixed) = coeffs
      # #names(fit$coefficients$random) = c(g1name,ivar)
      # fit$call$fixed[2]=depname
      # fit$call$fixed[3]=g1name
      if (!minimal) {
        cat("\nRepeated measures anova: lme(",depname," ~ ",g1name,"+",g2name,
            "+",g1name,"*",g2name,", random=~1|Subject/",g1name,
            ", na.action=na.omit)\n",sep="")
        print(summary(fit))
        cat("\n",depname,": ",g1name," comparisons WITHIN EACH ",
            g2name," group\n\n",sep="")
      }
      for (i2 in 1:gp2) {  # factor 1 effects at each level of factor 2
        if (!minimal) cat(g2names[i2],"\n")
        if (i2>1) fac2zi = endg21 + (i2-2)*gp11
        for (i1 in 1:gp11) {  # comparing factor 1 levels
          for (j1 in (i1+1):gp1) {
            vec = c(rep(0,zeros))  # j1+i2 minus i1+i2
            vec[j1+endg01] = 1; if (i1 > 1) vec[i1+endg01] = -1
            if (i2 > 1) {vec[fac2zi+j1] = 1; if (i1 > 1) vec[fac2zi+i1] = -1}
            if (!minimal) cat(g1names[j1],"minus",g1names[i1])
            jpval = nlev12m1*((i1-1)*gp2+i2-1)+(j1-1)*gp2+i2-1
            estpv = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
            pvalues[jpval] = estpv
            repout[jrepo] = ifelse (estpv <= pnosig, signif(estpv,3), " ")
            repn[jrepo] = paste("p(",g2names[i2],":",
                g1names[i1],"vs",g1names[j1],")",sep="")
            jrepo = jrepo+1
            #          cat("\n",i1,i2,j1,jpval)
          }
        }
        if (!minimal) cat("\n")
      }
      if (!minimal) cat(depname,": ",g2name," comparisons WITHIN AND BETWEEN ",
                "repeats of ",g1name,"\n\n",sep="")
      npairsgp1 = (gp1*(gp1-1))/2;  ndifbar = gp2*npairsgp1
      nd2 = (ndifbar-1)**2; pvalsdiff = rep(NA,nd2)
      #print(pvalsdiff)
      for (i2 in 1:gp21) {  # comparing factor 2 levels
        if (i2>1) fac2zi = endg21 + (i2-2)*gp11
        for (j2 in (i2+1):gp2) {
          if (!minimal) cat(g2names[j2],"minus",g2names[i2],"\n")
          repnam = paste(":",g2names[j2],"vs",g2names[i2],")",sep="")
          fac2zj = endg21 + (j2-2)*gp11
          for (i1 in 1:gp1) {        # at each level of factor 1
            vec = c(rep(0,zeros))    # i1+j2 minus i1+i2
            vec[endg11+j2] = 1; if (i2 > 1) vec[endg11+i2] = -1
            if (i1>1) {vec[fac2zj+i1] = 1; if (i2 > 1) vec[fac2zi+i1] = -1}
            if (!minimal) cat(g1names[i1]) 
            jpval = nlev12m1*((i1-1)*gp2+i2-1)+(i1-1)*gp2+j2-1
            estpv = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
            pvalues[jpval] = estpv
            repout[jrepo] = ifelse (estpv <= pnosig, signif(estpv,3), " ")
            repn[jrepo] = paste("p(",g1names[i1],repnam,sep="")
            jrepo = jrepo+1
            #cat("\n",i1,i2,j2,jpval)
          }
          jbdif = (i2-1)*(ndifbar-1)+i2
          for (i1 in 1:gp11) {  # between factor 1 levels
            for (j1 in (i1+1):gp1) {
              vec = c(rep(0,zeros)) #(j1+j2 minus j1+i2)vs(i1+j2 minus i1+i2)
              vec[fac2zj+j1] = 1; if (i2 > 1) vec[fac2zi+j1] = -1
              if (i1>1) {vec[fac2zj+i1] = -1; if (i2 > 1) vec[fac2zi+i1] = 1}
              if (!minimal) cat(g1names[j1],"minus",g1names[i1])
              pvaldiff = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
              pvminint = min(pvminint,pvaldiff)
              #cat("\njbdif,i2,j2,pvaldiff:",jbdif,i2,j2,pvaldiff)
              pvalsdiff[jbdif+j2-i2-1] = pvaldiff
              repout[jrepo] = ifelse (pvaldiff<=pnosig,signif(pvaldiff,3), " ")
              repn[jrepo] = paste("p(",g1names[j1],"-",g1names[i1],repnam,sep="")
              jrepo = jrepo+1
              jbdif = jbdif+gp2*ndifbar
            }
          }
          if (!minimal) cat("\n")
        }
      }
      repout[jrepo] = ifelse(pnormin<=pnorm,pnormin," "); repn[jrepo] = "pnormin"
      # cat("\nrepout:\n"); print(repout)
      if (!minimal) {
        cat("Note:",g2name,"differences between repeats of",g1name,
            "are same as\n     ",g1name,"differences between",g2name,
            "groups\ne.g.,",g2names[gp2],"minus",g2names[gp21],"between",
            g1names[gp1],"and",g1names[gp11],"is identical to\n     ",
            g1names[gp1],"minus",g1names[gp11],"between",
            g2names[gp2],"and",g2names[gp21])
        cat("\n\nRest of the pairwise comparisons (unlikely to be of interest)\n")
        for (ipv in 1:nlev12m1){
          i1 = (ipv-1) %/% gp2 + 1;  i2 = (ipv-1) %% gp2 + 1
          if (i2>1) fac2zi = endg21 + (i2-2)*gp11
          for (jpv in (ipv+1):nlevboth12){
            jpval = nlev12m1*(ipv-1) + jpv-1
            if (pvalues[jpval]<0) {
              j1 = (jpv-1) %/% gp2 + 1;  j2 = (jpv-1) %% gp2 + 1
              vec = c(rep(0,zeros))  # j1+j2 minus i1+i2
              fac2zj = endg21 + (j2-2)*gp11
              if (j1 > 1) vec[endg01+j1] = 1
              if (i1 > 1) vec[endg01+i1] = -1
              if (j2 > 1) {vec[endg11+j2] = 1;if (j1 > 1) vec[fac2zj+j1] = 1}
              if (i2 > 1) {vec[endg11+i2] = -1;if (i1 > 1) vec[fac2zi+i1] = -1}
              cat(bothnames[jpv],"minus",bothnames[ipv])
              pvalues[jpval] = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
            }
          }
        }
        cat ("\nSummary of p-values by repeated measures anova and Fisher's LSD (post-hoc t)")
      }
    }  # end of interact=T
    else {
      fit = lme(depvar ~ group1+group2, random=~1|Subject/group1, na.action=stats::na.omit)
      names(fit$coefficients$fixed) = coeffs
      if (!minimal) {
        cat("Repeated measures anova: lme(",depname," ~ ",g1name,"+",g2name,
            ", random=~1|Subject/",g1name,", na.action=na.omit)\n",sep="")
        print(summary(fit))
        cat("\n",g1name," COMPARISONS (SAME AS WITHOUT ",g2name," in model)\n\n",sep="")
      }
      pvalue1 = rep(2,gp11*gp11); ipo = -1;
      for (i in 1:gp11) {
        for (j in (i+1):gp1) {
          vec=c(rep(0,zeros)); vec[j+endg01] = 1; if (i>1) vec[i+endg01] = -1
          if (!minimal) cat(g1names[j], " minus ", g1names[i],sep="")
          pvalue1[ipo+j] = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
        }
        ipo = ipo+gp11
      }
      if (!minimal) cat("\n",g2name," comparisons\n\n",sep="")
      pvalue2 = rep(2,gp21*gp21); ipo = -1;
      for (i in 1:gp21) {
        for (j in (i+1):(gp2)) {
          vec=c(rep(0,zeros)); vec[j+endg11] = 1; if (i>1) vec[i+endg11] = -1
          cat(g2names[j], " minus ", g2names[i],sep="")
          pvalue2[ipo+j] = cu_estout(fit, vec, conf.int=conf.int, minimal=minimal)
        }
        ipo = ipo+gp21
      }
      if (!minimal) {
        cat ("\nSummary of p-values by repeated measures anova and Fisher's LSD (post-hoc t)")
        cat("\n\n",depname,"compared across repeats of",g1name,"\n")
        cu_pvalmatout(pvalue1,g1names)
        cat("\n\n",depname,"compared across",g2name,"groups\n")
        cu_pvalmatout(pvalue2,g2names)
      }
      letbar = c(rep("",nlevboth12))
    }  # end of interact=F
    #car::plot(Effect(c(g1name,g2name), fit))
    #car::plot(fit, resid(., type = "p") ~ fitted(.), abline = 0)
  } # end of ebars!=4
  else {
    interact = T 
    npairsgp1 = (gp1*(gp1-1))/2;  ndifbar = gp2*npairsgp1
    nd2 = (ndifbar-1)**2; pvalsdiff = rep(NA,nd2)  # above 2 to go with interact=T
    if (anyNA(depvar)) {
      if (!minimal) cat("\nSome missing data, so no overall p-value by Friedman.")
      dss = data.frame(Subject, depvar,group2,group1)
      #print(dss)
      rmw = stats::reshape(dss, direction="wide", idvar="Subject",timevar="group1", v.names="depvar")
      if (!minimal) print(rmw)
      for (i in 1:nlevm1) {
        for (j in (i+1):(nlev)) {
          #print(dss[,i+1]); cat("\nj"); print(dss[,j+1])
          pvalues[ipo+j] = stats::wilcox.test(rmw[,i+1], rmw[,j+1], paired=T, exact=F)$p.value
        }
        ipo = ipo+nlevm1
      }
    }
    else {
      for (i2 in 1:gp2) {
        fried = stats::friedman.test(depvar ~ group1 | Subject, ds,
                              subset=group2 == g2names[i2])
        fried$data.name = paste(g2name,'=',g2names[i2],":",
                                depname,"~",g1name, "| Subject")
        if (!minimal) print (fried)
        # oldw <- getOption("warn")
        # options(warn = -1)
        for (i1 in 1:gp11) {  # comparing factor 1 levels
          for (j1 in (i1+1):gp1) {
            jpval = nlev12m1*((i1-1)*gp2+i2-1)+(j1-1)*gp2+i2-1
            pvalues[jpval] = stats::wilcox.test(
              depvar[(group1==g1names[i1])&(group2==g2names[i2])],
              depvar[(group1==g1names[j1])&(group2==g2names[i2])],
              paired=T, exact=F)$p.value
            # cat("\n",i1,i2,j1,jpval)
          }
        }
      }
      for (ipv in 1:nlev12m1) { # rest of p-value matrix
        i1 = (ipv-1) %/% gp2 + 1;  i2 = (ipv-1) %% gp2 + 1
        for (jpv in (ipv+1):nlevboth12){
          jpval = nlev12m1*(ipv-1) + jpv-1
          if (pvalues[jpval]<0) {
            j1 = (jpv-1) %/% gp2 + 1;  j2 = (jpv-1) %% gp2 + 1
            pvalues[jpval] = 
              stats::wilcox.test(depvar[groupvar==bothnames[jpv]], 
                depvar[groupvar==bothnames[ipv]], paired=F, exact=F)$p.value
            # cat("\n",i1,i2,j1,j2,jpval)
          }
        }
      }
    }
    # options(warn = oldw)
    if (!minimal) cat ("\nSummary of p-values by Wilcoxon (signed-rank within ",
         g2name,", rank-sum between)", sep="")
  }
  if (interact) {
    if (!minimal) {
      cat("\n(For ",g2name," comparisons between repeats of ",
          g1name,", see above",sep="")
      if (ebars==4) cat("\n   before the multi-column table)")
      else cat(")")
      cat("\n\n",depname," compared across ",nameboth12," groups\n",sep="")
      cu_pvalmatout(pvalues,bothnames)
      #      cat("\n",order)
      mapord = cu_mapor2(order,g1names,gp1,gp2,nlevboth12)
      #      cat("\n",order,mapord)
      letbar = cu_letbar(pvalues,psigcld,mapord,nlevboth12)
      #     if (psigcld > 0) letbar = cu_letbugfix(letbar,g1name,gp1,gp2)
    }
  }
  names(repout) = repn #; print(repout)
  if (minimal) return(repout)
  if (!is.na(refmean)) cat ("\nComparing each group mean to reference mean of",
                            refmean,"\n",pval1ts)
  if (ebars != 4 && partialF && interact) {
    cat("\n\nPartial F-test vs simpler model:")
    ndfz=fit$fixDF$X[1]; rsez = stats::sigma(fit)
    # print(fit$fixDF$terms); cat("\nndfz,rsez",ndfz,rsez)
    #fitnoi = lme(depvar ~ group1+group2, random=~1|Subject/group1)
    pvalnoi = 0 #partfout(fitnoi,"no interaction")
    fitg1 = lme(depvar ~ group1, random=~1|Subject/group1, na.action=stats::na.omit)
    pval1 = partfout(fitg1, paste("no",g2name))
    if (pvalnoi > 0.05 || pval1 > 0.05) {
      cat("\nModel is overly complex. Consider dropping the second factor.")
      if (pvminint > 0.05) cat("\nThis is consistent with the smallest interaction p-value of",
                               signif(pvminint,3))
      else cat("\nPuzzling since the smallest interaction p-value is",
               signif(pvminint,3),". Do discuss with someone.")
    }
    else if (pvminint > 0.05) {
      cat(", but smallest interaction p-value is",signif(pvminint,3),
          "\nThe significant partial F-test shows value of",g2name,
          "in the model,\nbut without any interaction with",g1name,
          "reaching significance.")
    }
    cat("\n")
  }
  #cat("\npvalues\n"); print(pvalues)
  if (mainx != 1) {
    letbat = letbar; dft = df
    for (i in 1:gp1) {
      for (j in 1:gp2) {
        jnew = (j-1)*gp1+i; jold = (i-1)*gp2+j
        letbar[jnew] = letbat[jold]; df[,jnew] = dft[,jold]
      }
    }
    pvalt = pvalues
    for (jg1 in 1:(nlevboth12-1)) {
      kg1 = 1 + floor((jg1-1)/gp2); kg2 = 1 + (jg1-1)%%gp2
      for (ig2 in (jg1+1):nlevboth12) {
        mg1 = 1 + floor((ig2-1)/gp2); mg2 = 1 + (ig2-1)%%gp2
        pval = pvalt[ig2-1 + (jg1-1)*(nlevboth12-1)]  # pvalues[ig2-1,jg1]
        jg1n = (kg2-1)*gp1 + kg1; ig2n = (mg2-1)*gp1 + mg1
        if (jg1n > ig2n) {i = jg1n; jg1n = ig2n; ig2n = i}
        pvalues[ig2n-1 + (jg1n-1)*(nlevboth12-1)] = pval
        #cat("\nkg1,kg2,mg1,mg2,pval,jg1n,ig2n:",kg1,kg2,mg1,mg2,pval,jg1n,ig2n)
      }
    }
    #cat("\ndf\n"); print(df)
    #cat("\npvalues\n"); print(pvalues)
  }
  if (interact) {
    if (mainx==1) {n1 = gp1; n2 = gp2} else {n1 = gp2; n2 = gp1}
    pvstruc = cu_pvline(df,pvpairs,pvalues,n1,n2,pvlab,pnosig,psignif,p2stars,p3stars,p4stars,
                        letbar,pvypos,pvnshide,pvspill,chpvref,ebars,dots,yscale)
    # no good if nlevnom < nlev1*nlev2 due to missing combinations
    # cat("\npvstruc,pvlab,letbar:\n")
    # print(pvstruc); print(pvlab,letbar)
    pvdf = pvstruc[[1]]; pvypos = pvstruc[[2]]; pvlab = pvstruc[[3]]
    letbar = pvstruc[[5]]; yrange = pvstruc[[4]]
    #cat("\npvypos,pvdf\n"); print(pvdf); print(pvypos)
    #cat("\npvalues"); print(pvalues); cat("\npvalsdiff"); print(pvalsdiff)
    #cat("\nnpairsgp1,gp2:",npairsgp1,gp2)
    if (itrans==2) {
      #cat("\nddep:\n"); print(ddep)
      for (i in 1:length(depvar)) if (!is.na(depvar[i])) depvar[i] = 10**depvar[i]
      #cat("\nddep:\n"); print(ddep)
      yrange = 10**yrange; yscale = "log10"; ebars = 4  # to get median and iqr
    }
    else if (yscale=="log10") ebars = 4  # to get median and iqr
    # if (is.null(pvtipl)) pvtipl = yrange*5e-5
    # cat("\npvlab:",pvlab)
    pvstruc = cu_pvline(ddf,"all",pvalsdiff,npairsgp1,gp2,pvlab,pnosig,psignif,p2stars,p3stars,p4stars,
                        NULL,NULL,pvnshide,pvspill,chpvref,ebars,dots,"none")
    pvddf = pvstruc[[1]]; pvyposd = pvstruc[[2]]
    letbard = pvstruc[[5]]; pvtipd = pvtipl #pvstruc[[4]]*5e-5
    #cat ("\npvdf:"); print(pvdf); cat ("\npvddf:"); print(pvddf)
    #cat ("\npvypos:"); print(pvypos); cat ("\npvyposd:"); print(pvyposd); cat("\n")
  }
  else {pvdf=NULL; letbar=NULL; letbard=NULL; pvddf=NULL; pvtipd=pvtipl}
  if (is.null(title)) title=paste(depnact, "at different levels of", nameboth12)
  if (is.null(barfill)) barfill="colors"  # needed?
  #ddepn = paste(g1name," effect on ",depname,sep="")
  dtitl = paste(ddepn,"s at different levels of ",g2name,sep="")
  p <- cu_plot2("curepmeas", ddep,dgp1,dgp2,gp1,gp2, NULL, g1name, ddepn, g2name, dtitl,
                plot=plot, linetype=linetype, linesize=linesize, theme=theme,
                letbar=letbard, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                pvdf=pvddf,pvypos=pvyposd,pvstinc=pvstinc,pvlab=pvlab,pvprefix=pvprefix,
                pvsize=pvsize, pvnshide=pvnshide,pvtipl=pvtipd,
                caption=caption, legend=legend, size=size, width=width, posd=posd,
                yscale="none",xangle=xangle, yangle=yangle, dotshape=dotshape, 
                fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
                fontxticks=fontxticks, fontyticks=fontyticks,
                axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
                ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
                orientation="vertical", binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor)
  if (mainx == 1)
    p <- cu_plot2("curepmeas", depvar,group1,group2,gp1,gp2, NULL, g1name, depnact, g2name, title,
                  plot=plot, linetype=linetype, linesize=linesize, theme=theme,
                  letbar=letbar, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                  pvdf=pvdf,pvypos=pvypos,pvstinc=pvstinc,pvlab=pvlab,pvprefix=pvprefix,
                  pvsize=pvsize, pvnshide=pvnshide,pvtipl=pvtipl,
                  caption=caption, legend=legend, size=size, width=width, posd=posd,
                  yscale=yscale,xangle=xangle, yangle=yangle, dotshape=dotshape, 
                  fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
                  fontxticks=fontxticks, fontyticks=fontyticks,
                  axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
                  ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
                  orientation="vertical", binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                  suff=suff, ftype=ftype, fname=fname, fscale=fscale,
                  fwidth=fwidth, fheight=fheight, dpi=dpi, remove=remove)
  else
    p <- cu_plot2("curepmeas",depvar,group2,group1,gp2,gp1, NULL, g2name, depnact, g1name, title,
                  plot=plot, linetype=linetype, linesize=linesize, theme=theme,
                  letbar=letbar, ebars=ebars, dots=dots, barcolor=barcolor, barfill=barfill,
                  pvdf=pvdf,pvypos=pvypos,pvstinc=pvstinc,pvlab=pvlab,pvprefix=pvprefix,
                  pvsize=pvsize, pvnshide=pvnshide,pvtipl=pvtipl,
                  caption=caption, legend=legend, size=size, width=width, posd=posd,
                  yscale=yscale,xangle=xangle, yangle=yangle, dotshape=dotshape, 
                  orientation="vertical", binwfac=binwfac, dotsize=dotsize, dotcolor=dotcolor,
                  fontmain=fontmain, fontxname=fontxname, fontyname=fontyname, 
                  fontxticks=fontxticks, fontyticks=fontyticks,
                  axiscolor=axiscolor, tickcolor=tickcolor, axisthick=axisthick, tickthick=tickthick,
                  ticklength=ticklength, xticks.by=xticks.by, yticks.by=yticks.by, 
                  suff=suff, ftype=ftype, fname=fname, fscale=fscale,
                  fwidth=fwidth, fheight=fheight, dpi=dpi, remove=remove)
  #return(repout)
}
