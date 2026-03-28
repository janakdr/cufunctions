#' Cox modeling, all possible models, summarize best (or specified) model,
#' LR tests, ROCs, K-M of Cox score strata, predicted K-M for each factor
#' @param dsgiven , timnam, depnam, formula  required: dataset, char-strings of names of time-var and variable being analyzed, and model formula
#' @param dopredkm =T (default) to get (T) or not get (F) predicted Kaplan-Meier curves
#' @param doroc =T (default, at 50,75,100\% events) for ROC curves [c(,,) list to specify times]
#' @param docoxkm =T (default, quartiles) for K-M vs Cox risk score [c(,,) list to specify cut-pts]
#' @param wtnam =NULL (default) char-string pointing to variable with weights
#' @param printfit =F (default) to get (T) or not get (F) fitted risk scores for all subjects
#' @param dodredge =T (default) to get (T) or not get (F) all possible models
#' @param usemod =1 (default) to use the usemod-th best found by dredge
#' @param nmodshow =16 (default) to show top nmodshow models by dredge
#' @param m.min and m.max= to set smallest and largest model sizes in dredge
#' @param fixed = c(,,) list to specify variables that must be in model
#' @param subset =logical expression describing models to keep in dredge
#' @param tmax =0 (default for no max T), numeric study end time
#' @param xlabpredkm ="Time" (default) for predicted K-M x-axis label
#' @param ylabpredkm ="Survival" (default) for predicted K-M y-axis label
#' @param titlepredkm ="Predicted Kaplan-Meier Curves" (default) for predicted K-M title
#' @param palette ="hue" (default) "grey" "RdBu" "Blues" "lancet" etc
#' @param fun ="pct" (default) for survival, "event" for cumulative events, "cumhaz" for cumulative hazard
#' @param color ="red" (default) for single ROC curve color (glm object)
#' @param xlabroc ="x axis label" (default "1 - Specificity")
#' @param ylabroc ="y axis label" (default "Sensitivity")
#' @param allkmpairs =T (default) or F (to not compare each pair of curves)
#' @param xlabkm =NULL (default) for K-M x-axis label
#' @param ylabkm =NULL (default) for K-M y-axis label
#' @param titlekm =NULL (default) for K-M title
#' @param nverkm,nhorkm,nverroc,nhorroc =2 (default) #km,rocplots vert,horiz
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return returns nothing
#' @examples
#' \dontrun{
#' cucox(coxdata, "TimeToEvent", "Outcome", "C.Index + scale(LVEF) + scale(BNP)")
#' }
#' @importFrom ggpubr ggarrange
#' @importFrom survminer ggadjustedcurves
#' @export
cucox = function(dsgiven, timnam, depnam, formula, dopredkm=T, doroc=T, docoxkm=T,
                 wtnam=NULL, printfit=F, dodredge=T, usemod=1,
                 nmodshow=16, m.min=1, m.max=99, fixed=NULL, subset=as.expression(1),
                 tmax=0, xlabpredkm="Time", ylabpredkm="Survival", 
                 titlepredkm="Predicted Kaplan-Meier", palette="hue", fun="pct",
                 color ="red", xlabroc="1 - Specificity", ylabroc="Sensitivity", 
                 allkmpairs=F, xlabkm=NULL, ylabkm=NULL, titlekm=NULL,
                 nverkm=2, nhorkm=2, nverroc=2, nhorroc=2,
                 ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                 fheight=NULL, dpi=300, remove=NULL)
{
  cuf_apply_defaults(match.call(), environment())
  ds = dsgiven; itim=0; idep = 0
  for (i in 1:length(ds)) {
    if (names(ds[i])==timnam) itim = i
    else if (names(ds[i])==depnam) {ds[,i]=as.integer(ds[,i]); idep=i}
  }
  if (itim==0) stop("\n\nTime-to-event variable not in dataset:",timnam,"\n\n")
  if (idep==0) stop("\n\nOutcome variable not in dataset:",depnam,"\n\n")
  if (!is.numeric(ds[,itim])) stop("\n\nTime-to-event variable not numeric:",timnam,"\n\n")
  nlev = nlevels(as.factor(ds[,idep]))
  if (nlev!=2) stop("\n\nOutcome variable ",depnam," has ",nlev," values. Must be 2.\n\n")
  #cat("\n",itim,idep)
  wts=NULL
  if (!is.null(wtnam)) {
    for (i in 1:length(ds)) {
      if (names(ds[i])==wtnam) {wts=ds[,i]; i=100}
    }
    if (is.null(wts)) stop("\n\nWeighting variable not in dataset:",wtnam,"\n\n")
  }
  if (is.null(formula)) stop("\n\nFormula missing for model\n\n")
  vlist = c(itim,idep)
  dscopy = data.frame(A=ds[,itim], B=ds[,idep], stringsAsFactors = F)
  if (tmax > 0) for (i in 1:nrow(dscopy)) {
    if (dscopy[i,1] > tmax) {dscopy[i,1] = tmax; dscopy[i,2] = 0}
  }
  varnams = cu_parsexp(formula)
  for (iv in 1:length(varnams)) {
    vnam = varnams[iv] #; jv = 0
    if (substr(vnam,1,6)=="scale(") 
      {vnam = substr(vnam,7,nchar(vnam)-1); varnams[iv] = vnam}
    # for (i in 1:length(ds)) {if (names(ds[i])==vnam) {jv = i; break}}
    jv = match(vnam,names(ds))
    if (is.na(jv)) stop("\n\nVariable not in dataset:",vnam)
    vlist = c(vlist,jv)
    if (is.character(ds[,jv])) xvar = as.factor(ds[,jv])
    else xvar = ds[,jv]  #ifelse doesn't work - vector?
    dscopy[,iv+2] = xvar
    #cat(iv,jv); print(ds[,jv])
  }
  colnames(dscopy) = c(timnam,depnam,varnams)
  dsnomiss = na.omit(dscopy)
  for (jv in 3:(length(varnams)+2)) {
    xvar = dsnomiss[,jv]
    nlev = ifelse(is.factor(xvar),nlevels(xvar),nlevels(as.factor(xvar)))
    if (nlev< 2) 
      stop("\n\nVariable has just one level:",varnams[jv-2],". Re-run without it.\n\n")
  }
  if (is.null(wts)) callwt = ""
  else callwt = ",weights=wts"
  fpre = paste("Surv(", timnam, ", ", depnam, ") ~ ", formula, sep="")
  forla = as.formula(fpre)
  #cat(fpre) #print(forla)
  # dodredge = TRUE # need a way to flag all predictors for ggadjustedcurves
  if (dodredge && length(varnams)>1) {
    # superseded by dsnomiss above? dsnomiss = na.omit(ds)
    fitcox = coxph(forla, data=dsnomiss, x=T, weights=wts, na.action="na.fail")
    print(fitcox)
    coefvec = fitcox$coefficients; ncoef = length(coefvec)
    varlist = as.null("x")
    for (i in 1:ncoef) {
      if (is.na(coefvec[i])) varlist = c(varlist,paste(names(coefvec)[i]," "))
    }
    if (!is.null(varlist))
      stop("\n\nUnestimable predictor(s) (due to perfect correlation with some others):\n",
           varlist,"\nRe-run without the variable(s).\n\n")
    if (ncoef > 2) { # no need to dredge with just one predictor
      if (ncoef > 15) cat("\nPROGRAM MAY DISAPPEAR AND NOT RETURN IF TOO MANY PREDICTORS",
                          "\nIF THIS HAPPENS, USE FEWER THAN 16 PREDICTORS\n")
      if (m.min>m.max) m.min=1
      dredobj = dredge(fitcox, m.lim=c(m.min, m.max), fixed=fixed, subset=subset)
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
        fpre = paste("Surv(", timnam, ", ", depnam, ") ~ ", paste(dredvars, collapse="+"),sep="")
        forla = as.formula(fpre)
      }
    }
    else dredvars = rep(formula,1)
  }
  fitcox = coxph(forla, data=dscopy, x=T, weights=wts, na.action = "na.exclude")
  zpho = cox.zph(fitcox); nophvn = 0; nophvar = rep(0,1)
  for (i in 1:length(zpho$table[,3])) if (zpho$table[i,3] < 0.2)
    {nophvn = nophvn+1; nophvar[nophvn] = i}
  cat("\nTest of Proportional Hazards (PH) assumption using Schoenfeld residuals\n")
  print(zpho)  # ; plot(cox.zph(fitcox))
  cat("PH assumption ")
  if (nophvn > 0) {
    chspc = "may not be met by"
    for (i in 1:nophvn) {cat(chspc,rownames(zpho$table)[nophvar[i]]); chspc=","}
  }
  else cat("appears to be met for all predictors in final model")
  cat("\n")
  #  dsnam = deparse(substitute(ds))[1]
  if (dopredkm) {
    dslen = length(ds); nscat=0
    for (i in 1:length(dredvars)) {
      fac2=dredvars[i]; g2num=0
      for (j in 1:dslen) {if (names(ds[j])==fac2) g2num=j} # use simpler fn
      if (is.integer(ds[,g2num]) | is.factor(ds[,g2num]) | is.logical(ds[,g2num])) {
        p = ggadjustedcurves(fitcox, data=ds, variable=fac2, 
          palette=palette, fun=fun, xlab=xlabpredkm, ylab=ylabpredkm,
          title=titlepredkm)  #  , legend.title=fac2
        nscat = nscat+1
        if (nscat==1) plist = list(p)
        else plist[[nscat]] = p
      }
    }
    if (nscat > 0) {
      if (nscat==1) {nverkm=1; nhorkm=1}
      pout = ggarrange(plotlist=plist,nrow=nverkm,ncol=nhorkm)
      cu_plout(pout,"cucox-predkm", ftype=ftype, fname=fname,
        scale=fscale, width=fwidth, height=fheight, dpi=dpi,remove=remove)
    }
  }
  #cat("\n") # crashes if later better to process $table[,3]?
  if (is.logical(doroc) && !doroc) doroc = NULL
  if (!is.null(doroc)) {
    dsroc = data.frame(A=fitcox$y[,1], B=fitcox$y[,2], 
                       C=fitcox$linear.predictors)
    colnames(dsroc) = c(timnam,depnam,"cox-score"); nsubj = length(fitcox$y)
    dsord <- dsroc[order(dsroc[,2],dsroc[,1]),]; ibeg = match(1,dsord[,2])
    if (is.logical(doroc)) {
      evemed = median(dsord[ibeg:nsubj,1])
      eve75 = quantile(dsord[ibeg:nsubj,1],0.75); evend = dsord[nsubj,1]
      doroc = c(evemed,eve75,evend); cm75e = c("half","75% of","all")
    }
    else cm75e = NULL
    #print(dsroc); print(dsord); print(doroc)
    par(font.lab=2, mfrow=c(nverroc,nhorroc))
    for (iroc in 1:length(doroc)) {
      timroc = doroc[iroc]
      nsubnom = 0; vecpronom = rep(0,1); veccasnom = rep(0,1)
      for (i in 1:nsubj) {
        if (dsord[i,1] > timroc) outc = 0
        else if (dsord[i,2] == 1) outc = 1
        else outc = -1
        if (outc >= 0) {
          nsubnom = nsubnom+1; vecpronom[nsubnom]=dsord[i,3]; veccasnom[nsubnom]=outc
        }
      }
      mytab = table(vecpronom > 0, veccasnom, dnn=c("Cox-score",depnam))
      row1name = "<=0"; row2name = paste(" >0")
      cat("\nAt time ",timroc,",",
          ifelse(is.null(cm75e),"",paste(" when ",cm75e[iroc],
                 " the events have occurred, with data in ",sum(mytab),":\n",sep="")),
          " 2x2 Table of maximum accuracy\n",sep="")
      cu_2x2out(mytab,row1name,row2name)
      PredObj = prediction(vecpronom, veccasnom)
      perfAUC = performance(PredObj, measure="auc"); aucstr = strtrim(perfAUC@"y.values",5)
      cat("Area Under the Curve (AUC) =", aucstr, "\n")
      AUCtitle = paste(depnam, " at ",timroc,"\n", "AUC = ", aucstr)
      perfPlot = performance(PredObj, "tpr", "fpr")
      ROCR::plot(perfPlot, add=FALSE, 
                 main=AUCtitle, xlab=xlabroc, ylab=ylabroc, col=color)
      add=TRUE
    }
  }
  if (printfit) {
    cat ("\n",timnam," ('+' means censored) and Fitted risk scores for all subjects\n",sep="")
    nper=10; nwid = 7; nlin = ceiling(nsubj/nper); ib = 1
    for (ilin in 1:nlin) {
      ic = min(ib+nper-1,nsubj)
      cat("\n"); for (i in ib:ic) cat(sprintf("%7.0f",i))
      cat("\n "); for (i in ib:ic) {
        tim = fitcox$y[i,1]
        ch1=" "; if (fitcox$y[i,2]=="0") ch1="+"
        cat(sprintf("%6.0f",tim),ch1,sep="")
      }
      cat("\n"); for (i in ib:ic) {
        cat(sprintf("%7.3f",fitcox$linear.predictors[i]))
      }
      ib = ic+1
    }
    cat ("\n")
  }
  if (is.logical(docoxkm) && !docoxkm) docoxkm = NULL
  if (!is.null(docoxkm)) {
    rq = quantile(fitcox$linear.predictors); 
    if (is.logical(docoxkm)) {
      numcat=3; docoxkm = rq[2:4]; stratnam = c("Q1","Q2","Q3","Q4")
      cat("\nKaplan-Meier analysis of CoxStrata quartiles (",
          signif(rq[2:4],3),")\n")
    }
    else {
      numcat = length(docoxkm); stratnam = rep("0",numcat+1)
      for (i in 1:numcat) stratnam[i] = paste("<",docoxkm[i],sep="")
      stratnam[numcat+1] = paste(">=",docoxkm[i],sep="")
      cat("\nKaplan-Meier analysis of CoxStrata strata\n")
    }
    CoxStrata = rep("0",nsubj); numcat=numcat+1; docoxkm[numcat] = 1e50
    for (i in 1:nsubj) {
      riskscore = fitcox$linear.predictors[i]
      for (j in 1:numcat) if (riskscore < docoxkm[j]) break
      CoxStrata[i] = stratnam[j]
    }
    #dskm = data.frame(A=fitcox$y[,1],B=fitcox$y[,2],C=riskcat)
    #colnames(dskm) = c(timnam,depnam,"CoxStrata")
    #print(riskquant); print(dskm)
    kmtype = ifelse(fun=="pct", "survival", "ci")
    Time = fitcox$y[,1]; Outcome = fitcox$y[,2]
    cukm(Time, Outcome, CoxStrata, kmtype=kmtype, allkmpairs=allkmpairs,
         xlab=xlabkm, ylab=ylabkm, title=titlekm, ftype=ftype, fname=fname, fscale=fscale,
         fwidth=fwidth, fheight=fheight, dpi=dpi, remove=remove)
    cat("\n\n")
  }
  fitcox$call = paste("coxph(formula =", fpre, ",data=",
                deparse(substitute(dsgiven)),",na.action = 'na.exclude')",callwt,sep="")
  sumfitc = summary(fitcox)
  colnames(sumfitc$coefficients)[2] = "Hazard Ratio"
  colnames(sumfitc$conf.int)[1] = "Hazard Ratio"
  colnames(sumfitc$conf.int)[2] = "HR for non-event"
  print(sumfitc)
  cat("Concordance (aka Harrell's C-index, C-statistic) details:\n   ",
    fitcox$concordance[1]," pairs concordant; ", 
    fitcox$concordance[2]," pairs disconcordant",
    "\n(",nsubj*(nsubj-1)/2 - sum(fitcox$concordance[1:5]),
    " incomparable: censored vs later event, or both censored", sep="")
  if (sum(fitcox$concordance[3:5]) >0) {
    cat(", also"); ch1 = ""
    if (fitcox$concordance[3] >0) {cat(ch1,fitcox$concordance[3],"tied.time"); ch1=";"}
    if (fitcox$concordance[4] >0) {cat(ch1,fitcox$concordance[4],"tied.event"); ch1=";"}
    if (fitcox$concordance[5] >0) cat(ch1,fitcox$concordance[5],"tied.time&event")
  }
  cat(")\n\n")
  cat("Hazard Ratio is exp(coef).\n   HR of non-event is for event not happening, equal to 1/HR.\n")
  cat("For scale(continuous variable), HR is for a 1 SD increase.\n")
  cat("For a continuous variable w/o scale, HR is for a 1 unit increase.\n")
  # fitcox
}
