#' Logistic regression, all possible models, summarize best (or specified) model,
#' do ROC curve(s), AUC, and summary table with best accuracy
#' @param dsgiven , depnam, formula required: dataset, char-strings of name of variable being analyzed, and model formula
#' @param xs list of probability cut points for 2x2 tables (besides 0.5)
#' @param ordinal =NULL (default)/c("...") to specify ordering of variable being analyzed
#' @param logitlog ="logit" (default) to model odds ("logit") or risk ("log")
#' @param start =NULL (default) a list of initial guesses at model coefficients
#' @param wtnam =NULL (default) char-string pointing to variable with weights
#' @param dosimpler =T (default) to compare (T) or not (F) each indep-var across levels of dep-var
#' @param nlevmax =5 (default) for max # of levels in dependent variable
#' @param printfit =F (default) to get (T) or not get (F) fitted probabilities for all subjects
#' @param ndecfit =3 (default) for the number of decimal digits in fitted probabilities
#' @param dodredge =T (default) to get (T) or not get (F) all possible models
#' @param usemod =1 (default) to use the usemod-th best found by dredge
#' @param nmodshow =16 (default) to show top nmodshow models by dredge
#' @param m.min and m.max= to set smallest and largest model sizes in dredge
#' @param fixed =c(,,,) to specify variables that must be in model
#' @param subset logical expression describing models to keep in dredge
#' @param emf =F(default)/T/name (to create culogist.emf or name.emf needed on Mac)
#' @param xlab ="x axis label" (default "1 - Specificity")
#' @param ylab ="y axis label" (default "Sensitivity")
#' @param color ="red" (default) for ROC curve color
#' @param veccolor =c("red","blue","green","black") (default) for multiple curves (polr object)
#' @return returns nothing
#' @examples
#' \dontrun{
#' culogist(Met, "MetSyn", "TG+HDL+LN_TG+BMI+GLUC")  # for AUC and summary table with best accuracy
#' culogist(Met, "MetSyn", "TG+HDL+LN_TG+BMI+GLUC",c(0.3,0.7))  # 3 summary tables (best accuracy, prob=0.3, 0.7)
#' }
#' @export
culogist = function(dsgiven, depnam, formula, xs=NULL, ordinal=NULL,
                    logitlog="logit", start=NULL, wtnam=NULL, dosimpler=T, nlevmax=5,
                    printfit=F, ndecfit=1, dodredge=T, usemod=1,
                    nmodshow=16, m.min=1, m.max=99, fixed=NULL, subset=as.expression(1),
                    emf=F, xlab="1 - Specificity", ylab="Sensitivity",
                    color="red", veccolor=c("red","blue","green","black"))
{
  cuf_apply_defaults(match.call(), environment())
  getstartRR = function() {
    if (is.null(start)) {
      if (twolev) LRobj = glm(forla, family=binomial(link=logit), data=dsnomiss, na.action="na.fail", weights=wts)
      else LRobj = polr(forla, dsnomiss, method="logistic", Hess=T, na.action="na.fail", weights=wts)
      cmat = summary(LRobj)$coefficients; ncoef = nrow(cmat)
      start = cmat[1,1] # half is close to estimate, but Error: cannot find valid starting values
      if (twolev) {
        ndep12 = table(dsnomiss[,1]); preval = min(ndep12)/(ndep12[1]+ndep12[2])
        for (i in 2:ncoef) {
          oddsrat = exp(cmat[i,1])
          start = c(start,log(oddsrat/(1+preval*(oddsrat-1)))) # log(rel.risk)
        }
      }
      else for (i in 2:ncoef) start = c(start,cmat[i,1]/2) # won't work for polr
      # print(start) # nonsense: intercept estimate close to half logistic's
    }
    #start = c(-8.8,-0.035,0.93,0.14,0.0025,-0.022); print(start)
    return(start)
  }
  pastecall = function() {
    return(ifelse(twolev, paste("glm(formula = ", depnam, " ~ ", formula,
           ", family=binomial(link='",logitlog,"'),na.action='na.fail')", callwt,sep=""),
     paste("polr(formula =", depnam, "~", formula, 
           ", method=logistic,na.action='na.fail',Hess=T)", callwt,sep="")))
  }
  ds = dsgiven; idep = 0; startg = start
  for (i in 1:length(ds)) {
    if (names(ds[i])==depnam) 
    {depvar=as.factor(ds[,i]); ds[,i]=depvar; idep=i}
    else if (is.character(ds[1,i])) ds[,i]=as.factor(ds[,i]) # brute force, names?
  }
  if (idep==0) stop("\n\nDependent variable not in dataset:",depnam)
  wts=NULL
  if (!is.null(wtnam)) {
    for (i in 1:length(ds)) {
      if (names(ds[i])==wtnam) {wts=ds[,i]; i=100}
    }
    if (is.null(wts)) stop("\n\nWeighting variable not in dataset:",wtnam)
  }
  if (is.null(formula)) stop("\n\nFormula missing for model")
  if (is.null(wts)) callwt = ""
  else callwt = ",weights=wts"
  levnames = levels(depvar); nlevdep = nlevels(depvar);
  if (nlevdep > nlevmax) stop("\n\nToo many levels:",depnam,". Did you want culinreg?\n")
  if (nlevdep<2) stop("\n\nDependent variable has only one level:",depnam)
  twolev = nlevdep<3
  if (!is.null(ordinal)) if (length(ordinal) != nlevdep) {
    i = 0; 
    cat("\nordinal must have exactly",nlevdep,depnam,"names. ordinal ignored\n")
  }
  else {
    for (i in (1:nlevdep)) {
      if (!(levels(depvar)[i] %in% ordinal)) {
        cat("\n",depnam," '",levels(depvar)[i],"' not in ordinal. ordinal ignored\n",sep="")
        i = 0; break
      }
    }
    if (i!=0) {depvar = factor(depvar, levels=ordinal); ds[,idep]=depvar}
  }
  #cvec = unlist(strsplit(formula, split = "~"))
  #cat(cvec, "\n")
  #depnam = cvec[1]
  vlist = c(idep); dscopy = data.frame(A=ds[,idep], stringsAsFactors = F)
  varnams = cu_parsexp(formula)
  for (iv in 1:length(varnams)) {
    vnam = varnams[iv]; jv = 0
    for (i in 1:length(ds)) {if (names(ds[i])==vnam) {jv = i; break}}
    if (jv==0) stop("\n\nVariable not in dataset:",vnam)
    vlist = c(vlist,jv)
    if (is.character(ds[,jv])) xvar = as.factor(ds[,jv])
    else xvar = ds[,jv]  #ifelse doesn't work - vector?
    dscopy[,iv+1] = xvar
    #print(depvar); print(ds[,jv])
    #if (dosimpler) cu1way(ds[,jv],depvar,minimal=T,depname=vnam,g1name=depnam)
  }
  colnames(dscopy) = c(depnam,varnams)
  dsnomiss = na.omit(dscopy)
  for (jv in 2:(length(varnams)+1)) {
    xvar = dsnomiss[,jv]
    nlev = ifelse(is.factor(xvar),nlevels(xvar),nlevels(as.factor(xvar)))
    if (nlev< 2) 
      stop("\n\nVariable has just one level:",varnams[jv-1],". Re-run without it.\n\n")
  }
  gtnam = ifelse(twolev, paste("=",levnames[nlevdep]),
                 paste("NOT",levnames[1]))
  cat ("\nPredicting",depnam,gtnam)
  #print(dsnomiss)
  if (dosimpler) { #don't know how to set name of vector
    dsnodep = dsnomiss[,2:(length(varnams)+1)]
    cat("\nPredictor summary vs",depnam,"and p-values\n")
    cutable1(dsnodep,dsnomiss[,1],compare=T,brief=T,plot="no",doAll=F)
  }
  if (m.min>m.max) {m.min=1; m.max = 99}
  forla = as.formula(paste(depnam, "~", formula))
  if (logitlog=="log") start = getstartRR() 
  else if (logitlog!="logit")
    {cat("logitlog can only be 'logit' or 'log'. Ignored"); logitlog="logit"}
  #cat(idep,levnames,nlevdep,names(dscopy)); print(forla)
  
  # if (twolev) tryCatch(summary(glm(f, family=binomial(link=logitlog), start=start)),
  #        #warning = function(w){print("THERE WAS A WARNING"); NaN}
  #        error = function(e){print("Advice on getting guesses"); erflag=TRUE})
  #tryCatch(summary(polr()))...
  if (dodredge) {
    #cat ("\n",twolev); print(forla); print(dsnomiss)
    if (twolev) LRobj = glm(forla, family=binomial(link=logitlog), start=start, data=dsnomiss, na.action="na.fail", weights=wts)
    else LRobj = polr(forla, dsnomiss, method="logistic", Hess=T, na.action="na.fail", weights=wts)
    sumglm = summary(LRobj)
    coefvec = LRobj$coefficients; ncoef = length(coefvec)
    if (ncoef > 2) { # no need to dredge with just one predictor
      sumglm$call = pastecall() #; print(start)
      cat ("\nPredicting",depnam,gtnam)
      print(sumglm)
      varlist = as.null("x")
      for (i in 1:ncoef) {
        if (is.na(coefvec[i])) varlist = c(varlist,paste(names(coefvec)[i]," "))
      }
      if (!is.null(varlist))
        stop("\n\nUnestimable predictor(s) (due to perfect correlation with some others):\n",
             varlist,"\nRe-run without the variable(s).\n\n")
      if (ncoef > 15) cat("\nPROGRAM MAY DISAPPEAR AND NOT RETURN IF TOO MANY PREDICTORS",
                          "\nIF THIS HAPPENS, USE FEWER THAN 16 PREDICTORS\n")
      if (m.min>m.max) m.min=1
      dredobj = dredge(LRobj, m.lim=c(m.min, m.max), fixed=fixed, subset=subset)
      nvar = length(dredobj)-6 #always 6?
      printmax = min(nrow(dredobj), nmodshow)
      print(dredobj[1:printmax,]) # always prints preamble with "call" and footnote about ranking
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
          cat("\nvs ",rownames(dredobj[ir]),": LR, degf, \u03C7\u00B2 p-value ",
              signif(twolr,3),", ",ndf,", ",cu_pval9(pvalchi),sep="")
        }
      }
      dredvars = names(dredobj[usemod,2:(1+nvar)]) # names returns NULL if nvar=1
      # print(dredvars)
      for (i in length(dredvars):1) {
        if (is.na(dredobj[usemod,(i+1)])) dredvars = dredvars[-i]
      }
      if (length(dredvars)>0) { #redo given model if dredge crashes?
        formula = paste(depnam, paste(dredvars, collapse="+"), sep=" ~ ")
        forla = as.formula(formula)
      }
      if (length(dredvars)<nvar) {
        if (!is.null(startg)) {
          cat("\nGiven start values ignored for final model. If no convergence,",
              "\n   rerun with just final model and appropriate start values.")
          start = NULL; 
        }
        if (logitlog=="log") {start = NULL; start = getstartRR()}
      }
    }
  }
  oldw <- getOption("warn")
  options(warn = -1)
  if (twolev) LRobj = glm(forla, family=binomial(link=logitlog), start=start,
                          data=dscopy, na.action="na.exclude", weights=wts)
  else LRobj = polr(forla, dsnomiss, method="logistic", Hess=T, na.action="na.exclude", weights=wts)
  options(warn = oldw)
  if (printfit) {
    cat ("\n",depnam," and Fitted probabilities for all subjects\n",sep="")
    ch12 = ""
    if (ndecfit<2) {nper=20; if (ndecfit<=0) {ch12 = " "; ndecfit=0}}
    else nper=10
    if (twolev) nprow = 1  #; inx = length(LRobj$y)}
    else nprow = nlevdep
    nwid = ndecfit+3; inx = length(depvar); nlin = ceiling(inx/nper); ib = 1;
    for (ilin in 1:nlin) {
      ic = min(ib+nper-1,inx)
      cat("\n"); for (i in ib:ic) cat(sprintf("%*.0f",nwid,i))
      cat("\n"); for (i in ib:ic) cat(sprintf("%*s",nwid,as.character(depvar[i])),sep="")
      for (jr in 1:nprow) {
        cat("\n"); for (i in ib:ic) {
          cat(ch12,sprintf("%.*f",ndecfit,
                           ifelse(twolev,LRobj$fitted.values[i],LRobj$fitted.values[i,jr])),sep="")
        } 
      }
      ib = ic+1
    }
    cat ("\n")
  }
  sumglm = summary(LRobj)
  sumglm$call = pastecall() #; print(start)
  cat ("\nPredicting",depnam,gtnam)
  print(sumglm)
  cmat = sumglm$coefficients
  ie = nrow(cmat)
  if (twolev) ib = 2
  else {ib = 1; ie = ie+1-nlevdep} # polr puts nlevdep-1 intercepts at end
  # print(cmat)
  chrror = ifelse(logitlog=="log","Relative Risk","Odds Ratio")
  cat(chrror,"stats derived from Coefficients above \n")
  for (i in ib:ie)  {  # is this correct for relative risk?
    coeff = cmat[i,1]; RROR = exp(coeff); SE = cmat[i,2]
    CLlo = exp(coeff - 1.96*SE); CLhi = exp(coeff + 1.96*SE)
    if (i==ib) {
      newdf = data.frame(B=signif(coeff,4),C=signif(SE,4), D=signif(RROR,4), E=signif(CLlo,4),
                         F=signif(CLhi,4), stringsAsFactors = F)
      colnames(newdf) = c("coeff", " Std Err", chrror, " lower .95", "upper .95")
    }
    else {
      newrow = c(signif(coeff,4), signif(SE,4), signif(RROR,4), signif(CLlo,4), signif(CLhi,4))
      newdf = rbind(newdf, newrow)
    }
  }
  rownames(newdf)=names(coefficients(LRobj)[ib:ie])
  print(newdf)
  curoc(LRobj, depvar, twolev=twolev, xs=xs, namedep=depnam, logitlog=logitlog,
        printfit=printfit, emf=emf, xlab=xlab, ylab=ylab, 
        label.ordering = ordinal, color=color, veccolor=veccolor)
}
