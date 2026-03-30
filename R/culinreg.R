#' Multiple linear regression, all possible models, summarize best (or specified) model,
#' do added variable plots
#' @param dsgiven , depnam, formula required: dataset, char-string of name of variable being analyzed, 
#' @param formula of model "preda+predb+predc*predd" etc. Add "+0" for zero intercept
#' @param printfit =F (default) to get (T) or not get (F) fitted values for all subjects
#' @param ndecfit =3 (default) for the number of decimal digits in fitted values
#' @param dodredge =T (default) to get (T) or not get (F) all possible models
#' @param usemod =1 (default) to use the usemod-th best found by dredge
#' @param nmodshow =16 (default) to show top nmodshow models by dredge
#' @param m.min and m.max= to set smallest and largest model sizes in dredge
#' @param fixed = c(,,,) to specify variables that must be in model
#' @param subset =logical expression describing models to keep in dredge
#' @param fitasx =T(default) or set to T to plot observed as x
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return returns nothing
#' @examples
#' culinreg(NEJM, "tcstudy", "tcpre+tgpre+Diet")
#' @importFrom car avPlots
#' @export
culinreg = function(dsgiven, depnam, formula,
                  weights=NULL, printfit=F, ndecfit=2, dodredge=T, usemod=1,
                  nmodshow=16, m.min=1, m.max=99, fixed=NULL, subset=as.expression(1),
                  fitasx=T,ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                  fheight=NULL, dpi=300, remove=NULL)
{
  cuf_apply_defaults(match.call(), environment())
  ds = dsgiven; idep = 0
  for (i in 1:length(ds)) {
    if (names(ds[i])==depnam) idep=i
  }
  if (idep==0) stop("\n\nDependent variable not in dataset:",depnam,"\n\n")
  if (!is.numeric(ds[,idep])) stop("\n\nDependent variable not numeric:",depnam,". Did you want culogist?\n\n")
  if (is.null(formula)) stop("\n\nFormula missing for model\n\n")
  dscopy = data.frame(A=ds[,idep], stringsAsFactors = F)
  varnams = cu_parsexp(formula); vlist = c(idep)
  ndscol = 1; ifzeroint = F; colnames(dscopy)[1] = depnam
  for (iv in 1:length(varnams)) {
    vnam = varnams[iv]; jv = 0
    if (vnam=="0" || vnam=="1") ifzeroint = T
    else {
      for (i in 1:length(ds)) {if (names(ds[i])==vnam) {jv = i; break}}
      if (jv==0) stop("\n\nVariable not in dataset:",vnam)
      vlist = c(vlist,jv)
      if (is.character(ds[,jv])) xvar = as.factor(ds[,jv])
      else xvar = ds[,jv]  #ifelse doesn't work - vector?
      ndscol = ndscol+1; dscopy[,ndscol] = xvar
      colnames(dscopy)[ndscol] = vnam
      # print(ds[,nindep])
    }
  }
  if (ndscol==1) stop("\n\nmodel formula has no predictors:",formula)
  dsnomiss = stats::na.omit(dscopy)
  dscorr = dsnomiss
  for (jv in 2:ndscol) {
    xvar = dsnomiss[,jv]
    nlev = ifelse(is.factor(xvar),nlevels(xvar),nlevels(as.factor(xvar)))
    if (nlev< 2) 
        stop("\n\nVariable has just one level:",colnames(dscopy)[jv],
             ". Re-run without it.\n\n")
    if (is.factor(xvar)) dscorr[,jv] = as.numeric(xvar)
  }
  cat("\nPairwise correlations and P-values among the variables:\n")
  print(rcorr(as.matrix(dscorr)))
  fpre = paste(depnam, "~", formula)
  forla = stats::as.formula(fpre)
  if (dodredge && ndscol>2) {
    fitlm = stats::lm(forla, data=dsnomiss, na.action="na.fail")
    sumfit = summary(fitlm)
    sumfit$call = paste("lm(formula = ", fpre,
        ifelse(ifzeroint," (zero intercept)",""), ", data=",
        deparse(substitute(dsgiven)),", na.action = 'na.fail')",sep="")
    print(sumfit)
    coefvec = fitlm$coefficients; ncoef = length(coefvec)
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
    dredobj = dredge(fitlm, m.lim=c(m.min, m.max), fixed=fixed, subset=subset)
    nvar = length(dredobj)-6+ifelse(ifzeroint,1,0) #always 6?
    #cat("\n",nvar,dredobj[1,nvar+2])
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
        twolr = 2*(loglz-logl1); ndf = npz-np1; pvalchi = 1-stats::pchisq(twolr,ndf)
        cat("\nvs #",rownames(dredobj[ir]),": LR, degf, \u03C7\u00B2 p-value",
            signif(twolr,3),ndf,cu_pval9(pvalchi))
      }
    }
    i01 = ifelse(ifzeroint,0,1) 
    ib = i01+1; ie = i01+nvar
    dredvars = names(dredobj[usemod,ib:ie])
    for (i in length(dredvars):1) {
      if (is.na(dredobj[usemod,(i+i01)])) {
        dredvars = dredvars[-i]
      }
    }
    if (length(dredvars)>0) {
      fpre = paste(depnam, "~", paste(dredvars, collapse="+"))
      if (ifzeroint) fpre = paste(fpre,"+0")
      forla = stats::as.formula(fpre)
    }
  }
  fitlm = stats::lm(forla, data=dscopy, na.action = "na.exclude", y=T)
  if (printfit) {
    cat ("\n",depnam," Observed and Fitted values for all subjects\n",sep="")
    ndecfit = min(5,max(0,ndecfit))
    nper=10; nwid = 8; inx = length(fitlm$y); nlin = ceiling(inx/nper); ib = 1
    for (ilin in 1:nlin) {
      ic = min(ib+nper-1,inx)
      cat("\n"); for (i in ib:ic) cat(sprintf("%*.0f",nwid,i))
      cat("\n"); for (i in ib:ic) {
        cat(sprintf("%8.*f",ndecfit,fitlm$y[i]),sep="")
      }
      cat("\n"); for (i in ib:ic) {
        cat(sprintf("%8.*f",ndecfit,fitlm$fitted.values[i]),sep="")
      }
      ib = ic+1
    }
    cat ("\n")
  }
  avPlots(fitlm) #avPlots returns coordinates, so can't print object
  cu_plout(fitlm,"culinreg", avplots=T, ftype=ftype, fname=fname, scale=fscale,
           width=fwidth, height=fheight, dpi=dpi, remove=remove) 
  sumfit = summary(fitlm)
  sumfit$call = paste("lm(formula = ", fpre, 
    ifelse(ifzeroint," (zero intercept)",""), ", data=",
    deparse(substitute(dsgiven)),", na.action = 'na.exclude')",sep="")
  print(sumfit)
  #  code above doesn't work for lme - true? look into it.
  ypred = stats::predict(fitlm)
  if (fitasx)
    scao = cuscatter(dsnomiss[,1],ypred, doline=T, minimal=T, showr2eqn="both",
      xname=paste(depnam,"fitted"), yname=paste(depnam,"observed"), 
      title=paste(depnam,"observed vs fitted"))
  else scao = cuscatter(ypred,dsnomiss[,1], doline=T, minimal=T, showr2eqn="both",
      yname=paste(depnam,"fitted"), xname=paste(depnam,"observed"), 
      title=paste(depnam,"fitted vs observed"))
}
