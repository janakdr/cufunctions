#' internal cufunction to do dredge work, called by culogist, cucox, culinreg
#' @keywords internal
cu_dredge = function(regobj,forlag,
             nmodshow=16, m.min=1, m.max=99, fixed=NULL, subset=as.expression(1))
{
  coefvec = regobj$coefficients; ncoef = length(coefvec); dobj = ncoef-1; forla = forlag
  print(coefvec); print(ncoef); print(forla); print(dobj)
  if (ncoef > 2) { # no need to dredge with just one predictor
    varlist = as.null("x")
    for (i in 1:ncoef) {
      if (is.na(coefvec[i])) varlist = c(varlist,paste(names(coefvec)[i]," "))
    }
    if (!is.null(varlist))
      stop("\n\nUnestimable predictor(s) (due to perfect correlation with some others):\n",
           varlist,"\nRe-run without the variable(s).\n\n")
    if (ncoef > 15) cat("\nPROGRAM MAY DISAPPEAR AND NOT RETURN IF TOO MANY PREDICTORS",
                        "\nIF THIS HAPPENS, USE FEWER THAN 16 PREDICTORS\n")
    if (m.min>m.max) {m.min=1; m.max = 99}
    cat("\nbefore dredge")
    dredobj = dredge(regobj, m.lim=c(m.min, m.max), fixed=fixed, subset=subset)
    cat("\nafter dredge")
    nvar = length(dredobj)-6 #always 6?
    printmax = min(nrow(dredobj), nmodshow)
    print(dredobj[1:printmax,]) # always prints preamble with "call" and footnote about ranking
    if (usemod>printmax) usemod=1
    npz = dredobj[usemod,nvar+2]; loglz = dredobj[usemod,nvar+3]
    cat("\n\nLikelihood ratio (LR) tests of chosen ",ifelse(usemod==1,"top",
                                                            ifelse(usemod==2,"2nd",ifelse(usemod==3,"3rd",paste(usemod,"th best",sep="")))),
        " model #",rownames(dredobj[usemod]), " (assuming nesting of smaller models)",sep="")
    for (ir in 1:printmax) {
      np1 = dredobj[ir,nvar+2]; logl1 = dredobj[ir,nvar+3]
      if (np1 < npz) {
        twolr = 2*(loglz-logl1); ndf = npz-np1; pvalchi = 1-pchisq(twolr,ndf)
        cat("\nvs ",ir,"th best #",rownames(dredobj[ir]),": LR, degf, \u03C7\u00B2 p-value ",
            signif(twolr,3),", ",ndf,", ",cu_pval9(pvalchi),sep="")
      }
    }
    dredvars = names(dredobj[usemod,2:(1+nvar)]) # names returns NULL if nvar=1
    # print(dredvars)
    for (i in length(dredvars):1) {
      if (is.na(dredobj[usemod,(i+1)])) dredvars = dredvars[-i]
    }
    ndre = length(dredvars)
    if (ndre>0) { #redo given model if dredge ends with null?
      formula = paste(depnam, paste(dredvars, collapse="+"), sep=" ~ ")
      forla = as.formula(formula)
    }
  }
  dobj$form = forla; dobj$ndred = length(dredvars)
  print(dobj)
  return(dobj)
}
