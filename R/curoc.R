#' For logistic-regression, does ROC curve(s), AUC, and summary table with best accuracy
#' @param LRobj logistic-regression object (glm or polr) (required)
#' @param depvar outcome variable (required)
#' @param twolev =T (default) for glm object; =F for polr object (>2 levels)
#' @param xs list of probability cut points for 2x2 tables (besides 0.5)
#' @param namedep outcome variable name labeling graph
#' @param logitlog ="logit" (default) to model odds ("logit") or risk ("log")
#' @param emf =F(default)/T/name (to create culogist.emf or name.emf needed on Mac)
#' @param xlabroc ="x axis label" (default "1 - Specificity")
#' @param ylabroc ="y axis label" (default "Sensitivity")
#' @param label.ordering = c(two labels) to override alphabetical order
#' @param color ="red" (default) for single ROC curve color (glm object)
#' @param veccolor =c("red","blue","green","black") (default) for multiple curves (polr object)
#' @return returns nothing
#' @examples
#' logfit <- glm(MetSyn ~ TG + HDL, data = Met, family = binomial)
#' curoc(logfit, Met$MetSyn)
#' curoc(logfit, Met$MetSyn, xs=c(0.3, 0.7))  # AUC with 3 summary tables
#' polrfit <- MASS::polr(as.factor(WTCAT) ~ TG + HDL, data = Met)
#' curoc(polrfit, Met$WTCAT, twolev=FALSE)  # multi-level via polr
#' @export
curoc = function(LRobj, depvar, twolev=TRUE, xs=NULL, namedep=NULL, logitlog="logit", 
                 printfit=F, emf=F, xlabroc="1 - Specificity", ylabroc="Sensitivity",
                 label.ordering = NULL, color="red", veccolor=c("red","blue","green","black"))
{
  cuf_apply_defaults(match.call(), environment())
  if (is.null(namedep)) namedep = deparse(substitute(depvar))
  depvar = as.factor(depvar)
  levnames = levels(depvar); nlev = nlevels(depvar); nsubj <- length(depvar)
  if (nlev < 2) stop(namedep," must have at least 2 levels. Has only",nlev,"level")
  nroc <- nlev-1;
  if (twolev) {
    if (nlev != 2) stop("If >2 levels, call polr, then call curoc with twolev=F")
    veccase = as.character(depvar)
    vecprob = stats::fitted.values(LRobj); veclegs = c(rep("AUC",nroc))
  }
  else {
    veccase = c(rep(0,nsubj)); vecprob = c(rep(0,nsubj))
    veccolors = c(rep("",nroc)); veclegs = veccolors
    coeffs = summary(LRobj)$coefficients[,1]; ncoeffs = length(coeffs)
    #print(coeffs)
  }
  add = FALSE; jcolor=0
  graphics::par(font.lab=2, mfrow=c(1,1)) # in case set elsewhere
  for (jroc in 1:nroc) {
    if (!twolev) {
      if (jcolor < length(veccolor)) {jcolor = jcolor+1} else jcolor = 1
      color = veccolor[jcolor]; veccolors[jroc] = color
      for (i in 1:nsubj) {
        vecprob[i] = vecprob[i] + stats::fitted.values(LRobj)[i,jroc]
        if (as.numeric(depvar[i]) <= jroc) {veccase[i] = "No"} else veccase[i] = "Yes"
      }
    }
    gtnam = ifelse(jroc < nroc, paste(">",levnames[jroc]),
                   paste("=",levnames[nlev]))
    cat ("\nPredicting",namedep,gtnam)
    ch12 = ""; ndecfit=2
    if (ndecfit<2) {nper=20; if (ndecfit<=0) {ch12 = " "; ndecfit=0}}
    else nper=10
    nwid = ndecfit+3; inx = length(depvar); nlin = ceiling(inx/nper); ib = 1;
    if (printfit) for (ilin in 1:nlin) {
      ic = min(ib+nper-1,inx)
      cat("\n"); for (i in ib:ic) cat(sprintf("%*.0f",nwid,i))
      cat("\n"); for (i in ib:ic) cat(sprintf("%*s",nwid,as.character(depvar[i])),sep="")
      cat("\n"); for (i in ib:ic) cat(sprintf("%*s",nwid,veccase[i]))
      cat("\n"); for (i in ib:ic) {  # depvar identical to veccase??
        cat(ch12,sprintf("%.*f",ndecfit,vecprob[i],sep=""))
      } 
      ib = ic+1
    }
    cat ("\n")
    for (k in 0:length(xs)) {
      if (k==0) {prob = 0.5} else prob = max(0.0001,min(0.9999,xs[k]))
      cutpt = ifelse(twolev, ifelse(logitlog=="logit",log(prob/(1-prob)),log(prob)),
                                    coeffs[ncoeffs-nroc+jroc])
      cat("\n2x2 for logistic fn cut-pt ",signif(cutpt,3)," (prob ",prob,")",sep="")
      if (prob==0.5) cat(" Table of maximum accuracy")
      cat("\n")
      if (twolev) { #it seems "fitted" gives [0,1] probability, not the linear function
        mytab = table(vecprob > prob, veccase, dnn=c("logistic",namedep))
        if (!is.null(label.ordering) && label.ordering[1] != colnames(mytab)[1]) {
          colnames(mytab) = label.ordering #; cat ("switching mytab columns\n")
          icsav = mytab[1,1]; mytab[1,1] = mytab[1,2]; mytab[1,2] = icsav
          icsav = mytab[2,1]; mytab[2,1] = mytab[2,2]; mytab[2,2] = icsav
        }
      }
      else mytab = table(vecprob <= prob, veccase, dnn=c("logistic",paste(namedep,gtnam)))
      #print(mytab); print(rownames(mytab))
      #print(nrow(mytab)); print(mytab[1]); print(mytab[3])
      row1name = paste("<=",signif(cutpt,2),sep="")
      row2name = paste(" >",signif(cutpt,2),sep="")
      cu_2x2out(mytab,row1name,row2name)
    }
    nsubnom = 0; vecpronom = rep(0,1); veccasnom = rep(0,1)
    for (i in 1:nsubj) {
      if (!is.na(vecprob[i]) && !is.na(veccase[i])) {
        nsubnom = nsubnom+1; vecpronom[nsubnom]=vecprob[i]; veccasnom[nsubnom]=veccase[i]
      }
    }
    PredObj = prediction(vecpronom, veccasnom)  #??, label.ordering = label.ordering)
    perfAUC = performance(PredObj, measure="auc"); aucstr = strtrim(perfAUC@"y.values",5)
    cat("Area Under the Curve (AUC), aka Concordance, Harrell's C, C-statistic:",
        "\nAUC = ", aucstr, "\n")
    if (twolev) {AUCtitle = paste(namedep, "\n AUC = ", aucstr)}
    else {
      AUCtitle = namedep; veclegs[jroc] = paste(gtnam,"  AUC=",aucstr)
    }
    # print(AUCtitle)
    perfPlot = performance(PredObj, "tpr", "fpr")
    ROCR::plot(perfPlot, add=add, main=AUCtitle, 
           xlab=xlabroc, ylab=ylabroc, col=color); add=TRUE
  }
  if (!twolev) graphics::legend("bottomright", veclegs, lty=1, col = veccolors, bty="n", inset=c(0,0.01*nroc))
  # ROCR::plot just plots, no returned object
  if (is.logical(emf)&&emf) emf = "culogist"
  if (is.character(emf) && nroc==1) {
    fname = paste(emf,".emf",sep="")
    #cat("\nfname:",fname,">")
    if (!requireNamespace("devEMF", quietly = TRUE))
      stop("Package 'devEMF' is required for EMF output but is not installed.")
    devEMF::emf(fname, emfPlus=F)
    oldw <- getOption("warn")
    options(warn = -1)
    perfPlot = performance(PredObj, "tpr", "fpr")
    graphics::par(font.lab=2)
    ROCR::plot(perfPlot, add=F, main=AUCtitle, xlab=xlab, ylab=ylab, col=color)
    options(warn = oldw)
    grDevices::dev.off()
  }
  cat("") #needed to avoid "RStudioGD" and 2 on next line
  #cu_plout(p,"culogist",emf,seq=ifelse(nroc>1,jroc,0))
}
