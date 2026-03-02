#' Default method for cusum
#' @param object Object to summarize.
#' @param ... Additional arguments.
#' @param digits Number of significant digits.
#' @return Summary statistics.
#' @export
cusum.default = function (object, ..., digits = max(3L, getOption("digits") - 3L))
{
  if (is.character(object)) object = as.factor(object)
  if (is.factor(object)) {
    sumfab = summary.factor(object, ...); ntot = sum(sumfab); lenb = length(sumfab)
    if (names(sumfab[lenb])=="NA's") {nmiss=sumfab[lenb]; lenb=lenb-1; ntot=ntot-nmiss}
    else nmiss=0
    sumfac = c("N"=ntot,"NA's"=nmiss,sumfab[1:lenb])
    for (i in 3:(lenb+2)) {
      sumfac[i] = paste(round(as.integer(sumfac[i])*100/max(1,ntot),digits=1),
                  "%: ",sumfac[i],sep="") #without space, can't extract n
    }
    return(sumfac)
  }
  else if (is.matrix(object))
    return(summary.matrix(object, digits = digits, ...))
    value <- if (is.logical(object))
    c(Mode = "logical", {
      tb <- table(object, exclude = NULL)
      if (!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n))) dimnames(tb)[[1L]][iN] <- "NA's"
      tb
    })
  else if (is.numeric(object)) {
    nas <- is.na(object); nmiss = sum(nas)
    object <- object[!nas]; nomiss=length(object)
    if (nomiss<2) {
      av = ifelse(nomiss==0,NA,object[1])
      stdev=NA; cv=NA; se=NA; qtl = c(NA,NA,NA,NA,NA); DoNorm=0
    }
    else {
      qtl <- stats::quantile(object)
      av=mean(object)
      stdev=sd(object)
      DoNorm=1
      if (nomiss<6 | nomiss>4000 | stdev==0) DoNorm=0
      cv = ifelse(av!=0 && qtl[2]*qtl[4]>0, 100*stdev/abs(av), NA)
      se=sd(object)/sqrt(nomiss)
      if (DoNorm) {
        sk=skewness(object, type=2)
        kt=kurtosis(object, type=2)
        skp=2*(1-pt(abs(sk/(sqrt(6/nomiss))), nomiss-1))
        ktp=2*(1-pt(abs(kt/(sqrt(24/nomiss))), nomiss-1))
        sh=shapiro.test(object)
        sh2=sh[[2]] 
      }
    }
    qq <- c(nomiss,nmiss,signif(av,3),signif(stdev,3),round(cv,1),signif(se,2),
            signif(qtl[1L:5L],3))  # 11 eleven elements
    nams = c("N","NA's","Mean","SD","CV%","SEM","Min.","1st Q","Median","3rd Q","Max.")
    if (DoNorm) {
      qq <- c(qq,round(sk,2),round(kt,2)) # plus 2 for skew, kurtosis
      nams <- c(nams,"Skew","Kurt.")
      if (skp<.1 | ktp<.1 | sh2<.1) { # plus 3 for p-values
        qq <- c(qq,ifelse (skp < 0.001, "<.001", round(skp,3)),
                ifelse (ktp < 0.001, "<.001", round(ktp,3)),
                ifelse (sh2 < 0.001, "<.001", round(sh2,3)))
        nams <- c(nams,"Skew p","Kurt p","norm ?")
      }
    }
    qq <- format(qq, digits=digits, drop0trailing=TRUE)
    names(qq) <- nams
    qq
  } # what follows is from the generic function, no idea what it does
  else if (is.recursive(object) && !is.language(object) &&
           (n <- length(object))) {
    sumry <- array("", c(n, 3L), list(names(object), c("Length",
                                                       "Class", "Mode")))
    ll <- numeric(n)
    for (i in 1L:n) {
      ii <- object[[i]]
      ll[i] <- length(ii)
      cls <- oldClass(ii)
      sumry[i, 2L] <- if (length(cls))
        cls[1L]
      else "-none-"
      sumry[i, 3L] <- mode(ii)
    }
    sumry[, 1L] <- format(as.integer(ll))
    sumry
  }
  else c(Length = length(object), Class = class(object), Mode = mode(object))
  class(value) <- c("summaryDefault", "table")
  value
}
