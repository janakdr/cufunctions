#' Data frame method for cusum
#' @param object Data frame to summarize.
#' @param maxsum Maximum number of levels to show for factors.
#' @param digits Number of significant digits.
#' @param ... Additional arguments.
#' @return Summary table.
#' @export
cusum.data.frame = function (object, maxsum = 7L, digits = max(3L, getOption("digits") -
                                                                 3L), ...)
{
  ncw <- function(x) {
    z <- nchar(x, type = "w")
    if (any(na <- is.na(z))) {
      z[na] <- nchar(encodeString(z[na]), "b")
    }
    z
  }
  z <- lapply(X = as.list(object), FUN = cusum.default, maxsum = maxsum,
              digits = 12L, ...)
  nv <- length(object)
  nm <- names(object)
  lw <- numeric(nv)
  nr <- if (nv)
    max(unlist(lapply(z, NROW)))
  else 0
  #cat(nv,nm,lw,nr)
  for (i in seq_len(nv)) {
    sms <- z[[i]]
    if (is.matrix(sms)) {
      cn <- paste(nm[i], gsub("^ +", "", colnames(sms),
                              useBytes = TRUE), sep = ".")
      tmp <- format(sms)
      if (nrow(sms) < nr)
        tmp <- rbind(tmp, matrix("", nr - nrow(sms),
                                 ncol(sms)))
      sms <- apply(tmp, 1L, function(x) paste(x, collapse = "  "))
      wid <- sapply(tmp[1L, ], nchar, type = "w")
      blanks <- paste(character(max(wid)), collapse = " ")
      wcn <- ncw(cn)
      pad0 <- floor((wid - wcn)/2)
      pad1 <- wid - wcn - pad0
      cn <- paste0(substring(blanks, 1L, pad0), cn, substring(blanks,
                                                              1L, pad1))
      nm[i] <- paste(cn, collapse = "  ")
      z[[i]] <- sms
    }
    else {
      sms <- format(sms, digits=digits, drop0trailing=TRUE)
      lbs <- format(names(sms))
      if (is.integer(z[[i]])) sms <- paste(lbs, ":", sms)
      #if (!is.integer(z[[i]])) sms <- paste(lbs,"  ")
      #cat(class(z[[i]]),"\n")
      lw[i] <- ncw(lbs[1L])
      length(sms) <- nr
      z[[i]] <- sms
    }
  }
  if (nv) {
    z <- unlist(z, use.names = TRUE)
    dim(z) <- c(nr, nv)
    if (any(is.na(lw)))
      warning("probably wrong encoding in names(.) of column ",
              paste(which(is.na(lw)), collapse = ", "))
    blanks <- paste(character(max(lw, na.rm = TRUE) + 2L),
                    collapse = " ")
    pad <- floor(lw - ncw(nm)/2)
    #nm <- paste0(substring(blanks, 1, pad), nm)
    dimnames(z) <- list(rep.int("", nr), nm)
  }
  else {
    z <- character()
    dim(z) <- c(nr, nv)
  }
  attr(z, "class") <- c("table")
  msage="Skewness, Kurtosis & Normality testing not done if SD=0 or n<6 or n>4000.\n"
  cat(msage)
  msage="Normality test is by Shapiro-Wilk\n"
  cat(msage)
  if (length(rownames(z))==15) rownames(z)=c("N","Mean","SD","CV%","SEM","Min.","1st Q","Median","3rd Q","Max.","Skew","Kurt.","Skew p","Kurt p","norm ?")
  if (length(rownames(z))==12) rownames(z)=c("N","Mean","SD","CV%","SEM","Min.","1st Q","Median","3rd Q","Max.","Skew","Kurt.")
  # <12 assumes all categorical
  #rownames(z)=c("N","Mean","SD","CV%","SEM","Min.","1st Q","Median","3rd Q","Max.","Skew","Kurt.","Skew p","Kurt p","norm ?")
  z
}
