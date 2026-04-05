#' internal cufunction to print estimable results, return p-value
#' @keywords internal
cu_estout = function(fit, vec, conf.int=0.95,minimal=F) {
  # https://rdrr.io/cran/gmodels/src/R/estimable.R
  # included here to avoid loading gmodels -> gdata that needs Perl
  # Contrasts and estimable linear functions of model coefficients
  #
  # Compute and test contrasts and other estimable linear functions of model
  # coefficients for for lm, glm, lme, mer, and geese objects
  #
  # `estimable` computes an estimate, test statitic, significance test, and
  # (optional) confidence interval for each linear functions of the model
  # coefficients specified by `cm`.
  #
  # @aliases estimable estimable.default estimable.mlm
  # @param obj Regression (lm, glm, lme, mer, mlm) object.
  # @param cm Vector, List, or Matrix specifying estimable linear functions or
  # contrasts.  See below for details.
  # @param beta0 Vector of null hypothesis values
  # @param conf.int Confidence level.  If provided, confidence intervals will be
  # computed.
  # https://rdrr.io/cran/gmodels/src/R/to.est.R
  
  # Return a vector for cm in estimable()
  # @param obj    estimable object
  # @param params character vector of names or logical vector with one element per model parameter selecting desrired parameter(s).
  # @author Randy Johnson, Laboratory of Genomic Diversity at NCI-Frederick
  
  .to.est <- function(obj, params)
  {
    ## if('lme' %in% class(obj) | 'mer' %in% class(obj))
    ##   {
    ##     eff.obj <- fixef(obj)
    ##   }
    ## else
    if('geese' %in% class(obj))
    {
      eff.obj <- obj$beta
    }
    else
    {
      eff.obj <- coef(obj)
    }
    
    if(is.null(obj))
      stop("Error obtaining model coefficients")
    
    est <- rep(0, length(eff.obj))
    names(est) <- names(eff.obj)
    
    if(!missing(params))
    {
      if(is.null(names(params)))
        if(length(params)==length(est))
          names(params) <- names(est)
      else
        stop("'param' has no names and does not match number of coefficients of model. Unable to construct coefficient vector")
      else
      {
        matches <- names(params) %in% names(est)
        if(!(all(matches)))
          stop(
            '\n\t',
            'Invalid parameter name(s): ',
            paste(names(params)[!matches], collapse=', '),
            '\n\t',
            'Valid names are: ',
            paste(names(est), collapse=', ')
          )
      }
      
      if(is.list(params))
        est[names(params)] <- unlist(params)
      else
        est[names(params)] <- params
    }
    
    return(est)
  }
  
  cu_estimable <- function (obj, cm, beta0, conf.int=NULL, joint.test=F,  show.beta0, ...)
  {
    {
      if (is.matrix(cm) || is.data.frame(cm))
      {
        cm <- t(apply(cm, 1, .to.est, obj=obj))
      }
      else if(is.list(cm))
      {
        cm <- matrix(.to.est(obj, cm), nrow=1)
      }
      else if(is.vector(cm))
      {
        cm <- matrix(.to.est(obj, cm), nrow=1)
      }
      else
      {
        stop("`cm' argument must be of type vector, list, or matrix.")
      }
      
      if(missing(show.beta0))
      {
        if(!missing(beta0))
          show.beta0=TRUE
        else
          show.beta0=FALSE
      }
      
      
      if (missing(beta0))
      {
        beta0 = rep(0, ifelse(is.null(nrow(cm)), 1, nrow(cm)))
        
      }
      
      
      if (joint.test==TRUE)
      {
        .wald(obj, cm, beta0)
      }
      else
      {
        if ("lme" %in% class(obj)) {
          stat.name <- "t.stat"
          cf <- summary(obj)$tTable
          rho <- summary(obj)$cor
          vcv <- rho * outer(cf[, 2], cf[, 2])
          tmp <- cm
          tmp[tmp==0] <- NA
          df.all <- t(abs(t(tmp) * obj$fixDF$X))
          df <- apply(df.all, 1, min, na.rm=TRUE)
          problem <- apply(df.all !=df, 1, any, na.rm=TRUE)
          if (any(problem))
            warning(paste("Degrees of freedom vary among parameters used to ",
                          "construct linear contrast(s): ",
                          paste((1:nrow(tmp))[problem], collapse=", "),
                          ". Using the smallest df among the set of parameters.",
                          sep=""))
        }
        else if ("lm" %in% class(obj))
        {
          stat.name <- "t.stat"
          cf <- stats::summary.lm(obj)$coefficients
          vcv <- stats::summary.lm(obj)$cov.unscaled * stats::summary.lm(obj)$sigma^2
          df <- obj$df.residual
          if ("glm" %in% class(obj))
          {
            vcv <- summary(obj)$cov.scaled
            if(stats::family(obj)[1] %in% c("poisson", "binomial"))
            {
              stat.name <- "X2.stat"
              df <- 1
            }
            else
            {
              stat.name <- "t.stat"
              df <- obj$df.residual
            }
          }
        }
        else if ("geese" %in% class(obj))
        {
          stat.name <- "X2.stat"
          cf <- summary(obj)$mean
          vcv <- obj$vbeta
          df <- 1
        }
        else if ("gee" %in% class(obj))
        {
          stat.name <- "X2.stat"
          cf <- summary(obj)$coef
          vcv <- obj$robust.variance
          df <- 1
        }
        else
        {
          stop("obj must be of class 'lm', 'glm', 'aov', 'lme', 'gee', 'geese' or 'nlme'")
        }
        if (is.null(cm))
          cm <- diag(dim(cf)[1])
        if (!dim(cm)[2]==dim(cf)[1])
          stop(paste("\n Dimension of ", deparse(substitute(cm)),
                     ": ", paste(dim(cm), collapse="x"),
                     ", not compatible with no of parameters in ",
                     deparse(substitute(obj)), ": ", dim(cf)[1], sep=""))
        ct <- cm %*% cf[, 1]
        ct.diff <- cm %*% cf[, 1] - beta0
        
        vc <- sqrt(diag(cm %*% vcv %*% t(cm)))
        if (is.null(rownames(cm)))
          rn <- paste("(", apply(cm, 1, paste, collapse=" "),
                      ")", sep="")
        else rn <- rownames(cm)
        switch(stat.name,
               t.stat={
                 prob <- 2 * (1 - stats::pt(abs(ct.diff/vc), df))
               },
               X2.stat={
                 prob <- 1 - stats::pchisq((ct.diff/vc)^2, df=1)
               })
        
        if (stat.name=="X2.stat")
        {
          retval <- cbind(hyp=beta0, est=ct, stderr=vc,
                          "X^2 value"=(ct.diff/vc)^2,
                          df=df, prob=1 - stats::pchisq((ct.diff/vc)^2, df=1))
          dimnames(retval) <- list(rn, c("beta0", "Estimate", "Std. Error",
                                         "X^2 value", "DF", "Pr(>|X^2|)"))
        }
        else if (stat.name=="t.stat")
        {
          retval <- cbind(hyp=beta0, est=ct, stderr=vc, "t value"=ct.diff/vc,
                          df=df, prob=2 * (1 - stats::pt(abs(ct.diff/vc), df)))
          dimnames(retval) <- list(rn, c("beta0", "Estimate", "Std. Error",
                                         "t value", "DF", "Pr(>|t|)"))
        }
        
        if (!is.null(conf.int))
        {
          if (conf.int <=0 || conf.int >=1)
            stop("conf.int should be between 0 and 1. Usual values are 0.95, 0.90")
          alpha <- 1 - conf.int
          switch(stat.name,
                 t.stat={
                   quant <- stats::qt(1 - alpha/2, df)
                 },
                 X2.stat={
                   quant <- stats::qt(1 - alpha/2, 100)
                 })
          nm <- c(colnames(retval), "Lower.CI", "Upper.CI")
          retval <- cbind(retval, lower=ct.diff - vc * quant, upper=ct.diff +
                            vc * quant)
          colnames(retval) <- nm
        }
        rownames(retval) <- make.unique(rownames(retval))
        retval <- as.data.frame(retval)
        if(!show.beta0) retval$beta0 <- NULL
        
        class(retval) <- c("estimable", class(retval))
        
        return(retval)
      }
    }
    
    # @importFrom stats coef
    .wald <- function (obj, cm,
                       beta0=rep(0, ifelse(is.null(nrow(cm)), 1, nrow(cm))))
    {
      if (!is.matrix(cm) && !is.data.frame(cm))
        cm <- matrix(cm, nrow=1)
      df <- nrow(cm)
      if ("geese" %in% class(obj))
      {
        cf <- obj$beta
        vcv <- obj$vbeta
      }
      else if ("gee" %in% class(obj))
      {
        cf <- summary(obj)$coef
        vcv <- obj$robust.variance
      }
      else if ("lm" %in% class(obj))
      {
        cf <- stats::summary.lm(obj)$coefficients[, 1]
        if ("glm" %in% class(obj))
          vcv <- summary(obj)$cov.scaled
        else
          vcv <- stats::summary.lm(obj)$cov.unscaled * stats::summary.lm(obj)$sigma^2
      }
      else if ("lme" %in% class(obj))
      {
        s.o <- summary(obj)
        cf <- s.o$tTable[,1]
        se <- s.o$tTable[, 2]
        rho <- s.o$cor
        vcv <- rho * outer(se, se)
      }
      else
        stop("obj must be of class 'lm', 'glm', 'aov', 'gee', 'geese', or 'lme'.")
      u <- (cm %*% cf)-beta0
      vcv.u <- cm %*% vcv %*% t(cm)
      W <- t(u) %*% solve(vcv.u) %*% u
      prob <- 1 - stats::pchisq(W, df=df)
      retval <- as.data.frame(cbind(W, df, prob))
      names(retval) <- c("X2.stat", "DF", "Pr(>|X^2|)")
      print(as.data.frame(retval))
    }
  }
  for (i in (2:length(vec))) { 
    if (is.na(fit$coefficients[i])) {
      if (!minimal) cat(":coeff ",i," (",names(fit$coefficients)[i],
          ") unavailable (missing combination?)\n",sep="")
      return(1.0)
    }
  }
  est = suppressWarnings(cu_estimable(fit, vec, conf.int=conf.int))
  if (!minimal) {
    cat(": ",signif(est[,1],3)," \u00B1 ",signif(est[,2],3),", p=",signif(est[,5],3),
        ", CL=[",signif(est[,6],3),",",signif(est[,7],3),"]",sep="")
    # cat(" (",vec,")",sep=" ")
    cat("\n")
  }
  return (est[,5])
}
