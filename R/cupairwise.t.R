#' pairwise.t.test to handle group(s) with just 1 observation
#' same arguments as pairwise.t.test, same code (Nov.20) with added:
#' \code{for (j in 1:length(s)) \{if (is.na(s[j])) s[j] <- 0\}}
#' @export
cupairwise.t = function (x, g, p.adjust.method = stats::p.adjust.methods, pool.sd = !paired, 
                paired = FALSE, alternative = c("two.sided", "less", 
                                                "greater"), ...) 
{
  cuf_apply_defaults(match.call(), environment())
  if (paired & pool.sd) 
    stop("pooling of SD is incompatible with paired tests")
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
  g <- factor(g)
  p.adjust.method <- match.arg(p.adjust.method)
  alternative <- match.arg(alternative)
  if (pool.sd) {
    METHOD <- "t tests with pooled SD"
    xbar <- tapply(x, g, mean, na.rm = TRUE)
    s <- tapply(x, g, stats::sd, na.rm = TRUE)
    n <- tapply(!is.na(x), g, sum)
    degf <- n - 1
    total.degf <- sum(degf)
    for (j in 1:length(s)) {if (is.na(s[j])) s[j] <- 0}  # assume n[j]=1
    pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
    compare.levels <- function(i, j) {
      dif <- xbar[i] - xbar[j]
      se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
      t.val <- dif/se.dif
      if (alternative == "two.sided") 
        2 * stats::pt(-abs(t.val), total.degf)
      else stats::pt(t.val, total.degf, lower.tail = (alternative == 
                                                 "less"))
    }
  }
  else {
    METHOD <- if (paired) 
      "paired t tests"
    else "t tests with non-pooled SD"
    compare.levels <- function(i, j) {
      xi <- x[as.integer(g) == i]
      xj <- x[as.integer(g) == j]
      stats::t.test(xi, xj, paired = paired, alternative = alternative, 
             ...)$p.value
    }
  }
  # cat("\nxbar,s,n,degf,pooled.sd\n")
  # print(xbar);print(s);print(n);print(degf);print(pooled.sd)
  # print(compare.levels); cat("\ng\n"); print(levels(g))
  PVAL <- stats::pairwise.table(compare.levels, levels(g), p.adjust.method)
  ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
              p.adjust.method = p.adjust.method)
  class(ans) <- "pairwise.htest"
  ans
}
