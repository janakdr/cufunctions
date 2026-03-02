#' partial F test to test if model augmentation is significant
#' 
#' @param rse1 residual standard error of reduced (simpler) model (required)
#' @param ndf1 degrees of freedom of reduced (simpler) model (required)
#' @param rse2 residual standard error of full (bigger) model (required)
#' @param ndf2 degrees of freedom of full (bigger) model (required)
#' @return returns p-value of partial F(np,ndf2) = ((rse1^2*ndf1-rse2^2*ndf2)/np)/rse2^2
#' @examples
#' \dontrun{
#' cupartialF(12.39,33,7.56,30)
#' }
#' @export
cupartialF = function (rse1, ndf1, rse2, ndf2) 
{
  np = ndf1-ndf2
  # if (np<=0) stop ("simpler model must have more degrees of freedom")
  # if (rse2==0) stop ("bigger model cannot have zero error")
  # fstat = ((rse1^2*ndf1-rse2^2*ndf2)/np)/rse2^2
  # cat("\n",fstat,np,ndf2)
  pval = ifelse(np<=0, 1.0, ifelse(rse2==0, 0.0,
       pf(((rse1^2*ndf1-rse2^2*ndf2)/np)/rse2^2, np, ndf2, lower.tail=F)))
  pval
}