#' Does summary stats
#' @param object to be summarized
#' @param ... additional arguments passed to methods
#' @return returns nothing
#' @export
cusum = function (object, ...)
{
  UseMethod("cusum")
}
