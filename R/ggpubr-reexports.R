#' @importFrom ggpubr mean_sd mean_ci median_q1q3 mean_se_
#' @importFrom ggplot2 mean_se
#' @keywords internal
#' @name ggpubr-reexports
#'
#' Re-exported ggpubr/ggplot2 summary functions.
#'
#' These functions are re-exported so that \code{ggbarplot(add = "mean_sd")}
#' can resolve them via \code{get()} when ggpubr is imported but not attached.
#'
#' @export mean_sd
#' @export mean_se
#' @export mean_ci
#' @export median_q1q3
#' @export mean_se_
NULL
