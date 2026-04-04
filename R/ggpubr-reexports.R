# Re-exported ggpubr/ggplot2 summary functions.
# These functions are re-exported so that ggbarplot(add = "mean_sd")
# can resolve them via get() when ggpubr is imported but not attached.

#' @importFrom ggpubr mean_sd
#' @export
ggpubr::mean_sd

#' @importFrom ggplot2 mean_se
#' @export
ggplot2::mean_se

#' @importFrom ggpubr mean_ci
#' @export
ggpubr::mean_ci

#' @importFrom ggpubr median_q1q3
#' @export
ggpubr::median_q1q3

#' @importFrom ggpubr mean_se_
#' @export
ggpubr::mean_se_
