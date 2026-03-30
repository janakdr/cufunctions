#' Create a violin plot
#'
#' @param depvar Numeric dependent variable.
#' @param groupvar Factor grouping variable.
#' @param letbar Optional significance letters for bars.
#' @param ebars Error bar type: 1=SD, 2=SE, 3=CL, 4=IQR (default 4).
#' @param dots Show individual data points (default 1).
#' @param fill Fill color: "colors" for group colors, or a single color.
#' @param xlab X-axis label override.
#' @param ylab Y-axis label override.
#' @param boxwidth Width of the inner box (default 0.1).
#' @param dotwidth Dot bin width (default 2).
#' @return A ggplot object.
#' @importFrom rlang .data
#' @keywords internal
cu_violin = function(depvar,groupvar, letbar=NULL, ebars=4, dots=1, fill="colors",
                     xlab=NULL, ylab=NULL, boxwidth=0.1, dotwidth=2) {
  cuf_apply_defaults(match.call(), environment())
  df = data.frame(V1 = groupvar, V2=depvar) 
  if (is.null(xlab)) xlab = deparse(substitute(groupvar))
  if (is.null(ylab)) ylab = deparse(substitute(depvar))
  #print(xlab); print(ylab)
  # colnames(df) = c(xlab,ylab) #; print(df)
  if (fill=="colors") {color = "groupvar"; fill = groupvar}
  p <- ggplot(df, aes(df[,1], df[,2], fill=fill)) + geom_violin(trim=F) + 
    labs(x=xlab) + labs(y=ylab) + labs(fill=xlab)
  if (!is.null(letbar)) p <- p + 
    stat_summary(geom="text",label=letbar,fun=max,vjust=-1,
                 aes(group=.data$V1),hjust=0.25,position=position_dodge(0.75))
  if (ebars>0) {
    funname = switch(ebars, ggpubr::mean_sd, ggplot2::mean_se,
                     ggplot2::mean_cl_boot, ggpubr::median_q1q3)
    p <- p + stat_summary(fun.data = funname, geom = "pointrange")
  }
  if (dots>0) p <- p + geom_dotplot(binaxis="y",stackdir="center",binwidth=dotwidth,fill="white")
  p # print(p)
}
