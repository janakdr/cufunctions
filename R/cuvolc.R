#' Volcano plot
#'
#' @param ds data frame with column "varnam" column "fold" and column "pval"
#' @param xdash =0.5 (default) to locate dashed vertical lines
#' @param psig =0.05 (default) for p-value threshold of interest
#' @param psigmin =0.2 (default) for upper limit of p-values to display
#' @param pfactor =1 (default) for factor to divide psig by for multiple comparison correction
#' @param foldmin =NULL (default) for left end of log2(FoldChange) (usually set to -2)
#' @param foldmax =NULL (default) for right end of log2(FoldChange) (usually set to 2)
#' @param ylim =5 (default) for upper limit of graph (-log10(pval))
#' @examples
#' cuvolc(volc)
#' @export
cuvolc = function(ds, xdash=0.5, psig = 0.05, psigmin = 0.2, pfactor=1, 
                  foldmin=NULL, foldmax=NULL, ylim=5, title="Volcano Plot")
{
  cuf_apply_defaults(match.call(), environment())
  # https://biocorecrg.github.io/CRG_RIntroduction/volcano-plots.html
  # https://github.com/erikaduan/r_tips/blob/master/tutorials/dv-volcano_plots_with_ggplot/dv-volcano_plots_with_ggplot.md#import-a-test-dataset
  ds$log2fc = log2(ds$fold); psigadj = psig/pfactor
  ds$Colors = "ns"
  if (is.null(foldmin)) {
    foldmin = 1e40
    for (i in 1:nrow(ds)) foldmin = min(foldmin,ds$log2fc[i])
  }
  if (is.null(foldmax)) {
    foldmax = -1e40
    for (i in 1:nrow(ds)) foldmax = max(foldmax,ds$log2fc[i])
  }
  for (i in 1:nrow(ds)) {
    ds$log2fc[i] = min(max(ds$log2fc[i],foldmin),foldmax)
    ds$Colors[i] = ifelse(ds$pval[i] > psigadj,"ns",
                 ifelse(ds$log2fc[i]>0, "up", "down"))
    if (ds$pval[i] > psigmin/pfactor) ds$pval[i]= NA
  }
  #str(ds)
  cols <- c("up" = "red", "down" = "blue", "ns" = "grey") 
  sizes <- c("up" = 2, "down" = 2, "ns" = 0.5) 
  alphas <- c("up" = 1, "down" = 1, "ns" = 1)
  volc <- ggplot(data = ds, aes(x = log2fc, y = -log10(pval), col = Colors, 
                   #alpha = Colors, label = varnam, show.legend=F,
     fill = Colors, size = Colors)) +
    geom_vline(xintercept = c(-xdash, xdash), col = "gray", linetype = 'dashed') +
    geom_hline(yintercept = -log10(psig), col = "gray", linetype = 'dashed') +
    geom_point(shape=21) +
    scale_color_manual(values = c("blue", "black", "red"))+#, # to set the colours of our variable
                       #labels = c("Downregulated", "Not significant", "Upregulated")) + # to set the labels in case we want to overwrite the categories from the dataframe (UP, DOWN, NO)
    coord_cartesian(ylim = c(0, ylim), xlim = c(foldmin,foldmax)) + 
    labs(#color = 'Colors', #legend_title,
         x = expression("log"[2]*"FoldChange"), y = expression("-log"[10]*"p-value")) +
    scale_fill_manual(values = cols) + # Modify point colour
    scale_size_manual(values = sizes) + # Modify point size
    scale_alpha_manual(values = alphas) + # Modify point transparency
    scale_x_continuous(breaks = c(seq(-5, 5, 1)), limits = c(-10, 10)) +  
    ggtitle(title) + theme(plot.title = element_text(hjust=0.5)) +
    geom_text(label=ds$varnam, nudge_x=0.17, nudge_y=0.1, 
              check_overlap=T, show.legend=F) +
    guides(color="none")
  oldw <- getOption("warn")
  options(warn = -1)
  volc
  options(warn = oldw)
  return(volc)
}