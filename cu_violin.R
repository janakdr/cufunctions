cu_violin = function(depvar,groupvar, letbar=NULL, ebars=4, dots=1, fill="colors",
                     xlab=NULL, ylab=NULL, boxwidth=0.1, dotwidth=2) {
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
                 aes(group=groupvar),hjust=0.25,position=position_dodge(0.75))
  if (ebars>0) {
    funname = ifelse(ebars==4,"median_q1q3",ifelse(ebars==3,"mean_cl_boot",
                     ifelse(ebars==2,"mean_se","mean_sd")))
    p <- p + stat_summary(fun.data = funname, geom = "pointrange")
  }
  if (dots>0) p <- p + geom_dotplot(binaxis="y",stackdir="center",binwidth=dotwidth,fill="white")
  p # print(p)
}
