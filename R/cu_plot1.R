#' internal cufunction to plot data by one factor
#' @importFrom rlang .data
#' @keywords internal
cu_plot1 = function(progname, depvar,group1, g1order, g1name, depname, title,
                    plot="bar", linetype="n", linecolor="black", linesize=1,
                    letbar=NULL, ebars=4, dots=1, barcolor="black", barfill="colors",
                    pvdf=NULL,pvypos=NULL,pvstinc=NULL,pvlab="p",
                    pvprefix="p=", pvsize=3.5, pvnshide=T,pvtipl=0.005,
                    caption=NULL, legend="top", size=NULL, width=NULL, posd=NULL,
                    xangle=NULL, yangle=NULL, orientation="vertical",
                    dotshape=NULL, dotsize=NULL, dotcolor=NULL, binwfac=30, vscale="area",
                    theme="bw", yscale="none", fontfamily="sans",
                    yrange=NULL, ymin=NA, ymax=NA,
                    fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                    fontxticks=NULL, fontyticks=NULL,
                    axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                    ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                    titlejust="center", legheadsize=12, legtextsize = 10,
                    ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                    fheight=NULL, dpi=300, remove=NULL, suff=NULL) {
  #print(letbar) #cat("\nenter plot1")
  df = stats::na.omit(data.frame(depvar, group1)); depvar = df[,1]; group1 = df[,2]
  #print(colnames(df)); print(df)

    # colnames(df) = c(g1name,depname); print(df)
  #print(depname)
  binwidth = 1 # ifelse((rang=max(depvar)-min(depvar))>0,rang/binwfac,1)
#  setting binwidth avoids the warning msg about default being 1/30
#  cat("rang,binwfac,binwidth ",rang,binwfac,binwidth,"\n")
  if (plot=="box") title=paste(title,"\n Box Plots")
  else {
    if (ebars==1) {addon="mean_sd"; title=paste(title,"\n Mean +/- S.D.")}
    if (ebars==2) {addon="mean_se"; title=paste(title,"\n Mean +/- S.E.")}
    if (ebars==3) {addon="mean_ci"; title=paste(title,"\n Mean and 95% CL")}
    if (ebars==4) {addon="median_q1q3"; title=paste(title,"\n Median and IQR")}
  }
  #if (is.null(dotsize)) dotsize = ifelse(is.null(yrange),1,0.03*yrange)
  if (is.null(dotsize)) dotsize = ifelse(plot=="rod",0.35,0.7) #yrange no good
  if (is.null(dotshape)) dotshape = c(4,5,3,6,8,2,0,1,4,5,3,6,8,2,0,1)
  #if (is.null(dotshape)) dotshape = 5
  #https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
  # ifdotcolor = FALSE # not used so far
  if (is.null(barfill) || barfill=="colors") {barcolor = "group1"; barfill = "group1"} #NG
  else if (barfill=="white") barcolor="black"  # ; ifdotcolor = TRUE}
  else barcolor = barfill
  if (is.null(fontmain)) fontmain=c(14,"bold","black")
  if (is.null(fontxname)) fontxname=c(12,"bold","black")
  if (is.null(fontyname)) fontyname=c(12,"bold","black") 
  if (is.null(fontxticks)) fontxticks=c(12,"bold","black")
  if (is.null(fontyticks)) fontyticks=c(12,"bold","black")
  if (is.null(axiscolor)) axiscolor="black"
  if (is.null(tickcolor)) tickcolor="black"
  if (is.null(axisthick)) axisthick=0.5
  if (is.null(tickthick)) tickthick=0.5
  if (is.null(ticklength)) ticklength=1
  if (plot=="bar") {
    if (is.null(posd)) posd=0.9
    if (dots==0) 
      p = ggbarplot(df,x="group1",y="depvar",add = addon,
                    color=barcolor, fill=barfill, label=letbar, order=g1order,
                    xlab=g1name, ylab=depname, legend=legend, na.rm=F,
                    size=size, width=width)
    else if(dots==1) { 
        #https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
        #https://blog.albertkuo.me/post/point-shape-options-in-ggplot/
        #http://www.sthda.com/english/wiki/ggplot2-point-shapes
        #0-14 open:square,circle,triangle,+ x,diamond,inv.tri,x in sq,*
        #+ in diam,+ in o,2 tria,+ in sq,x in o,tria in sq
        #15-19 closed: squ,o,tria,diam,O
        #20-25 blue closed: o,O,squ,diam,tria,inv.tria
      #addon=c(addon, "jitter"); 
      p = ggbarplot(df,x="group1",y="depvar",add = addon,
                    color=barcolor, fill=barfill, label=letbar, order=g1order,
                    xlab=g1name, ylab=depname, legend=legend, na.rm=F,
                    size=size, width=width) +
        geom_jitter(aes(x=.data$group1,y=.data$depvar, shape=.data$group1),size=dotsize,
                    color = "black", stroke=1.5, width=0.1,height=0) +
        scale_shape_manual(values = dotshape)
      #if (barfill=="white")
      #  p <- p + geom_jitter(aes(shape=group1,col=group1), size=2,height=0,width=0.05)
      #else p <- p + geom_jitter(color="grey", size=2,height=0,width=0.05)
    }
    else { # dots > 1 bin points, separate dotplot leads to 2nd legend
      addon=c(addon, "dotplot",binwidth=binwidth,dotsize=dotsize)
      #cat ("\nbinwidth",binwidth)
      p = ggbarplot(df,x="group1",y="depvar",add = addon,
                    add.parms=list(binwidth=binwidth,dotsize=dotsize),
                    color=barcolor, fill=barfill, label=letbar, order=g1order,
                    xlab=g1name, ylab=depname, legend=legend, na.rm=F,
                    size=size, width=width)
    }
    #cat("\nbefore ggbar")
    #add.parms must say dotsize but fill not dotfill; can't change shape
            # no good geom_point(shape = dotshape, color=dotcolor, size=dotsize)
       #, position = position_dodge(width=posd) # this makes CLD go to bottom of -ve bars
    #print(dotshape)
    # seems not to work with dotplot
    #if (!is.null(dotshape)) p <- p + scale_shape_manual(values = c(18,18,18))
    #if (ifdotcolor) p <- p + scale_fill_manual(values = c("red","green","blue"))
    #cat("after ggbar,pvypos,pvlab,pvnshide,pvtipl,pvdf",
    #    pvypos,pvlab,pvnshide,pvtipl,"\n"); print(pvdf)
    if (!is.null(pvdf)) { # box Error: Continuous value supplied to discrete scale
      pvlbl = ifelse(is.null(pvprefix)||pvlab=="p.signif",
                     pvlab, paste(pvprefix,"{",pvlab,"}",sep=""))
      #cat("\npvlbl:",pvlbl)
      if (length(pvypos)>1) pvstinc = 0
      #print(pvdf); print(pvypos); print(pvlbl)
      #cat("\npvlab:",pvlab,"'\n",sep="")
      if (is.null(pvsize)&&(pvlab!="p")) pvsize = 7
      p <- p + 
        stat_pvalue_manual(pvdf, y.position=pvypos, step.increase=pvstinc,
        size=pvsize, label = pvlbl, hide.ns=F, tip.length=pvtipl,
        bracket.shorten=0.01)
      #cat("\ndid stat_pvalue_manual")
    }
  }
  else { # "box" "violin" "rod" #ggboxplot to use stat_pvalue_manual
    if (is.null(posd)) posd = 
        ifelse(plot=="box",0.75,ifelse(plot=="violin",0.9,0.9))
    if (barfill=="white") p <- ggplot(df,aes(x=.data$group1,y=.data$depvar)) #,yscale=yscale
    else p <- ggplot(df,aes(x=.data$group1,y=.data$depvar,fill=.data$group1))
      # had scale=vscale for geom_boxplot, can't tell why vscale="area"
    if (plot=="box") {p <- p + geom_boxplot(width=width); vjus = -1}
    else if (plot=="violin") {p <- p + geom_violin(trim=F); vjus = -5}
    else vjus = ifelse(dots>0,-1,1)  # "rod"
    p <- p + xlab(g1name)+ylab(depname) + labs(fill=g1name)
    if (!is.null(letbar)) p <- p + stat_summary(geom="text",label=letbar,
      fun=max,vjust=vjus,aes(group=.data$group1),hjust=0.25,position=position_dodge(posd))
    if (plot!="box" && ebars>0) {
      funname = switch(ebars, ggpubr::mean_sd, ggplot2::mean_se,
                       ggplot2::mean_cl_boot, ggpubr::median_q1q3)
      legTF = ifelse(plot=="violin",TRUE,FALSE)
      p <- p + stat_summary(fun.data = funname, show.legend = legTF, 
                            position=position_dodge(width=posd), geom = "pointrange")
    }
  #dotplot does only dots, so dotshape and dotcolor no use
    if (is.null(dotcolor)) dotcolor = ifelse(dots==2, "white", "grey")
    if (dots>0) if (dots==1) 
      p <- p +         
      geom_jitter(aes(x=.data$group1,y=.data$depvar, shape=.data$group1),
                  color = "black", stroke=1.5, width=0.1,height=0) +
      scale_shape_manual(values = dotshape)
    else if (barfill=="white")
      p <- p + geom_jitter(aes(shape=.data$group1,col=.data$group1), size=2,height=0,width=0.05)
      else p <- p + geom_jitter(color=dotcolor, size=2,height=0,width=0.05)
    # p <- p + scale_shape_manual(values=c(18,18,18))
  }
  # p <- p + scale_y_log10(); #scale_y_continuous(trans='log',breaks=c(100)) 
  # scale_y_continuous(trans='log10') same; coord_trans(y = "log10" no good
  if (linetype !="n") {
    funname = ifelse(ebars==4 || plot=="box","median","mean")
      # line is different from point: requires group=2 unlike aes in plot2
    p <- p + stat_summary(fun=funname, geom="line", group=2, 
                          position=position_dodge(width=posd), linetype=linetype, linewidth=linesize) +
      stat_summary(fun=funname, geom="point", position=position_dodge(width=posd))
  }
  pal = "lancet"
  if (is.null(theme)) theme = "bw"
  if (!(theme %in% c("grey","gray","bw","linedraw","light","dark","minimal","classic","void"))) {
    warning("theme='",theme,"' no good. Taken to be 'bw'", call. = FALSE); theme = "bw"
  }
  funtheme = get(paste("theme_",theme,sep=""))
  p <- p + funtheme() 
  # above must come before ggtitle, else jus is ineffective
  if (title!="") {
    if (is.null(titlejust)) jus = 0.5
    else if (is.numeric(titlejust)) jus = titlejust
    else if (titlejust=="left") jus = 0
    else if (titlejust=="right") jus = 1
    else jus = 0.5
    p <- p + ggtitle(title) + theme(plot.title = element_text(hjust=jus))
  }
  p <- p + 
    theme(axis.line = element_line(color=axiscolor,linewidth=axisthick),
          axis.ticks= element_line(color=tickcolor,linewidth=tickthick), # color ignored
          axis.ticks.length=unit(ticklength,"mm"),
          legend.title = element_text(size=legheadsize),
          legend.text = element_text(size=legtextsize))
  p <- ggpar(p, 
             palette=pal, font.family=fontfamily,
             font.main = fontmain, font.x = fontxname, font.y = fontyname,
             font.xtickslab = fontxticks, font.ytickslab = fontyticks,
             xticks.by=xticks.by, yticks.by=yticks.by,
             x.text.angle=xangle, y.text.angle=yangle, 
             caption=caption, orientation=orientation, yscale=yscale) +
    labs(fill = g1name) +  # colour = g1name, 
       geom_hline(yintercept=0)
  #cat("\nbefore plout")
  cu_plout(p,progname, suff=suff, ftype=ftype, fname=fname, scale=fscale,
           width=fwidth, height=fheight, dpi=dpi, remove=remove)
  #cat("\nafter plout")
  p # print(p)
}
