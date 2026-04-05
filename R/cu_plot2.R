#' internal cufunction to plot data by two factors
#' @importFrom rlang .data
#' @keywords internal
cu_plot2 = function(progname,depvar,group1,group2,nlev1,nlev2, g1order, g1name, depname, g2name, title,
                    plot="bar", linetype="n", linecolor="black", linesize=1,
                    letbar=NULL, ebars=4, dots=1, barcolor="black", barfill="colors",
                    pvdf=NULL,pvypos=NULL,pvstinc=NULL,pvlab="p",
                    pvprefix="p=", pvsize=NULL, pvnshide=T,pvtipl=NULL,
                    caption=NULL, legend="top", size=NULL, width=NULL, posd=NULL,
                    xangle=NULL, yangle=NULL, orientation="vertical",
                    binwfac=30, dotsize=NULL, dotshape=NULL, dotcolor=NULL, vscale="area",
                    theme="bw", yscale="none", fontfamily="sans",
                    yrange=NULL, ymin=NA, ymax=NA,
                    fontmain=NULL, fontxname=NULL, fontyname=NULL, 
                    fontxticks=NULL, fontyticks=NULL,
                    axiscolor=NULL, tickcolor=NULL, axisthick=NULL, tickthick=NULL,
                    ticklength=NULL, xticks.by=NULL, yticks.by=NULL, 
                    titlejust="center", legheadsize=12, legtextsize = 10,
                    ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                    fheight=NULL, dpi=300, remove=NULL, suff=NULL) {
  barntoxm = function(barno) {
    ig = (barno-1)%/%nlev2; jg = (barno-1)%%nlev2 #; cat("\nig,jg:",ig,jg)
    return((barno-1)%/%nlev2 + 0.55 + ((barno-1)%%nlev2 +0.5) * 0.9/nlev2)
  }
  df = stats::na.omit(data.frame(depvar, group1, group2))
  depvar = df[,1]; group1 = df[,2]; group2 = df[,3]
  # print(colnames(df)); print(df)
  # colnames(df) = c(g1name,depname); print(df)
  # try to use dsnomiss
  # print(df)
  binwidth = ifelse((rang=max(depvar)-min(depvar))>0,rang/binwfac,1)
#  setting binwidth avoids the warning msg about default being 1/30
#  cat("binwidth ",binwidth,"\n")
#  if (is.null(dotsize)) dotsize = ifelse(plot=="rod",0.35,0.7)
  if (is.null(pvtipl)) pvtipl = 0.005
  if (plot=="box") title=paste(title,"\n Box Plots")
  else {
    if (ebars==1) {addon="mean_sd"; title=paste(title,"\n Mean +/- S.D.")}
    if (ebars==2) {addon="mean_se"; title=paste(title,"\n Mean +/- S.E.")}
    if (ebars==3) {addon="mean_ci"; title=paste(title,"\n Mean and 95% CL")}
    if (ebars==4) {addon="median_q1q3"; title=paste(title,"\n Median and IQR")}
  }
  #letbar = c("a","b","c","d","e","f"); print(letbar)
  if (is.null(dotsize)) dotsize = ifelse(is.null(yrange),0.8,0.3+0.005*yrange)
  if (is.null(dotshape)) dotshape = c(4,5,3,6,8,2,0,1,4,5,3,6,8,2,0,1)
  else if (length(dotshape) != nlev2) {
    message("dotshape no good: ",dotshape,". Need to specify ",nlev2)
    dotshape = c(4,5,3,6,8,2,0,1,4,5,3,6,8,2,0,1)
  }
  # dotshape doesn't do anything except with dots=1 (jitter)
  # if (is.null(dotshape)) dotshape = 5
  #https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
  # ifdotcolor = FALSE # not used so far
  if (is.null(barfill) || barfill=="colors") {barcolor = "group1"; barfill = "group1"} #NG 
  else if (barfill=="white") barcolor="black"  # ; ifdotcolor = TRUE}
  else barcolor = barfill
  #if (is.null(dotcolor)) dotcolor = "white"
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
  # https://stackoverflow.com/questions/62350554/r-geom-jitter-mixes-symbols-from-two-factor-categories
  # https://statdoe.com/barplot-for-two-factors-in-r/
  # addon specifies mean/median bar and SD/SE/CL/IQR
  # fill is 2nd factor to split by; color is to use different colors?
  # position_dodge specifies width of each bar
  # lab.hjust is to move letbar text left (>0) or right (<0)
  # alpha makes bars transparent (0 to 1)
  # unclear from ? ggbarplot:
  # size (e.g.: size = 1). change the size of points and outlines.
  # width between 0 and 1 specifying box width.
  # facet.by character vector, of length 1 or 2, specifying grouping variables for faceting the plot into multiple panels. Should be in the data.
  # panel.labs a list of one or two character vectors to modify facet panel labels. 
  # short.panel.labs logical value. Default is TRUE. If TRUE, create short labels for panels by omitting variable names
  if (plot=="bar") {
    if (is.null(posd)) posd=0.9
    if (dots==0) 
      p = ggbarplot(df,x="group1",y="depvar",add = addon,
                    add.params=list(binwidth=binwidth,dotsize=dotsize),
                    color=barcolor, fill="group2", label=letbar, order=g1order,
                    xlab=g1name, ylab=depname, legend=legend, na.rm=F,
                    size=size, width=width,
                    position = position_dodge(width=posd),
                    lab.hjust=0.4)  #lab.vjust=c(1,-0.4,1,1,1,1) need to deal with -ve bars
    else if(dots==1) { # separate dotplot leads to 2nd legend
      p = ggbarplot(df,x="group1",y="depvar",add = addon,
                    color=barcolor, fill="group2", label=letbar, order=g1order,
                    xlab=g1name, ylab=depname, legend=legend, na.rm=F,
                    size=size, width=width,
                    position = position_dodge(width=posd),
                    lab.hjust=0.4) + #lab.vjust=c(1,-0.4,1,1,1,1) need to deal with -ve bars
        geom_jitter(aes(x=.data$group1,y=.data$depvar,fill=.data$group2, shape=.data$group2),
                    size=dotsize, color = "black", stroke=1.5,
                    position = position_jitterdodge(dodge.width=posd,
                                                    jitter.width=0.4,jitter.height=0)) +
        scale_shape_manual(values = dotshape)
      #                    position = position_dodge(width=0.5),
      #                     width=0.01, shape=4, size=2, stroke=2)
      #p <- p + geom_point(position = position_jitterdodge(), alpha=0.3)
      #p <- p + geom_point(aes(color="group2"),position = "jitter",width=0.0, shape=4, size=2, stroke=2, alpha=0.3)
    }
    else { # dots > 1 bin points, separate dotplot leads to 2nd legend
      addon=c(addon, "dotplot",binwidth=binwidth,dotsize=dotsize,dotshape=dotshape)
      #cat ("\nbinwidth",binwidth)
      p = ggbarplot(df,x="group1",y="depvar",add = addon,
                    add.parms=list(binwidth=binwidth,dotsize=dotsize,dotshape=dotshape),
                    color=barcolor, fill="group2", label=letbar, order=g1order,
                    xlab=g1name, ylab=depname, legend=legend, na.rm=F,
                    size=size, width=width,
                    position = position_dodge(width=posd),
                    lab.hjust=0.4)  #lab.vjust=c(1,-0.4,1,1,1,1) need to deal with -ve bars
    }
    #cu_plout(p); p
    # cat("\nbefore ggbar")
    #add.parms must say dotsize but fill not dotfill; can't change shape
    # no good geom_point(shape = dotshape, color=dotcolor, size=dotsize)
    #, position = position_dodge(width=posd) # this makes CLD go to bottom of -ve bars
    #print(dotshape)
    # seems not to work with dotplot
    #if (!is.null(dotshape)) p <- p + scale_shape_manual(values = c(18,18,18))
    #if (ifdotcolor) p <- p + scale_fill_manual(values = c("red","green","blue"))
    #cat("after ggbar,pvypos,pvlab,pvnshide,pvtipl,pvdf",
    #    pvypos,pvlab,pvnshide,pvtipl,"\n"); print(pvdf)
    # from ? stat_pvalue_manual
    # first two columns with x-coords of L/R of brackets. Default "group1"/"group2"
    # bracket.nudge.y	Vertical adjustment to nudge brackets by
    if (!is.null(pvdf)) { # box Error: Continuous value supplied to discrete scale
      pvlbl = ifelse(is.null(pvprefix)||pvlab=="p.signif",
                     pvlab, paste(pvprefix,"{",pvlab,"}",sep=""))
      #cat("\npvlbl,pvlab:",pvlbl,"'",pvlab,"'")
      if (length(pvypos)>1) pvstinc = 0
      for (ilin in 1:nrow(pvdf)) {
        pvdf[ilin,1] = barntoxm(pvdf[ilin,1]); 
        pvdf[ilin,2] = barntoxm(pvdf[ilin,2]);
        #pvdf[ilin,3] = scales::pvalue(pvdf[ilin,3])
      }
      #cat("\npvsi,pvtipl,pvdf,pvypos:",pvstinc,pvtipl,"\n"); print(pvdf); print(pvypos)
      if (is.null(pvsize)&&(pvlab!="p")) pvsize = 7
      p <- p + 
        stat_pvalue_manual(pvdf, y.position=pvypos, step.increase=pvstinc,
          size=pvsize, label = pvlbl, hide.ns=F, tip.length=pvtipl,
          bracket.shorten=0.01)
                    #??? label = "p = {scales::pvalue(pvdf[,3])}"
      #cat("\ndid stat_pvalue_manual")
    }
    #cu_plout(p); p
  }
  else { # "box" "violin" "rod"
    if (is.null(posd)) posd = 
        ifelse(plot=="box",0.75,ifelse(plot=="violin",0.9,0.9))
    p <- ggplot(df,aes(x=.data$group1,y=.data$depvar,color=.data$group2,fill=.data$group2))
    if (plot=="box") {p <- p + geom_boxplot(width=width); vjus = -1}
    else if (plot=="violin") {p <- p + geom_violin(trim=F); vjus = -5}
    else vjus = ifelse(dots>0,-1,1)  # "rod"
    p <- p + xlab(g1name)+ylab(depname) + labs(fill=g2name)
    if (!is.null(letbar)) { # why this fix? bug in geom text?
      letbat = c(letbar[1]); j = 1; nlevboth = nlev1*nlev2
      for (i in 2:nlevboth) {
        j = j+nlev2; if (j>nlevboth) j = j-nlevboth+1
        letbat[i] = letbar[j]
      }
      #print(letbar); print(letbat)
      p <- p + stat_summary(geom="text",label=letbat,
      fun=max,vjust=vjus,aes(group=.data$group2),hjust=0.25,position=position_dodge(posd))
    }
    if (plot!="box" && ebars>0) {
      funname = switch(ebars, ggpubr::mean_sd, ggplot2::mean_se,
                       ggplot2::mean_cl_boot, ggpubr::median_q1q3)
      legTF = ifelse(plot=="violin",TRUE,FALSE)
      p <- p + stat_summary(fun.data = funname, show.legend = legTF, 
                position=position_dodge(width=posd), geom = "pointrange")
    }
    #dotplot does only dots, so dotshape and dotcolor no use
    if (is.null(dotcolor)) dotcolor = ifelse(dots==1, "white", "grey")
    if (dots>0) if (dots==1) 
      p <- p + 
      geom_jitter(aes(x=.data$group1,y=.data$depvar,fill=.data$group2, shape=.data$group2),
                  color = "black", stroke=1.5,
                  position = position_jitterdodge(dodge.width=posd,
                                                  jitter.width=0.4,jitter.height=0)) +
      scale_shape_manual(values = dotshape)
    else if (barfill=="white")
      p <- p + geom_jitter(aes(shape=.data$group1,col=.data$group1), size=2,height=0,width=0.05)
    else p <- p + geom_jitter(color=dotcolor, size=2,height=0,width=0.05)
    # p <- p + scale_shape_manual(values=c(18,18,18))
  }
  # cu_plout(p); p
  if (linetype !="n") {
    funname = ifelse(ebars==4 || plot=="box","median","mean")
    p <- p + stat_summary(fun=funname, geom="line", aes(group=.data$group2,color=.data$group2), 
          position=position_dodge(width=posd), linetype=linetype, linewidth=linesize) +
             stat_summary(fun=funname, geom="point", aes(group=.data$group2),
                position=position_dodge(width=posd))
  }
  pal=barfill
  if (pal == "colors") pal="lancet"
  if (is.null(theme)) theme = "bw"
  if (!(theme %in% c("grey","gray","bw","linedraw","light","dark","minimal","classic","void"))) {
    message("theme='",theme,"' no good. Taken to be 'bw'"); theme = "bw"
  }
  funtheme = get(paste("theme_",theme,sep=""))
  # cu_plout(p); p
  p <- p + funtheme() 
  # cu_plout(p); p
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
  p <- ggpar(p, palette=pal, font.family=fontfamily,
             font.main = fontmain, font.x = fontxname, font.y = fontyname,
             font.xtickslab = fontxticks, font.ytickslab = fontyticks,
             xticks.by=xticks.by, yticks.by=yticks.by,
             x.text.angle=xangle, y.text.angle=yangle, 
             caption=caption, orientation=orientation, yscale=yscale) + 
       labs(fill = g2name) +  # colour = g2name, 
       geom_hline(yintercept=0)
  cu_plout(p,progname, suff=suff, ftype=ftype, fname=fname, scale=fscale,
           width=fwidth, height=fheight, dpi=dpi, remove=remove)
  p # print(p)
}
