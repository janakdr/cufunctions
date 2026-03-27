#' scatterplot of yvar vs xvar
#' @param yvar ,xvar required
#' @param doline =T (default) to show the regression line, F to not show
#' @param showr2eqn ="both" (default) to show R2 and eqn,"r2" for just R2, "eqn" for just eqn, "no" for neither
#' @param minimal =F (default)/T for minimal console output and no graph
#' @param yname ,xname,title ="xyz" to override names of yvar, xvar, title
#' @param caption ='' (default) or set to string to show at bottom right
#' @param shape =1 (default) (see http://www.sthda.com/english/wiki/ggplot2-point-shapes)
#' @param dotcolor ="black" (default) for point symbol color (="red" ="blue" etc)
#' @param dotsize =2 (default) or x to set size of point symbols
#' @param linetype ="solid" (default)/x for solid line ("solid" "dashed" "dotted" "blank" "longdash" "dotdash" "twodash")
#' @param linesize =1 (default)/x for line thickness
#' @param linecolor ="red" (default)/x for line color
#' @param theme ="bw" (default)/x for white background ("classic" (no grid lines),"linedraw" "gray" "minimal" "void")
#' @param xscale ,yscale ="none" (default), can be "log2", "log10", "sqrt"
#' @param fontfamily ="sans" (default), can be "serif" "mono" 
#' @param r2x =1 (default)/x for x-coordinate as fraction of axis of R2/eqn 
#' @param r2y =0.95 (default)/x for y-coordinate as fraction of axis of R2/eqn 
#' @param r2size =4 (default)/x for size of R2/eqn
#' @param r2color ="blue" (default)/x for R2/eqn color
#' @param r2font ="plain" (default)/x for R2/eqn font ("bold","italic","bold.italic")
#' @param xmin ,xmax,ymin,ymax =NA (default) or value to start/end x/y-axis 
#' @param fontmain =c(14,"bold","black") default, change for title, 0 for not title 
#' @param fontxname .fontyname,fontxticks,fontyticks = c(12,"plain","black") default, 0 to suppress
#' @param axiscolor ,tickcolor="black" (default)/x for axis/tick color
#' @param axisthick ,tickthick=0.5 (default)/x for axis/tick thickness
#' @param ticklength =1 (default)/x for tick length in mm
#' @param xticks.by ,yticks.by =NULL (default)/s for x/y tick spacing by s
#' @param xangle ,yangle for axis value angles: 0 (default) horizontal, 90 vertical, or any value between
#' @param orientation (default="vertical"), can be "horizontal" or "reverse"
#' @param titlejust ="center" (default) or "left" or "right"
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @return returns list with plot object, slope, intercept, pval, R^2
#' @examples
#' with(NEJM, cuscatter(tcstudy, tcpre))
#' with(NEJM, cuscatter(tcstudy, tcpre, showr2eqn="r2"))  # show just R-squared and p-value
#' # with(NEJM, cuscatter(tcstudy, tcpre, remove = c("x.ticks", "y.ticks")))  # requires ggpubr::rremove
#' @export
cuscatter = function(yvar,xvar, doline=T, showr2eqn="both", minimal=F,
                    yname=NULL, xname=NULL, title=NULL, caption="",
                    xangle=NULL, yangle=NULL, orientation="vertical",
                    shape=1, dotcolor="black", dotsize=2, 
                    linetype="solid", linesize=1, linecolor="red",
                    theme="bw", xscale="none", yscale="none", fontfamily="sans",
                    r2x=1.0, r2y=0.95, r2size=4, r2color="blue",r2font="plain",
                    xmin=NA, ymin=NA, xmax=NA, ymax=NA,
                    fontmain=c(14,"bold","black"), fontxname=c(12,"plain","black"), fontyname=c(12,"plain","black"), 
                    fontxticks=c(12,"plain","black"), fontyticks=c(12,"plain","black"),
                    axiscolor="black",tickcolor="black",axisthick=0.5,tickthick=0.5,
                    ticklength=1, xticks.by=NULL, yticks.by=NULL, 
                    titlejust="center", legheadsize=12, legtextsize = 10,
                    ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                    fheight=NULL, dpi=300, remove=NULL) {
  cuf_apply_defaults(match.call(), environment())
  transz = function(zvar,zscale) {
    if (zscale=="log10") zv = log10(zvar)
    else if (zscale=="log2") zv = log2(zvar)
    else if (zscale=="sqrt") zv = sqrt(zvar)
    else zv = zvar
    return(zv)
  }
  fnz = function(zscale) {
    if (zscale == "none") return("")
    else return(paste(zscale," ",sep=""))
  }
  if (is.logical(showr2eqn)) showr2eqn = ifelse(showr2eqn,"both","n")
  if (is.null(doline) ||
      (!is.logical(doline) && (doline %in% c("n","no","N","NO","No")))) 
     {doline = F; showr2eqn = "n"}
  else doline = T
  depsuby = deparse(substitute(yvar)); depsubx = deparse(substitute(xvar))
  if (is.null(yname)) yname = depsuby
  if (is.null(xname)) xname = depsubx
  if (is.null(title)) title = paste(yname,"vs",xname)
  #cat ("\nyname,xname,title:",yname,"/",xname,"/",title,"/")
  if (is.null(theme)) theme = "bw"
  if (!(theme %in% c("grey","gray","bw","linedraw","light","dark","minimal","classic","void"))) {
    cat("\ntheme='",theme,"' no good. Taken to be 'bw'",sep=""); theme = "bw"
  }
  xv = transz(xvar,xscale); fnx = fnz(xscale)
  yv = transz(yvar,yscale); fny = fnz(yscale)
  cort <- cor.test(xv, yv, method = "pearson")
  cort$data.name = paste(yname,"and",xname); if (!minimal) print(cort)
  fit = lm(yv ~ xv); fit$call = paste("lm(",yname," ~ ",xname,")",sep="")
  names(fit$coefficients) = c("(Intercept)",xname)
  fitlm = summary(fit); if (!minimal) print(fitlm)
  intercept = fitlm$coefficients[1]
  slope = fitlm$coefficients[2]  #should be 1,1 and 2,1?
  rsq = fitlm$r.squared; pval = fitlm$coefficients[2,4]
  # par(mfrow=c(2,2))
  # mfrpre=par()$mfrow
  ggplot(data.frame(yvar,xvar), aes(x=xvar,y=yvar)) +
    geom_point(shape=shape,color=dotcolor,size=dotsize) 
  #return(1) can't figure a way to put multiple plots in one
  p <- ggplot(data.frame(yvar,xvar), aes(x=xvar,y=yvar)) +
    geom_point(shape=shape,color=dotcolor,size=dotsize) 
  # par(mfrow=c(2,2))
  # mfrpost=par()$mfrow
  # cat("\nmmfrpre:"); print(mfrpre); cat("\nmmfrpost:"); print(mfrpost)
  if (doline) {
    p <- p + geom_smooth(formula = y ~ x, method=lm, se=FALSE,
            linewidth=linesize, linetype=linetype, color=linecolor)
    if (!(showr2eqn %in% c("n","no","N","NO","No"))) {
      pvtex = ifelse(pval<0.001,", p < 0.001",
                     paste(", p=",signif(pval,3),sep=""))
      r2tex = paste("R\U00B2=",signif(rsq,3),pvtex,sep="")
      if (slope >=0) {chpm = " +"; r2y = 1-r2y}
      else chpm = " "
      eqtex = paste(fny,"y = ",signif(intercept,3),chpm,signif(slope,3)," ",fnx,"x",sep="")
      if (showr2eqn=="r2") legtex = r2tex
      else if (showr2eqn=="eqn") legtex = eqtex
      else {
        if (showr2eqn!="both") cat("\nshowr2eqn taken to be 'both'\n")
        legtex = paste(r2tex,"\n",eqtex,sep="")
      } # need to recode below if either axis is reversed by zmax<zmin
      # cat ("\ngeomtext\n") no text if scale set before or after
      p <- p + geom_text(x=min(xvar)+r2x*(max(xvar)-min(xvar)), hjust=1.0,
                y=min(yvar)+r2y*(max(yvar)-min(yvar)), family=fontfamily,
                label=legtex,color=r2color,size=r2size,fontface=r2font)
    }
  }
  if (xscale != "none") p <- p + xscale(xscale) #+ scale_x_continuous(trans='log2')
  if (yscale != "none") p <- p + yscale(yscale)
  funtheme = get(paste("theme_",theme,sep=""))
  p <- p + labs(x=xname, y=yname) + funtheme() 
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
  p <- ggpar(p, xlim=c(xmin,xmax), ylim=c(ymin,ymax), font.family=fontfamily,
      font.main = fontmain, font.x = fontxname, font.y = fontyname,
      font.xtickslab = fontxticks, font.ytickslab = fontyticks,
      xticks.by=xticks.by, yticks.by=yticks.by,
      x.text.angle=xangle, y.text.angle=yangle, caption=caption)
  #ggsave("scatter.jpg",dpi=dpi)
  if (minimal) {
    retm = NULL; retm$A = p; retm$B = slope; retm$C = intercept
    retm$D = pval; retm$E = rsq #; print(retm)
    return(retm)
  } 
  cu_plout(p,"cuscatter", ftype=ftype, fname=fname,scale=fscale,
           width=fwidth,height=fheight, dpi=dpi, remove=remove)
}
