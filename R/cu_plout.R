#' internal function to display plot object, write hires .ftype or .emf file
#' @importFrom car avPlots
#' @keywords internal
cu_plout = function(plotobj,funcname,ftype=NULL,fname=NULL,suff=NULL,seq=0,
                    scale=NULL,width=NULL,height=NULL,units="in",dpi=300,
                    avplots=F, remove=NULL)
{
  if (!is.null(remove)) 
    for (k in 1:length(remove)) plotobj <- plotobj + ggpubr::rremove(remove[k])
  # mfr = par()$mfrow; cat("\nmmfr:"); print(mfr)
  oldw <- getOption("warn")
  options(warn = -1)
  #cat ("\nafter warn -1")
  if (!avplots) print(plotobj) # plotobj produces nothing if not barplot
   # prints $`i` for each plot, then attr(,"class") [1] list ggarrange
  options(warn = oldw)
  #cat ("\nafter warn oldw")
  if (is.character(ftype)) {
    if (avplots) ftype = "emf" # can't create anything else with lm object
    if (is.character(fname)) fname = paste(fname,"-",funcname,sep="")
    else fname = funcname
    fname = paste(fname,ifelse(is.null(suff),"",paste("-",suff,sep="")),
                  ifelse(seq==0,"",paste("-",seq,se2p="")),".",ftype,sep="")
    #cat("\nfname:",fname,">")
    if (ftype=="emf") { 
      if (!requireNamespace("devEMF", quietly = TRUE))
        stop("Package 'devEMF' is required for EMF output but is not installed.")
      devEMF::emf(fname, emfPlus=F)
      oldw <- getOption("warn")
      options(warn = -1)
      if (avplots) avPlots(plotobj) #avPlots returns coordinates, so can't print object
      else print(plotobj)  # this is writing to .emf; top wrote to screen
      options(warn = oldw)
      grDevices::dev.off()
    }
    else {# produce a file, usually hires
      if (is.null(scale)) scale = 1
      if (is.null(width)) width = 6.55
      if (is.null(height)) height = 7.1
      ggsave(fname,plotobj,scale=scale,width=width,height=height,
             units=units,dpi=dpi)
    }
  }
  else if (!is.null(ftype)) print("ftype has to be set to something within quotes")
  cat("") #needed to avoid "RStudioGD" and 2 on next line
}
