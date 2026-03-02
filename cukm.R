cukm = function(timevar, statvar, ttmtvar, kmtype="survival", pvalue=T,
                allkmpairs=T, tmax=0, xlab="Time", ylab=NULL, title=NULL,
                rho=0, legend="top", legend.labs=NULL, risk.table.y.text=F,
                scale="default", linetype=1, censor=TRUE, censor.shape="+",
                ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                fheight=NULL, dpi=300, remove=NULL)
{
  #' Kaplan-Meier curves with p-value, # at risk
  #'
  #' Does K-M curves of 2nd argument vs 1st for different levels of 3rd argument.
  #' Displays table of # at risk, and p-value. Does survival (default) or cumulative incidence.
  #' Plus sign indicates loss to follow-up (right-censoring).
  #' @param timevar , statvar, ttmtvar (3 required)
  #' @param kmtype ="survival" (default) or "ci" (cumulative incidence)
  #' @param pvalue =T (default) or F (for no p-value)
  #' @param allkmpairs =T (default) or F (to not compare each pair of curves)
  #' @param tmax =0 (default for no max T), numeric study end time
  #' @param xlab /ylab/title=NULL to override labels of x-axis/y-axis/title
  #' @param rho =0 (log-rank default) or 1 (Peto-Peto)
  #' @param legend ="top" (default), "bottom", "right", "left" to locate legend
  #' @param legend.labs =NULL(default)/c(...) to override risk factor level names
  #' @param risk.table.y.text =F (default) =T to list level names in risk table
  #' @param scale ="percent" to have y-axis display percent instead of decimal (default)
  #' @param linetype =1 (default for solid), 2 (dashed), 3 (dotted)
  #' @param censor =T (default) or F to show (or not) censoring on curves
  #' @param censor.shape ="+" (default) or any character to indicate censoring
  #' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
  #' @param fname =NULL(default) or set to prefix for "funcname.ftype"
  #' @param fscale ,fwidth,fheight =NULL(default) or set to numerical value
  #' @param dpi =300 (default) or set to desired resolution in dpi in file
  #' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
  #' @return returns nothing
  #' @examples
  #' cukm(TimeToEvent, Outcome, Treatment)
  #' cukm(TimeToEvent, Outcome, Treatment, kmtype="ci") for cumulative incidence
  #' @export
  TimeNam = deparse(substitute(timevar))
  TtmtNam = deparse(substitute(ttmtvar))
  StatNam = deparse(substitute(statvar))
  if (is.character(statvar)||is.factor(statvar)) statvar = as.integer(statvar)
  if (is.character(ttmtvar)) ttmtvar = as.factor(ttmtvar)
  ds = data.frame(Time=timevar,Status=statvar,Factor=ttmtvar)
  if (tmax > 0) for (i in 1:nrow(ds)) {
    if (ds[i,1] > tmax) {ds[i,1] = tmax; ds[i,2] = 0}
  }
  nlev=nlevels(ttmtvar); levs=levels(ttmtvar)
  if (is.null(title)) title=paste("Kaplan-Meier for",StatNam,"at",TtmtNam,"levels")
  if (kmtype=="survival"){ylbl="Event-free Survival"; fun = NULL; pvalx = 0}
  else {ylbl="Cumulative Incidence"; fun = "event"; pvalx = max(timevar)*.85}
  if (is.null(ylab)) ylab=ylbl
  if (is.null(legend.labs)) {
    legend.labs = levs[1]
    for (i in 2:nlev) {legend.labs = c(legend.labs,levs[i])}
  }
  cat("\nCall: survdiff(Surv(",TimeNam,",",StatNam,") ~ ",TtmtNam,")\n",sep="")
  kapmodesc = paste("Kaplan-Meier model:",TimeNam,"to",StatNam,"(event) depends on",TtmtNam)
  if (allkmpairs || nlev<=2) for (i in 1:(nlev-1)) {
    for (j in (i+1):nlev) {
      dsnew = subset(ds, ttmtvar==levs[i] | ttmtvar==levs[j])
      cat("\n\n",levs[i], "vs.", levs[j],"\n")
      SurvTst = survdiff(Surv(Time,Status) ~ Factor, data=dsnew, rho=rho)
      names(SurvTst$n) = c(paste(TtmtNam,substr(names(SurvTst$n[1]),start=7,stop=30),sep=""),
                           paste(TtmtNam,substr(names(SurvTst$n[2]),start=7,stop=30),sep=""))
      SurvTst$call = ifelse(i+j==3, kapmodesc, "")
      print(SurvTst)
      if (rho==0) cat("    by the log-rank or Mantel-Haenszel method\n")
      else if (rho==1) cat("by the Peto & Peto modification of the Gehan-Wilcoxon method\n")
      else cat("by a mix of log-rank and Peto-Peto-Gehan-Wilcoxon\n")
      if (SurvTst$exp[1] * SurvTst$exp[2] > 0) {
        # https://www.graphpad.com/support/faq/hazard-ratio-from-survival-analysis/
        denom = SurvTst$exp[2] * SurvTst$obs[1]
        HR = ifelse(denom!=0,SurvTst$obs[2]*SurvTst$exp[1]/denom,1e20)
        cat ("Hazard Ratio ",levs[j],":",levs[i],
             " ",round(HR,3),sep="")
        if (HR > 0) {
          sdlnhr = sqrt((1/SurvTst$exp[1])+(1/SurvTst$exp[2]))
          logHR = log(HR); conflnhr = 1.96*sdlnhr
          cat(" (95% CI ",round(exp(logHR-conflnhr),3),"-",
              round(exp(logHR+conflnhr),3),")\n",sep="")
        }
      }
    }
  }
  if (nlev>2) {
    pvalue=F
    if (allkmpairs) cat("\n\n Overall \n")
    SurvTst = survdiff(Surv(Time,Status) ~ Factor, data=ds, rho=rho)
    tablenams = paste(TtmtNam,substr(names(SurvTst$n[1]),start=7,stop=30),sep="")
    for (i in 2:nlev){
      tablenams = c(tablenams, paste(TtmtNam,substr(names(SurvTst$n[i]),start=7,stop=30),sep=""))
    }
    names(SurvTst$n) = tablenams; SurvTst$call = kapmodesc 
    print(SurvTst)
  }
  cat("'+' on K-M curves indicates censoring (loss to follow-up)")
  PlotObj = survfit(Surv(Time,Status) ~ Factor, data=ds)
  oldw <- getOption("warn")
  options(warn = -1)
  p=ggsurvplot(PlotObj, data=ds, fun=fun,  pval=pvalue, pval.coord=c(pvalx, 0.1), risk.table=TRUE,
               title=title, legend=legend, legend.title=TtmtNam, xlab=xlab, ylab=ylab,
               risk.table.y.text=risk.table.y.text, legend.labs=legend.labs,
               linetype=linetype, surv.scale=scale, censor=censor,censor.shape=censor.shape)
  # p <- p + theme(plot.title = element_text(hjust = 0.5))  
  options(warn = oldw)
  if (is.character(ftype)) ftype = "emf" # no other is possible
  cu_plout(p,"cukm",ftype=ftype, fname=fname, scale=fscale,
           width=fwidth, height=fheight, dpi=dpi, remove=remove)
}
