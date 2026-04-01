#' Does repeated measures anova and post-hoc tests when there is a second, non-repeated factor (long-format - multiple lines for each subject)
#' @param depvar required: dependent variable
#' @param group1 required: repeated grouping factor
#' @param group2 required: non-repeated grouping factor
#' @param Subject required: subject identifier
#' @param interact =TRUE (default), F or FALSE for no interaction
#' @param partialF =FALSE (default), T or TRUE for partial F vs simpler models
#' @param mainx =1 (default), 2 to interchange the two factors in bar graph
#' @param scale ="frequency" (default) or "percent"
#' @param ebars =1 (default)/2/3 (post-hoc t, SD/SE/CL) or 4 (nonparametric, IQR)
#' @param dots =0 (default), 1 to display data on graph
#' @param barcolor ="black" (default) for bar outline color
#' @param barfill ="colors" (default) for colors by group ("lancet" for 2 factors, other journal options "aaas", "jco", "uchicago", "npg"
#' @param plot ="bar" (default) for bar graphs; "box" "violin" "rod" "no"
#' @param order =NULL (default)/c("...") to reorder bars
#' @param psigcld =0 (no letters)/x for CLD letters on bars (any shared letter means P>psigcld, 0 for no letters)
#' @param conf.int =0.95 (default)/x/0 for confidence interval width of contrast estimates (0 for none)
#' @param depname =NULL to override name of depvar
#' @param g1name =NULL to override name of group1
#' @param g2name =NULL to override name of group2
#' @param legend ="top" (default), "bottom", "right", "left" to locate legend
#' @param linetype ="n" (default)/x for no connecting lines ("dashed" "dotted")
#' @param linecolor ="black" (default) line color
#' @param linesize =1 (default)/x for line thickness
#' @param theme ="bw" (default)/x for white background (or "gray" or "classic" (no grid lines))
#' @param size numeric value (e.g. size=1), to change size of points and outlines
#' @param width numeric value between 0 and 1 specifying box width
#' @param yscale (default="none"), can be "log2", "log10", "sqrt"
#' @param font.x =NULL (default) for x-axis font
#' @param xangle =NULL for x-axis value angle: 0 (default) horizontal, 90 vertical, or any value between
#' @param yangle =NULL for y-axis value angle: 0 (default) horizontal, 90 vertical, or any value between
#' @param orientation (default="vertical"), can change to "horizontal"
#' @param posd =NULL (default) set to values around 0.9 to fine-tune group2 bar spacing
#' @param binwfac =NULL (default 30) set to fraction of range within which points will be binned
#' @param dotsize =NULL (default 1) set to fraction of binwidth for dot size
#' @param dotshape =NULL (default) for dot shape
#' @param dotcolor =NULL (default "white") set to dot color
#' @param ftype =NULL(default)/eps/pdf/jpg/jpeg/tiff/png/emf (for hires file or name.emf for Mac)
#' @param fname =NULL(default) or set to prefix for "funcname.ftype"
#' @param fscale =NULL(default) or set to numerical value for file scale
#' @param fwidth =NULL(default) or set to file width
#' @param fheight =NULL(default) or set to file height
#' @param dpi =300 (default) or set to desired resolution in dpi in file
#' @param remove choose from =c("xlab","ylab","x.text","y.text","x.ticks","y.ticks","grid","x.grid","y.grid","axis","x.axis","y.axis")
#' @param barcolor ="black" (default) for bar outline color
#' @param pvpairs ="all" (default) p-value pairs to show
#' @param pvypos =NULL (default) position of p-value lines
#' @param pvsi =0.05 (default) increment for p-value lines
#' @param pvlab ="p" (default) label format for p-values
#' @param pvprefix ="p=" (default) prefix to p-values
#' @param chpvref ="ref" (default) char above reference group bar
#' @param pvcuts =c(0.2,0.05,0.01,0.001) p-value cutpoints
#' @param pvnshide =TRUE (default) to hide non-significant p-values
#' @param pvtipl =0.01 (default) length of p-value line tips
#' @param title =NULL to override plot title
#' @param suff =NULL suffix for file name
#' @param caption =NULL (default) caption text
#' @return returns nothing
#' @examples
#' # Internal function, not exported; called via curepmeas()
#' \dontrun{
#' cu_rep2way(TG, Diet, sex, ID)  # with interaction between the two fixed factors
#' # with no interaction (pointless, same result as without 2nd factor)
#' cu_rep2way(TG, Diet, sex, ID, interact=FALSE)
#' }
cu_rep2fac = function(depvar, group1, group2, Subject,
                      interact=TRUE, partialF=FALSE, mainx=1,
                      scale="frequency", ebars=1, dots=0, barcolor="black", barfill="colors", plot="bar",
                      order=NULL, psigcld=0, conf.int=0.95,
                      depname=NULL, g1name=NULL, g2name=NULL,
                      pvpairs="all", pvypos=NULL, pvsi=0.05, pvlab="p", 
                      pvprefix="p=", chpvref="ref",
                      pvcuts=c(0.2,0.05,0.01,0.001),pvnshide=T, pvtipl=0.01,
                      title=NULL, legend="top", suff=NULL, caption=NULL,
                      linetype="n", linecolor="black", linesize=1, theme="bw",
                      size=NULL, width=NULL, yscale="none", font.x=NULL, 
                      xangle=NULL, yangle=NULL, orientation="vertical",
                      posd=NULL, binwfac=30, dotsize=NULL, dotshape=NULL, dotcolor=NULL,
                      ftype=NULL,fname=NULL,fscale=NULL,fwidth=NULL,
                      fheight=NULL,dpi=300,remove=NULL) 
{
  cuf_apply_defaults(match.call(), environment())
  if(is.null(depname)) depname = deparse(substitute(depvar))
  if(is.null(g1name)) g1name = deparse(substitute(group1))
  if(is.null(g2name)) g2name = deparse(substitute(group2))
  if (psigcld < -3) {chslamp = "/"; depvar = as.factor(depvar)}
  else {chslamp = "&"; if (is.character(depvar)) depvar = as.factor(depvar)}
  nameboth12 = paste(g1name,chslamp,g2name,sep="")
  # cat("\ng2name,nameboth12:",g2name,nameboth12)
  groupvar = interaction(group1, group2, lex.order=T, sep=chslamp) 
  #factor(paste(group1, group2, sep="&"))
  ivar = deparse(substitute(Subject))
  gp1 = nlevels(group1); gp2 = nlevels(group2)
  g1names = levels(group1); g2names = levels(group2)
  nlevboth12=nlevels(groupvar); bothnames = levels(groupvar);
  # print(g1names); print(g2names); print(bothnames)
  cat("\n")
  # print(depvar); print(group1); print(group2); print(groupvar) 
  ds = data.frame(depvar, group1, group2, groupvar)
  df = cu_table0(depvar, groupvar, brief=F, doAll=F)[[1]]
  colnames(df)=bothnames
  cat(depname,"\n")
  print(df)
  nlevdep = nlevels(depvar); nlem = nlevboth12  # cf cu1way and NA's
  #    desc = by(depvar, groupvar, summary.factor)
  df2 = data.frame(A = rep(0,nlevdep),stringsAsFactors = F)
  #    df2 = matrix(nrow=nlevels(depvar),ncol=nlev)
  # why can't it be a matrix? no colnames for a matrix?
  rownams = levels(depvar)
  if (psigcld < -3) for (i in 1:nlevdep) {
    delta = as.numeric(rownams[i])
    rownams[i] = ifelse(delta==0,"same",
                        ifelse(delta<0,paste("dn",-delta),paste("up",delta)))
  }
  rownames(df2) = rownams
  for (j in 1:nlem) {
    for (i in 1:nlevdep) df2[i,j]= as.integer(substr(df[i+2,j],regexpr("%",df[i+2,j])+3,10))
  }
  colnames(df2) = levels(groupvar)
  partitle=paste(g1name,ifelse(psigcld < -1,"changes","levels"))
  df2mat = as.matrix(df2); colsum = rep(0,nlem)
  if (scale == "percent") for (j in 1:nlem) {
    colsum[j] = sum(df2mat[,j])
    if (colsum[j]>0) df2mat[,j] = 100*df2mat[,j]/colsum[j]
  }
  plotobj <- graphics::barplot(df2mat, xlab=partitle, ylab=depname,
                     main=paste(depname,"distribution at different",partitle),
                     col=grDevices::rainbow(nlevdep), legend = rownames(df2),
                     args.legend = list(x = "topright", bty = "n"))
  if (scale == "percent") plotobj <- graphics::text(plotobj,102,labels=colsum,xpd=T)
  plotobj # cu_plout with barplot obj prints out x vector or NULL
  #cu_plout(plotobj,"curm",emf,suff=suff)
  if (psigcld > -4) return(NULL)
  cat("\n"); print(df2)   # comparing ups with downs
  chifish = cu_chi2fish(df2);
  cat("\nComparing ups vs downs by binom.test (sign test)\nup:dn   ")
  ncstr = rep(0,nlevboth12)
  for (i in 1:(nlevboth12)) {
    strout = levels(groupvar)[i]; nc8 = max(nchar(strout),8); ncstr[i] = nc8+1
    cat(sprintf("%-*s",nc8,strout)," ",sep=""); 
  }
  levdepint = as.numeric(levels(depvar))
  ib = 1; ie = nlevdep; pvals = rep(0,nlevboth12); nposneg = rep(0,nlem); ifsomena=F
  while (ie > ib) {
    delb = abs(levdepint[ib]); dele = levdepint[ie]; deluse = max(delb,dele)
    jb = ifelse(delb==deluse, ib, ib-1)
    je = ifelse(dele==deluse, ie, ie+1)
    # nbin = nbin+1; rownames(bintab[nbin]) = paste("|| >",deluse-1)
    # cat("\nnbin,ib,ie,jb,je",ib,ie,jb,je)
    cat("\n",sprintf("%-9.9s",paste("|\u0394| >",deluse-1))," ",sep="")
    for (j in 1:nlem) {
      npos = ifelse(je>nlevdep,0,sum(df2[je:nlevdep,j]))
      nneg = ifelse(jb<1,0,sum(df2[1:jb,j])); npn = npos+nneg
      if (npn <= nposneg[j]) {pvals[j] = -1; ifsomena = T}
      else {pvals[j] = stats::binom.test(nneg,npn)$p.value; nposneg[j]=npn}
      cat(sprintf("%-*s",ncstr[j],paste(npos,":",nneg,sep="")))
    }
    cat("\n         ")
    for (j in 1:nlem) 
      cat(sprintf("%-*s",ncstr[j],paste("p=",cu_pval9(pvals[j]),sep="")))
    if (delb==deluse) ib = ib+1
    if (dele==deluse) ie = ie-1
  }
  if (ifsomena) cat ("\np-value NA means n zero or same as one above")
}
