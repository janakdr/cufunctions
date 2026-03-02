#' internal cufunction to prepare for p-value horizontals
#' @keywords internal
cu_pvline = function(df,pvpairs,pvalues,nlev1,nlev2,pvlab,pnosig,psignif,p2stars,p3stars,p4stars,
                     letbar,pvypos,pvnshide,pvspill,chpvref,ebars,dots,yscale) {
  if (pvlab == "p.signif") pvlab = "*"
  pvcuts = c(pnosig,psignif,p2stars,p3stars,p4stars)
  if (pvlab == "*") pvcuts[1] = ifelse(pvnshide,pvcuts[2],1)
  nlev = nlev1 * nlev2 # ncol(df)-1 didn't work with cutable1 nlev2=1
  # no good if nlevnom < nlev1*nlev2 due to missing combinations
  if (pvlab != "p") if (pvlab == "*") pvlab = "p.signif"
  else {
    cat("\npvlab ",pvlab," no good - must be p or *. Taken to be p")
    pvlab = "p"
  }
  # pvalues can be (nlev-1)*(nlev-1) matrix or a vector with the same elements
  # NOOO: R allows matrix elements to be referenced as if it were a vector
  # same logic in cu_letbar
  getij = function(ir) {
    jg1 = floor(ir/pvmod); ig2 = ir %% pvmod
    if (jg1 > ig2) {i = jg1; jg1 = ig2; ig2 = i}
    return(jg1>0 && ig2<=nlev)
  }
  pvmod = ifelse(nlev<10,10,100); dostdpairs = T; doallpairs = F
  npairs = length(pvpairs)
  if (npairs>1) {
    for (ir in 1:npairs) {
      dostdpairs = F # do npairs lines
      if (!getij(pvpairs[ir])) {
        cat("\npvpairs",ir," no good:",pvpairs[ir],". Doing std")
        dostdpairs = T; break
      }
    }
  }
  else if (is.numeric(pvpairs)) if (pvpairs>nlev) 
    if (getij(pvpairs)) {pvpairs = c(pvpairs); dostdpairs = F}
    else cat("\npvpairs no good:",pvpairs,". Doing std")
  else { # all groups compared to pvpairs
    if (pvpairs <= 0)
      {cat("\npvpairs bad value:",pvpairs,". taken to be 1"); pvpairs = 1}
    for (i in 1:nlev) {
      if (i != pvpairs) {
        if (i < pvpairs) {ig2 = pvpairs; jg1 = i}
        else {ig2 = i; jg1 = pvpairs}
        pval = pvalues[ig2-1 + (jg1-1)*(nlev-1)]  # pvalues[ig2-1,jg1]
        letbar[i] = cu_mapvalet(pval,pvlab,pvcuts) # ?? paste(pvprefix,...)
      }
    }
    letbar[pvpairs] = chpvref; return(list(NULL,0,pvlab,NULL,letbar))
  }
  else if (pvpairs=="no" || pvpairs=="none") 
    return(list(NULL,0,pvlab,NULL,letbar))
  else if (substr(pvpairs,1,1)=="#" && nlev2>1) {   # regexpr("#",pvpairs)[1] > 0 to locate #
    lref = as.numeric(substring(pvpairs,2))
    #cat ("\nlref:",lref)
    if (is.na(lref) || lref > nlev2) {
      cat("\npvpairs bad:",pvpairs,". taken to be #1")
      lref = 1
    }
    ib = 0
    for (j1 in 1:nlev1) {
      lre = lref + (j1-1)*nlev2
      for (i2 in 1:nlev2) {
        ib = ib+1; lro = i2 + (j1-1)*nlev2
        if (i2==lref) letbar[ib] = chpvref
        else {
          i = ifelse(i2<lref, lre-1 + (lro-1)*(nlev-1), lro-1 + (lre-1)*(nlev-1))
          pval = pvalues[i]
          #cat("\nib,j1,i2,lre,lro,i,pval",ib,j1,i2,lre,lro,i,pval)
          letbar[ib] = cu_mapvalet(pval,pvlab,pvcuts)
        }
      }
    }
    #cat ("\n"); print(pvalues); cat ("\nletbar:"); print(letbar)
    return(list(NULL,0,pvlab,NULL,letbar))
  }
  else if (pvpairs == "all") doallpairs = T
  else if (pvpairs != "std") 
    cat("\npvpairs bad:",pvpairs,". taken to be std")
  if (dostdpairs) if (doallpairs) npairs = (nlev*(nlev-1))/2
  else npairs = (nlev1 * nlev2 * (nlev1+nlev2-2))/2
  pvdf = data.frame(matrix(ncol=4),stringsAsFactors = F)
  #cat("\nnpairs,dostdpairs,pvpairs,pvalues", npairs,dostdpairs,pvpairs)
  #print(pvalues)
  ir = 0
  if (dostdpairs) {
    #cat("\npvalues"); print(pvalues)
    for (jg1 in 1:(nlev-1)) {
      kg1 = 1 + floor((jg1-1)/nlev2); kg2 = 1 + (jg1-1)%%nlev2
      for (ig2 in (jg1+1):nlev) {
        mg1 = 1 + floor((ig2-1)/nlev2); mg2 = 1 + (ig2-1)%%nlev2
        #cat ("\nkg1,kg2,mg1,mg2:",kg1,kg2,mg1,mg2)
        if (doallpairs || kg1==mg1 || kg2==mg2) {
          pval = pvalues[ig2-1 + (jg1-1)*(nlev-1)]  # pvalues[ig2-1,jg1]
          #cat(" pval:",pval)
          if (!is.na(pval)) {
            pval = signif(pval,ifelse(pval>pvcuts[2],2,3))
            if (pval < pvcuts[1] || !pvnshide) {
              ir = ir+1; pvdf[ir,1] = jg1; pvdf[ir,2] = ig2
              #cat("\nig2,jg1,pval,ir", ig2,jg1,pval,ir)
              pvdf[ir,3] = pval; pvdf[ir,4] = cu_mapvalet(pval,pvlab,pvcuts)
              #cat("pval,pvdf:",pval); print(pvdf)
            }
          }
        }
      }
    }
  }
  else for (jr in 1:npairs) {
    i = pvpairs[jr]; jg1 = floor(i/pvmod); ig2 = i %% pvmod
    if (jg1 > ig2) {i = jg1; jg1 = ig2; ig2 = i}
    pval = pvalues[ig2-1 + (jg1-1)*(nlev-1)]  # pvalues[ig2-1,jg1]
    #cat(" pval:",pval)
    if (!is.na(pval)) {
      pval = signif(pval,3)  # pvalues[ig2-1,jg1]
      if (pval < pvcuts[1] || !pvnshide) {
        ir = ir+1; pvdf[ir,1] = jg1; pvdf[ir,2] = ig2
        #cat("\nig2,jg1,pval,npairs", ig2,jg1,pval,npairs)
        pvdf[ir,3] = pval; pvdf[ir,4] = cu_mapvalet(pval,pvlab,pvcuts)
      }
    }
  }
  if (ir==0) {
    #cat("\nno p-values to display\n")
    return(list(NULL,0,pvlab,NULL,letbar))
  }
  npairs = ir
  colnames(pvdf) = c("group1","group2","p","p.signif")
  # print(pvdf)
  ytopv = NULL; ytop = 0; ybot =  0
  jadd = 6; pvdfac = 0  # SEM, used with ebars=2,3
  if (dots > 0) {jsub = 11; jsul = 7} # maximum, minimum
  else if (ebars==4 || yscale=="log10") {jsub = 10; jsul = 8} # 3rd, 1st quartiles
  else {
    jsub = 3; jsul = 3; pvdfac = 1  # mean
    if (ebars==1) jadd = 4 # SD
  }
  # cat ("\njadd,pvdfac,jsub,jsul,nlev",jadd,pvdfac,jsub,jsul,nlev,"\n"); print(df)
  for (ilev in 1:nlev) {
    if (ebars==3) if (dots==0) # critical value for t with n-1 df
      pvdfac = qt(0.975,max(1,as.numeric(df[1,ilev])-
                              as.numeric(df[2,ilev])-1))
    # cat (ilev)
    ytopv[ilev] = as.numeric(df[jsub,ilev])+pvdfac*as.numeric(df[jadd,ilev])
    ybot = min(ybot,as.numeric(df[jsul,ilev])-pvdfac*as.numeric(df[jadd,ilev]))
    ytop = max(ytop,ytopv[ilev])
    # cat(ybot,ytop)
  }
  iflog = F # didn't need it before??
  yrange = ytop-ybot # need yrange even if pvypos is given
  if (is.null(pvypos)) {
    iflog = yscale=="log10"
    if (iflog) {
      pvspace = ytop**0.05  #space for p-value text
      for (ilev in 1:nlev) ytopv[ilev] = ytopv[ilev]*(1+(pvspace-1)/4)
    }
    else {
      pvspace = 0.07*yrange  #space for p-value text
      for (ilev in 1:nlev) {
        ytopv[ilev] = ifelse(ytopv[ilev]>0,ytopv[ilev]+pvspace/2,pvspace)
      }
    }
    #cat("\niflog,pvspace,ytopv",iflog,pvspace,ytopv)
    #cat("\npvspace,pvspill:",pvspace,pvspill)
    for (ir in 1:npairs) {
      ypir = -1e40; irb = pvdf[ir,1]; ire = pvdf[ir,2]; jr = 1
      for (ibar in irb:ire) ypir = max(ypir,ytopv[ibar])
      while (jr < ir) {  # need to redo loop till no change; "for" won't work
        #cat("\n,ir,jr,ypir",ir,jr,ypir)
        jrb = pvdf[jr,1]; jre = pvdf[jr,2]
        if (iflog) if (((irb>=jrb && irb<jre) || (ire>jrb && ire<=jre)) &&
            ypir > pvypos[jr]/pvspace && ypir < pvypos[jr]*pvspace) 
            {ypir = pvypos[jr]*pvspace; jr=1}
          else jr = jr+1
        else if ((( pvspill&&((irb>=jrb && irb<=jre) || (ire>=jrb && ire<=jre)))
               || (!pvspill&&((irb>=jrb && irb< jre) || (ire> jrb && ire<=jre))))
                 && ypir > pvypos[jr]-1.01*pvspace && ypir < pvypos[jr]+0.99*pvspace) 
            {ypir = pvypos[jr] + pvspace; jr=1}
        else jr = jr+1
      }
      #if (yscale=="log10") ypir = log10(ypir)
      pvypos = c(pvypos, ypir)
      #cat(ypir)
    }
  }
  #cat("\npvypos",pvypos,"\n")
  if (iflog) for (ir in 1:npairs) pvypos[ir] = log10(pvypos[ir])
  return(list(pvdf, pvypos, pvlab, yrange, letbar))
}
