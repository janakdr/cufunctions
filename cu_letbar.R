cu_letbar = function(pvalue,psigcldnif,mapord,nlev,letleft=F,nletbarmax=3) {
  #' internal cufunction to construct significance letters on bars
  letfromcliq = function(ncl) return(ifelse(ncl<=26, letters[ncl], 
                ifelse(ncl<=52, LETTERS[ncl-26], "@")))
  # Janak: efficient to start with least connected nodes?
  # Find cliques for nodes with nedges=0 or 1 - flag as maximal
  # Note, in each clique, highest ranked (least connected) node and node count
  # If, on adding a node, the node count equals nedges[l.c.n], flag as maximal
  # Me: a bit skeptical since a node with 2 edges may be in 2 cliques of 2 each
  # Me: Still, if it cuts down on number of look-ups.
  if (psigcldnif <= 0) letbar = NULL   # CHECK: why no return(letbar)?
  else {
# pvalue is (nlev-1)x(nlev-1) matrix with p-values diagonal and below
# first column has first level compared to all other levels, 2nd has 2nd vs nlev-2, etc
#    print(pvalue)
    if (nlev > 30) cat("\nWorking on CLD letters for bars. With",nlev,"groups, this may take a while.",
       "\n   Each group and cliques to then will be shown to indicate progress")
    ipo = -1; nsmat = matrix(c(rep(0,nlev*nlev)),nrow=nlev,ncol=nlev)
    whichcl = nsmat; nclbar = c(rep(0,nlev)) # nletb set to whichcl just to declare
    for (i in 1:(nlev-1)) {
      im = mapord[i]
      for (j in (i+1):nlev) {
        jm = mapord[j]; pval = pvalue[ipo+j]
        #cat("ipo,im,jm,pval",ipo,im,jm,pval,"\n")
        if (is.na(pval)) pval = 1  # when a group has only 1 (or 0?)
        else if (regexpr("<",pval) >0) pval = 0  # pval is very small
        if (pval > psigcldnif) {nsmat[im,jm] <- nsmat[jm,im] <- 1}
      }
      ipo = ipo+nlev-1
    }
    #    nsmat = matrix(c(0,0,0,1,1,0, 0,0,0,0,1,1, 0,0,0,1,0,1,
    #        1,0,1,0,1,1, 1,1,0,1,0,1, 0,1,1,1,1,0),nrow=nlev,ncol=nlev)
    #print(nsmat)
    if (letleft) nranks = mapord
    else {
      nedges = c(0); for (i in 1:nlev) nedges[i] = sum(nsmat[i,])
      nranks = nlev+1 - rank(nedges,ties.method="first")
      #print(nedges)
    }
    # print(nranks)
    whichin = matrix(c(rep(0,nlev)),nrow=nlev,ncol=1)
    im = nranks[1]; nclique = 1; whichin[im,nclique] = 1
    nclbar[im] = 1; whichcl[im,1] = 1
    for (ir in 2:nlev) {
      im = nranks[ir]; nto = sum(nsmat[im,nranks[1:(ir-1)]]>0)
      # cat("\nim,nto",im,nto,"\n")
      if (sum(nsmat[im,nranks[1:(ir-1)]]>0)==0) { # new clique with just im
        nclique = nclique+1; whichin = cbind(whichin,c(rep(0,nlev)))
        whichin[im,nclique] = 1
        nclbar[im] = nclbar[im]+1; whichcl[im,nclbar[im]] = nclique
        # cat("im,nclique",im,nclique,whichcl[im,1:nclbar[im]],"\n")
      }
      else {
        for (jcl in 1:nclique) {
          okcl = TRUE
          for (jr in 1:(ir-1)) {
            i = nranks[jr]
            if (whichin[i,jcl]>0 && nsmat[im,i]==0) {okcl = F; break}
          }
          if (okcl) {
            for (jr in 1:(ir-1)) {
              i = nranks[jr]
              if (whichin[i,jcl]>0) nsmat[im,i] = -1 # flag that im&i in a clique
            }
            whichin[im,jcl] = 1
            nclbar[im] = nclbar[im]+1; whichcl[im,nclbar[im]] = jcl
            # cat("im,jcl",im,jcl,whichcl[im,1:nclbar[im]],"\n")
          }
        }  # gone through all cliques
        ntodo = sum(nsmat[im,nranks[1:(ir-1)]]>0)
        # cat("im,ntodo,nsmat",im,ntodo,nsmat[im,],"\n")
        while (ntodo > 0) {
          nintmax = 0
          for (jcl in 1:nclique) { # find clique with most nodes adj. to im
            nint = 0
            for (jr in 1:(ir-1)) {
              i = nranks[jr]
              if (whichin[i,jcl]>0 && nsmat[im,i]>0) nint = nint+1
            }
            if (nintmax < nint) {nintmax = nint; jcmax = jcl}
          }
          nclique = nclique+1; whichin = cbind(whichin,c(rep(0,nlev)))
          whichin[im,nclique] = 1
          nclbar[im] = nclbar[im]+1; whichcl[im,nclbar[im]] = nclique
          for (jr in 1:(ir-1)) {
            i = nranks[jr]
            if (whichin[i,jcmax]>0 && nsmat[im,i]>0) {
              whichin[i,nclique] = 1
              nclbar[i] = nclbar[i]+1; whichcl[i,nclbar[i]] = nclique
            }
          } # new clique set up with im and most possible earlier nodes
          # cat("im",im,whichcl[im,1:nclbar[im]],"\nnclique",nclique,which(whichin[,nclique]>0))
          ntodo = ntodo-nintmax
          # cat("im,ntodo,nsmat",im,ntodo,nsmat[im,],"\n")
        }
      }
      if (nlev > 30) {
        cat("cliques at group",im)
        for (jc in 1:nclique) cat("\n",jc,":",which(whichin[,jc]>0))
      }
    }
    letbar = c(rep("",nlev))
    if (nletbarmax<3) nletbarmax=2
    for (i in 1:nlev) {
      nclbi = nclbar[i]
#      cat ("\ni,nclbi,whichcl:",i,nclbi,whichcl[i,1:nclbi])
      if (nclbi <= nletbarmax) nclbn = nclbi
      else {
        nsh = 0; jp = 1; nclbn = 0; whicn = whichcl[i,1]+1; whichcl[i,nclbi+1] = 9999
        for (j in 2:(nclbi+1)) {
          if (whichcl[i,j] == whicn) whicn = whicn+1
          else {
            if (j-jp > nletbarmax) {
              nclbn = nclbn+2; whichcl[i,jp-nsh+1] = 1-whicn
              nsh = nsh+j-jp-2; whichcl[i,j-nsh] = whichcl[i,j]
            }
            else for (jj in (jp+1):j) {nclbn=nclbn+1; whichcl[i,jj-nsh] = whichcl[i,jj]}
            jp = j
          }
        }
      }
#      cat ("\ni,nclbn,whichcl:",i,nclbn,whichcl[i,1:nclbn])
      letbc = ""
      for (j in 1:nclbn) {
        ncl = whichcl[i,j]
        if (ncl < 0) {ncl = -ncl; letbc = paste(letbc,"-",sep="")}
        letbc = paste(letbc, letfromcliq(ncl), sep="")
      }
      # below, if just 1 char, add a space to avoid vertical of error bar
      if (nchar(letbc)==1) letbc = paste(letbc," ",sep="")
      letbar[i] = letbc; # cat ("'",letbc,"'")
    }
    return(letbar)
  }
}
