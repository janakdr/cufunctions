#' internal cufunction to construct significance letters on bars
#' @keywords internal
cu_letbar = function(pvalue,psignif,mapord,nlev,nletbarmax=3) {
  letfromcliq = function(ncl) return(ifelse(ncl<=26, letters[ncl], 
                                            ifelse(ncl<=52, LETTERS[ncl-26], "@")))
  if (psignif <= 0) letbar = NULL
  else {
    # pvalue is (nlev-1)x(nlev-1) matrix with p-values diagonal and below
    # first column has first level compared to all other levels, 2nd has 2nd vs rest, etc
    #    print(pvalue)
    if (nlev > 15) cat("\nWorking on CLD letters for bars. With",nlev,"groups, this may take a while.",
                       "\n   Each letter and its bars will be shown to indicate progress")
    ipo = -1; nsmat = matrix(c(rep(0,nlev*nlev)),nrow=nlev,ncol=nlev)
    whichcl = nsmat; nclbar = c(rep(0,nlev)) # nletb set to whichcl just to declare
    for (i in 1:(nlev-1)) {
      im = mapord[i]
      for (j in (i+1):nlev) {
        jm = mapord[j]; pval = pvalue[ipo+j]
        if (is.na(pval)) pval = 1  # when a group has only 1 (or 0?)
        else if (regexpr("<",pval) >0) pval = 0  # pval is very small
        if (pval > psignif) {nsmat[im,jm] <- nsmat[jm,im] <- 1}
      }
      ipo = ipo+nlev-1
    }
    #    nsmat = matrix(c(0,0,0,1,1,0, 0,0,0,0,1,1, 0,0,0,1,0,1,
    #        1,0,1,0,1,1, 1,1,0,1,0,1, 0,1,1,1,1,0),nrow=nlev,ncol=nlev)
    #    print(nsmat)
    nclique = 0; whichin = c(rep(0,nlev)); notinclique = c(rep(TRUE,nlev));
    for (i in 1:nlev) {
      nincl = 1; whichin[1] = i; notinclique[i] = FALSE; j = i; ntry=0
      while (nincl > 0) {
        nincp = nincl
        #        cat ("\nnincl,i,j",nincl,i,j)
        if (j < nlev) for (j in (j+1):nlev) {
          if (nsmat[i,j]==1) { # similar
            okincl = TRUE
            for (iin in 1:nincl) {
              if (nsmat[whichin[iin],j]==0) {okincl = FALSE; break}
            }
            if (okincl) {
              if (nincl==nincp) { # new clique
                nclique=nclique+1;
                #                cat (" nincl,nclique",nincl,nclique)
              }
              nincl = nincl+1; whichin[nincl] = j; notinclique[j] = FALSE
              #              cat (" j",j)
            } # okincl
          } # if nsmat
        } # for j
        if (nincl>nincp) { # is new clique contained in earlier one?
          oknew = TRUE
          for (ip in 1:whichin[nincl]) {
            if (notinclique[ip]) {
              okincl = TRUE # is level not in clique similar to all?
              for (iin in 1:nincl) {
                if (nsmat[whichin[iin],ip]==0) {okincl = FALSE; break}
              }
              if (okincl) {oknew = FALSE; break}
            }
          }
          if (oknew) {
            if (nlev > 15) cat("\n",letfromcliq(nclique),":",
                               whichin[1:nincl],"after",ntry); ntry=0
                               for (iin in 1:nincl) {
                                 j = whichin[iin]; nclbar[j] = nclbar[j]+1; whichcl[j,nclbar[j]] = nclique
                               }
          }
          else { # no new clique
            #            cat(" no new clique, reset nontinclique",nincp+1,"to",nincl)
            ntry=ntry+1
            # for (iin in (nincp+1):nincl) {notinclique[whichin[iin]] = TRUE}
            nclique=nclique-1; # nincl = nincp # back to previous clique
            #              back to nincp caused a bug when first clique with an i was old,
            #               but this looks to be very inefficient
          }
        }
        j = whichin[nincl];
        notinclique[j] = TRUE; nincl = nincl-1 # drop last clique member
        #        cat ("\ndrop nincl,i,j",nincl,i,j)
      } # do while
      #      if (letbar[i]=="") # level i different from all others
      #      {nclique=nclique+1; letbar[i]=letters[nclique]}
      if (nclbar[i]==0) # level i different from all others
      {nclique=nclique+1; nclbar[j] = 1; whichcl[i,1] = nclique}
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
