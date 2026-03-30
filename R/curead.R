#' Does a read.table and returns a dataframe object
#' @param skip =lines to be skipped (default=NULL)
#' @param nrows =lines to be read after line with names (default is all)
#' @param maxfaclev if < levels, treated as char-string (default sqrt(n))
#' @param maxvarprint =20 (default), or N, with no print if #variables larger
#'
#'
#' @return dataframe object read
#' @examples
#' # Requires clipboard access
#' \dontrun{
#' HD = curead()
#' NEJM = curead()
#' }
#' @export
curead = function(skip=NULL, nrows=-1, maxfaclev=0, maxvarprint=20)
{
  cuf_apply_defaults(match.call(), environment())
  if (is.null(skip)) skip = 0
  if(Sys.info()[1]=="Windows") {
    obj = utils::read.table('clipboard', h=T, skip=skip, nrows=nrows, sep="\t", quote="", 
                     na.strings="NA", stringsAsFactors=F)
    clippipe = "'clipboard'"
  }
  else {
    obj = utils::read.table(pipe('pbpaste'), h=T, skip=skip, sep="\t", quote="", 
                     na.strings="NA", stringsAsFactors=F)
    clippipe = "pipe('pbpaste')"
  }
  obj[obj==""] <-NA  # treat blanks as NA ie missing
  nrobj = nrow(obj) # look for and trim blank lines at the end
  for (ir in nrobj:1) {if (!all(is.na(obj[ir,]))) break}
  if (ir <=1 ) stop ("\nNot even two observations. Copy again and try\n")
  if (ir < nrobj) {obj <- obj[-c((ir+1):nrobj),]; nrobj = ir}
  # cat ("\n",ir,nrobj,nrow(obj))
  if (maxfaclev<2) maxfaclev = max(2,round(sqrt(nrow(obj))))
  nvar = ncol(obj)
  for (i in (1:nvar)) {
    if (is.character(obj[,i])) if (length(unique(obj[,i])) <= maxfaclev)
      obj[,i] = factor(obj[,i])
  }
  cat ("\nread.table(",clippipe,", h=T, skip=",skip,
       ", sep=\"\\t\", quote=\"\", na.strings=\"NA\")\n",sep="")
  if (nvar <= maxvarprint) utils::str(obj)
  else cat ("\n",nrobj,"obs. of",nvar,"variables\n")
  #   dummystr = "To be able to work with your data: dsname = curead()"
  return(obj)
}
