#' internal function to get list of the names in chstr separated by + *
#' @keywords internal
cu_parsexp = function(chstr)
{
  vnams = NULL
  while (chstr != "") {
    ipl = regexpr("\\+",chstr); imu = regexpr("\\*",chstr)
    # cat("\n",ipl,imu)
    iuse = ifelse(ipl<0 && imu<0, nchar(chstr)+1, ifelse(ipl*imu > 0, 
                  min(ipl,imu), ifelse(ipl > 0,ipl,imu)))
    vnam = gsub(" ","",substr(chstr,1,iuse-1))
    if (!any(vnams==vnam)) vnams = c(vnams,vnam)
    chstr = substring(chstr,iuse+1) # last token -> null even with blanks
    # cat(" '",vnam,"' '",chstr,"'",vnams,sep="")
  }
  return(vnams)
}
