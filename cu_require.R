#' Loads all packages needed by cufunctions (unused since package)
#' @param call with no arguments
#' @return returns nothing
#' @export
cu_require = function()
{
  packmiss = ""
  if (!require(Hmisc)) packmiss = paste(packmiss,"Hmisc")
  if (!require(e1071)) packmiss = paste(packmiss,"e1071")
  if (!require(survival)) packmiss = paste(packmiss,"survival")
  if (!require(gmodels)) packmiss = paste(packmiss,"gmodels")
  if (!require(nlme)) packmiss = paste(packmiss,"nlme")
  if (!require(ROCR)) packmiss = paste(packmiss,"ROCR")
  if (!require(dunn.test)) packmiss = paste(packmiss,"dunn.test")
  if (!require(ggplot2)) packmiss = paste(packmiss,"ggplot2")
  if (!require(MuMIn)) packmiss = paste(packmiss,"MuMIn")
  if (!require(MatchIt)) packmiss = paste(packmiss,"MatchIt")
  if (!require(survminer)) packmiss = paste(packmiss,"survminer")
  if (!require(MASS)) packmiss = paste(packmiss,"MASS")
  if(Sys.info()[1]!="Windows") if (!require(devEMF)) packmiss = paste(packmiss,"devEMF")
  if (packmiss == "") {
    cat("\nAll required packages have been loaded.\n(curequire v. Feb 2020  Steve Holleran)\n You can ignore the many warning messages.\n")
  }
  # qn: Why does "else" here trigger error msg "unexpected 'else'  "?
  if (packmiss != "") {
    warning("Some package(s) did not get loaded. You may need to reinstall\n",packmiss,"\n")
    cat("In lower right window,\n click Packages/Install-from-Repository and type package name,\n check off Install Dependencies\n")
    cat("Do this for each package under 'need to reinstall'\n(curequire v. Feb 2020  Steve Holleran)\n")
  }
  cat("\n")
}
