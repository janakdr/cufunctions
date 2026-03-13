#' Reorder factor levels
#'
#' @param group Factor variable to reorder.
#' @param order Character vector specifying new level order, or NULL.
#' @return Factor with reordered levels.
#' @keywords internal
 cu_reorder = function(group, order) {
   # internal cu function to reorder factor variable
   group = as.factor(group); nameord=deparse(substitute(order))
   if (is.null(order)) return(group)
   nlev = nlevels(group); nord = length(order)
   if (is.na(nord)) stop(nameord,"must be a list of names.")
   if (nord != nlev) stop(nameord," has ",nord," names; should have ",nlev,sep="")
   for (i in 1:nlev) {
     if (!(levels(group)[i] %in% order)) stop(nameord," is missing ",levels(group)[i],sep="")
   }
   return(factor(group,levels=order))
 }
