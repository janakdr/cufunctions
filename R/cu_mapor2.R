#' internal cufunction used if bars are reordered with "order="
#' @keywords internal
cu_mapor2 = function(order,levnams,gp1,gp2,nlev) {
  mapord = rep(0,nlev); locord = rep(0,gp1)
  if (!is.null(order)) {
    if (length(order) != gp1) {
      warning("order must list ",gp1," names", call. = FALSE)
      order=NULL  # has only local effect
    }
    else {
      for (i in 1:gp1) {
        loco = match(levnams[i],order)
        if (is.na(loco)) {
          warning("order is missing ",levnams[i], call. = FALSE); break
        }
        if (locord[loco] > 0) {
          warning(levnams[i]," appears more than once", call. = FALSE); break
        }
        locord[loco] = i; ib = (i-1)*gp2;
        for (j in 1:gp2) mapord[j+ib] = j+(loco-1)*gp2;
      }
    }
  }
  if (is.null(order)) for (i in 1:nlev) {mapord[i] = i}
  # print(mapord)
  return(mapord)
}
