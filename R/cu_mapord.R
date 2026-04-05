#' internal cufunction used if bars are reordered with "order=" (defunct)
#' @keywords internal
cu_mapord = function(order,levnams,nlev) {
  mapord = rep(0,nlev); locord = rep(0,nlev)
  if (!is.null(order)) {
    if (length(order) != nlev) {
      message("order must list ",nlev," names"); order=NULL
    }
    else {
      for (i in 1:nlev) {
        loco = match(levnams[i],order)
        if (is.na(loco)) {
          message("order is missing ",levnams[i]); break
        }
        if (locord[loco] > 0) {
          message(levnams[i]," appears more than once"); break
        }
        mapord[i] = loco; locord[loco] = i
      }
    }
  }
  if (is.null(order)) for (i in 1:nlev) {mapord[i] = i}
  # print(mapord)
  return(mapord)
}
