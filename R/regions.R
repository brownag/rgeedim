# regions

#' Prepare `region` Object for `download()`
#'
#' Create a bounding box polygon Python object for use with `download()`. The coordinates of the bounding box are expressed in WGS84 decimal degrees (`"OGC:CRS84"`).
#' @param ... either a single terra `SpatExtent` object or _named_ arguments `xmin`/`ymin`/`xmax`/`ymax`
#' @return a _list_ object describing a GeoJSON bounding rectangular polygon suitable for use as `regions` argument to `download()`
#' @export
#' @examples
#' gd_bbox(
#'   xmin = 5.744140,
#'   xmax = 6.528252,
#'   ymin = 49.44781,
#'   ymax = 50.18162
#' )
gd_bbox <- function(...) {
  args <- list(...)
  # TODO: bbox around all args for multiple SpatExtent?
  if (inherits(args[[1]], 'SpatExtent')) {
    m <- do.call('cbind', args[[1]]@ptr$as.points())
  } else {
    m <- matrix(c(args[["xmin"]], args[["ymin"]],
                  args[["xmin"]], args[["ymax"]],
                  args[["xmax"]], args[["ymax"]],
                  args[["xmax"]], args[["ymin"]]),
                ncol = 2, byrow = TRUE)
  }
  m <- rbind(m, m[1,])
  m <- m[rev(1:nrow(m)),]
  list(type = "Polygon",
       coordinates = list(apply(m, 1, c, simplify = FALSE)))
}
