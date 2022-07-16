# regions

#' Prepare `region` Object for `download()`
#'
#' Create a bounding box polygon Python object for use with `gd_download()`. The coordinates of the bounding box are expressed in WGS84 decimal degrees (`"OGC:CRS84"`).
#'
#' @details Expecting total of 4 bounding box arguments, If arguments are unnamed they should be in the following order: "xmin", "ymax", "xmax", "ymin".
#'
#' @param ... Either a single terra `SpatExtent` object or arguments: `xmin`/`ymax`/`xmax`/`ymin`. If the four bounding arguments are not named they should be in order.
#' @return a _list_ object describing a GeoJSON bounding rectangular polygon suitable for use as `regions` argument to `gd_download()` or `gd_search()`
#' @export
#' @examples
#' gd_bbox(
#'   xmin = 5.744140,
#'   ymax = 50.18162,
#'   xmax = 6.528252,
#'   ymin = 49.44781
#' )
gd_bbox <- function(...) {
  .gdal_projwin <- c("xmin", "ymax", "xmax", "ymin")
  args <- list(...)
  # TODO: bbox around all args for multiple SpatExtent?
  if (inherits(args[[1]], 'SpatExtent')) {
    m <- do.call('cbind', args[[1]]@ptr$as.points())
  } else if (all(.gdal_projwin %in% names(args))) {
    m <- matrix(c(args[["xmin"]], args[["ymin"]],
                  args[["xmin"]], args[["ymax"]],
                  args[["xmax"]], args[["ymax"]],
                  args[["xmax"]], args[["ymin"]]),
                ncol = 2, byrow = TRUE)
  } else {
    if (!length(args) == 4)
      stop('Expecting total of 4 bounding box arguments, If arguments are unnamed they should be in the following order: "xmin", "ymax", "xmax", "ymin".')
    names(args) <- .gdal_projwin
  }
  m <- rbind(m, m[1,])
  m <- m[rev(1:nrow(m)),]
  list(type = "Polygon",
       coordinates = list(apply(m, 1, c, simplify = FALSE)))
}


#' Create `region` Object from R Spatial Objects
#'
#' Creates a suitable input for the `region` argument to `gd_download(<Image>)` or `gd_search()` for Image Collections.
#'
#' @param x either a WKT string (character), a {terra} SpatRaster/SpatVector, an {sf} object, a {sp} Spatial* object or a {raster} RasterLayer/Stack.
#' @seealso `gd_bbox()`
#' @return am R list representing a bounding box extent in GeoJSON
#' @importFrom  methods as
#' @export
gd_region <- function(x) {

  if (is.list(x) &&
      !is.null(x$type) &&
      !is.null(x$coordinates)) {
    # short circuit, if already a suitable list object don't require any namespaces or do any conversion
    return(x)
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("package `terra` is required to convert R spatial objects to GeoJSON region bounding boxes.\n
         See `gd_bbox()` for a simpler region interface that takes numeric values (xmin/xmax/ymin/ymax) directly.", .call = FALSE)
  }

  # wkt string
  if (is.character(x)) {
    x <- terra::vect(x, crs = "OGC:CRS84")
  }

  # raster/sp support
  if (inherits(x, 'Spatial')) {
    if (requireNamespace('raster', quietly = TRUE)) {
      x <- terra::vect(as(x, 'Spatial'))
    }
  }

  if (inherits(x, 'RasterLayer') || inherits(x, 'RasterStack')) {
    if (requireNamespace('raster', quietly = TRUE)) {
      x <- terra::rast(x)
    }
  }

  # sf objects
  if (inherits(x, c('sf', 'sfc'))) {
    x <- terra::vect(x)
  }

  # terra
  if (inherits(x, c('SpatVector','SpatRaster','SpatVectorCollection', 'SpatRasterCollection'))) {

    # if it is projected we _know_ we cant interpret the bounds as being in or near WGS84
    # but this will not catch all CRS deviations
    # if (!terra::is.lonlat(x)) {

      # so, instead, we project everything. will fail if CRS in x not defined.
      x <- terra::project(x, "OGC:CRS84")

    # }

    x <- terra::ext(x)
  }
  gd_bbox(x)
}
