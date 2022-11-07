# regions

#' Prepare Bounding Box Region from X/Y Limits
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
  .args <- list(...)

  .mbbox <- function(x) {
    matrix(c(x[["xmin"]], x[["ymin"]],
             x[["xmin"]], x[["ymax"]],
             x[["xmax"]], x[["ymax"]],
             x[["xmax"]], x[["ymin"]]),
           ncol = 2, byrow = TRUE)
  }
  # TODO: bbox around all args for multiple SpatExtent?
  if (inherits(.args[[1]], 'SpatExtent')) {
    stopifnot(requireNamespace("terra", quietly = TRUE))
    m <- do.call('cbind', .args[[1]]@ptr$as.points())
  } else if (all(.gdal_projwin %in% names(.args))) {
    m <- .mbbox(.args)
  } else {
    if (!length(.args) == 4) {
      stop('Expecting total of 4 bounding box arguments, If arguments are unnamed they should be in the following order: "xmin", "ymax", "xmax", "ymin".', call. = FALSE)
    }
    names(.args) <- .gdal_projwin
    m <- .mbbox(.args)
  }
  m <- rbind(m, m[1,])
  m <- m[rev(1:nrow(m)),]
  list(type = "Polygon", coordinates = list(apply(m, 1, c, simplify = FALSE)))
}


#' Create GeoJSON Region from R Spatial Objects
#'
#' Creates a suitable input for the `region` argument to `gd_download(<Image>)` or `gd_search()` for Image Collections.
#'
#' If `x` is an R spatial object, each vertex (possibly after converting object extent to vector) is used to create the GeoJSON object. Otherwise, the extent is determined and passed to `gd_bbox()`.
#'
#' @param x either a WKT string (character), a {terra} SpatRaster(Collection)/SpatVector(Collection)/SpatExtent, an {sf} object, an {sp} Spatial* object or a {raster} RasterLayer/RasterStack.
#' @seealso `gd_bbox()`
#' @return list representing a GeoJSON extent
#' @importFrom  methods as
#' @export
#' @examplesIf requireNamespace("terra")
#' @examples
#' b <- terra::vect('POLYGON((-121.355 37.56,-121.355 37.555,
#'                     -121.35 37.555,-121.35 37.56,
#'                     -121.355 37.56))',
#'           crs = "OGC:CRS84")
#' gd_region(b)
gd_region <- function(x) {

  if (is.list(x) &&
      !is.null(x$type) &&
      !is.null(x$coordinates)) {
    # short circuit, if already a suitable list object don't require any namespaces or do any conversion
    return(x)
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("package `terra` is required to convert R spatial objects to GeoJSON regions.\n
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
  if (inherits(x, c('SpatVector',
                    'SpatRaster',
                    'SpatVectorCollection',
                    'SpatRasterCollection'))) {
    if (!inherits(x, 'SpatVector')) {
      x <- terra::as.polygons(x, extent = TRUE)
      # project everything. will fail if CRS in x not defined.
      x <- terra::project(x, "OGC:CRS84")

    }
    return(.gd_geojson(x))
  }
  gd_bbox(x)
}

.gd_geojson <- function(x) {
  # x is a terra vector object
  if (inherits(x, 'SpatVectorProxy')) {
    x <- terra::vect(terra::sources(x))
  } else if (!inherits(x, 'SpatVector')) {
    stop("`x` must be a SpatVector", call. = FALSE)
  }
  p <- terra::crds(terra::as.points(x))
  p <- p[rev(seq_len(nrow(p))), ]
  p <- rbind(p[nrow(p), ], p)
  list(type = "Polygon", coordinates = list(lapply(apply(p, 1, function(x) {
    list(as.numeric(x))
  }), .subset2, 1)))
}
