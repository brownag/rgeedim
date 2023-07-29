# regions

#' Prepare Bounding Box Region from X/Y Limits
#'
#' Create a bounding box polygon Python object for use with `gd_download()`. The coordinates of the bounding box are expressed in WGS84 decimal degrees (`"OGC:CRS84"`).
#'
#' @details Expecting total of 4 bounding box arguments, If arguments are unnamed they should be in the following order: "xmin", "ymax", "xmax", "ymin".
#'
#' @param ... One or more `SpatRaster`, `SpatRasterCollection`, `SpatVector`, `SpatVectorProxy` or `SpatExtent` objects (whose combined bounding box extent will be returned); or the following _named_ numeric arguments: `xmin`/`ymax`/`xmax`/`ymin`. If these four limit arguments are not named they should be in the stated order.
#' 
#' @return a _list_ object describing a GeoJSON bounding rectangular polygon suitable for use as `regions` argument to `gd_download()` or `gd_search()`
#' 
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
  
  if (length(.args) == 0) {
    stop("Must specify a spatial object, an Earth Engine Feature Collection or Geometry, or the X and Y minimum/maxiumum values", call. = FALSE)
  }
  
  if (inherits(.args[[1]], "ee.featurecollection.FeatureCollection")) {
    return(.args[[1]]$geometry()$bounds()$getInfo())
  }
  
  if (inherits(.args[[1]], "ee.geometry.Geometry")) {
    return(.args[[1]]$bounds()$getInfo())
  }

  .mbbox <- function(x) {
    matrix(c(x[["xmin"]], x[["ymin"]],
             x[["xmin"]], x[["ymax"]],
             x[["xmax"]], x[["ymax"]],
             x[["xmax"]], x[["ymin"]]),
           ncol = 2, byrow = TRUE)
  }
  
  # if ... has an ext() method we will use it
  tri <- vapply(.args, inherits, logical(1), c(
    "SpatRaster",
    "SpatRasterCollection",
    "SpatVector",
    "SpatVectorProxy",
    "SpatExtent"
  ))
  
  if (all(tri)) {
    
    stopifnot(requireNamespace("terra", quietly = TRUE))
    
    
    # extend extent for each terra object in ...
    if (!inherits(.args[[1]], 'SpatExtent')) {
      # convert non-terra to terra
      out <- .cast_spatial_object(.args[[1]], extent = TRUE)
    } else {
      out <- .args[[1]]
    }
    
    if (length(.args) > 1) {
      for (a in .args[-1]) {
        if (!inherits(a, 'SpatExtent')) {
          a <- terra::ext(a)
        }
        out <- terra::union(out, a)
      }
    }
    m <- terra::crds(terra::as.points(out))
      
  } else if (all(.gdal_projwin %in% names(.args))) {
    m <- .mbbox(.args)
  } else {
    if (!length(.args) == 4) {
      stop('Expecting total of 4 numeric bounding box arguments.\nIf arguments are unnamed they should be in the following order: "xmin", "ymax", "xmax", "ymin".', call. = FALSE)
    }
    names(.args) <- .gdal_projwin
    m <- .mbbox(.args)
  }
  m <- rbind(m, m[1,])
  m <- m[rev(1:nrow(m)),]
  list(type = "Polygon", coordinates = list(lapply(apply(m, 1, function(x) {
    list(as.numeric(x))
  }), .subset2, 1)))
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
#' @examplesIf !inherits(requireNamespace("terra", quietly=TRUE), 'try-error')
#' @examples
#' \donttest{
#' library(terra)
#' 
#' b <- terra::vect('POLYGON((-121.355 37.560, 
#'                            -121.355 37.555,
#'                            -121.350 37.555, 
#'                            -121.350 37.560,
#'                            -121.355 37.560))',
#'                  crs = "OGC:CRS84")
#'
#' gd_region(b)
#' }
gd_region <- function(x) {
  if (inherits(x, "ee.featurecollection.FeatureCollection")) {
     x <- x$geometry()
  }
  
  if (inherits(x, "ee.geometry.Geometry")) {
    return(x$getInfo())
  }

  if (is.list(x) &&
      !is.null(x$type) &&
      !is.null(x$coordinates))  {
    # short circuit, if already a suitable list object don't require any namespaces or do any conversion
    
    return(x)
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("package `terra` is required to convert R spatial objects to GeoJSON regions.\n
         See `gd_bbox()` for a simpler region interface that takes numeric values (xmin/xmax/ymin/ymax) directly.", .call = FALSE)
  }

  # convert non-terra to terra
  x <- .cast_spatial_object(x)
  
  # terra
  if (inherits(x, c('SpatVector',
                    'SpatRaster',
                    'SpatVectorCollection',
                    'SpatRasterCollection'))) {
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

#' Cast Spatial Object to SpatVector or SpatRaster
#' 
#' This internal function allows for consistent interfaces for non-terra Spatial objects by coercion to the terra native objects `SpatVector` or `SpatRaster`.
#' 
#' @param x A WKT string, Spatial*, Raster*, or sf* object
#' @param extent Return only SpatExtent of result? Default: `FALSE`
#' @details WKT string coordinates should use the longitude latitude WGS84 decimal degrees (`"OGC:CRS84"` spatial reference system).
#' @noRd
.cast_spatial_object <- function(x, extent = FALSE) {
  
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
  
  if (inherits(x, c('RasterLayer', 'RasterStack'))) {
    if (requireNamespace('raster', quietly = TRUE)) {
      x <- terra::rast(x)
    }
  }
  
  # sf and sfc objects
  if (inherits(x, c('sf', 'sfc'))) {
    x <- terra::vect(x)
  }
  
  # convert to simple geometries if we only want extent
  if (extent && !inherits(x, 'SpatVector')) {
    x <- terra::as.polygons(x, extent = TRUE)
  }
  
  # project what we can to OGC:CRS84
  if (inherits(x, 'SpatVector')) {
    # will fail if CRS in x not defined
    x <- try(terra::project(x, "OGC:CRS84"), silent = TRUE)
    if (inherits(x, 'try-error')) {
      stop(x[1], call. = FALSE)
    }
  }
  
  # raster Extent, sf bbox, extent=TRUE
  # assume these are already in correct CRS
  if (inherits(x, c('Extent', 'bbox')) || 
      (extent && !inherits(x, 'SpatExtent'))) {
    x <- terra::ext(x)
  }
  
  # return object (possibly unchanged)
  x
}
