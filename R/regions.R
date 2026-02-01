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
  
  if (length(.args) == 0 || is.null(.args[[1]])) {
    stop("Must specify a spatial object, an Earth Engine Feature Collection or Geometry, or the X and Y minimum/maxiumum values", call. = FALSE)
  }
  
  if (inherits(.args[[1]], "ee.featurecollection.FeatureCollection")) {
    res <- .args[[1]]$geometry()$bounds()$getInfo()
    if (is.null(res)) stop("Failed to get bounds from FeatureCollection (is it empty?)", call. = FALSE)
    return(res)
  }
  
  if (inherits(.args[[1]], "ee.geometry.Geometry")) {
    res <- .args[[1]]$bounds()$getInfo()
    if (is.null(res)) stop("Failed to get bounds from Geometry (is it empty?)", call. = FALSE)
    return(res)
  }

  if (inherits(.args[[1]], "ee.computedobject.ComputedObject") || inherits(.args[[1]], "python.builtin.object")) {
    # fallback for other EE objects (e.g. Image, Collection) or generic proxies
    # try geometry()
    g <- try(.args[[1]]$geometry(), silent = TRUE)
    if (!inherits(g, "try-error")) {
      res <- g$bounds()$getInfo()
      if (is.null(res)) stop("Failed to get bounds from Earth Engine object (is it empty?)", call. = FALSE)
      return(res)
    }
    # try bounds()
    b <- try(.args[[1]]$bounds(), silent = TRUE)
    if (!inherits(b, "try-error")) {
      res <- b$getInfo()
      if (is.null(res)) stop("Failed to get bounds from Earth Engine object (is it empty?)", call. = FALSE)
      return(res)
    }
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
#' @param x either a WKT string (character), a SpatRaster(Collection)/SpatVector(Collection)/SpatExtent, an sf object, an Spatial* object or a RasterLayer/RasterStack.
#' @seealso `gd_bbox()`
#' @return list representing a GeoJSON extent
#' @importFrom  methods as
#' @export
#' @examplesIf identical(Sys.getenv("R_RGEEDIM_RUN_EXAMPLES"), "TRUE") && gd_is_initialized() && !inherits(requireNamespace("terra", quietly=TRUE), 'try-error')
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
  if (inherits(x, "ee.featurecollection.FeatureCollection") || inherits(x, "python.builtin.object")) {
     try_geom <- try(x$geometry(), silent = TRUE)
     if (!inherits(try_geom, "try-error")) {
       x <- try_geom
     }
  }
  
  if (inherits(x, "ee.geometry.Geometry") || inherits(x, "python.builtin.object")) {
    info <- try(x$getInfo(), silent = TRUE)
    if (!inherits(info, "try-error") && is.list(info) && !is.null(info$type) && !is.null(info$coordinates)) {
      return(info)
    }
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
  
  if (!requireNamespace("yyjsonr", quietly = TRUE)) {
    stop("package 'yyjsonr' is required for GeoJSON conversion.", call. = FALSE)
  }
  
  # aggregate to single geometry (union)
  x <- terra::aggregate(x)
  
  # export to GeoJSON via temporary file
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  terra::writeVector(x, f, filetype = "GeoJSON", overwrite = TRUE)
  
  # read the GeoJSON string
  json_str <- readLines(f, warn = FALSE)
  json_str <- paste(json_str, collapse = "\n")
  
  # parse
  res <- yyjsonr::read_json_str(json_str)
  
  # return the geometry of the first feature
  # terra::writeVector creates a FeatureCollection
  geom <- NULL
  if (!is.null(res$features)) {
    if (is.data.frame(res$features)) {
      if (nrow(res$features) > 0) geom <- res$features$geometry[[1]]
    } else if (is.list(res$features)) {
      if (length(res$features) > 0) geom <- res$features[[1]]$geometry
    }
  }
  
  if (!is.null(geom)) {
    
    # helper to convert matrix coordinates (from yyjsonr) to list of lists
    # this ensures better compatibility with reticulate conversion to Python types
    mat_to_list <- function(x) {
      if (is.matrix(x) || is.array(x)) {
        lapply(seq_len(nrow(x)), function(i) as.numeric(x[i, ]))
      } else if (is.list(x)) {
        lapply(x, mat_to_list)
      } else {
        x
      }
    }
    geom$coordinates <- mat_to_list(geom$coordinates)
    
    # downgrade MultiPolygon to Polygon if it contains only one polygon
    if (geom$type == "MultiPolygon" && length(geom$coordinates) == 1) {
      geom$type <- "Polygon"
      geom$coordinates <- geom$coordinates[[1]]
    }
    
    return(geom)
  }
  
  stop("Failed to convert SpatVector to GeoJSON geometry.", call. = FALSE)
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

#' @description `gd_region_to_vect()` is the inverse function of gd_region/gd_bbox; convert GeoJSON-like list to Well-Known Text(WKT)/_SpatVector_. This may be useful, for example. when `gd_region()`-output was derived from an Earth Engine asset rather than local R object.
#' @details `gd_region_to_vect()` uses `yyjsonr` to parse the GeoJSON list and `terra` to create the spatial vector object. It supports all geometry types handled by `terra::vect` (e.g., Polygon, MultiPolygon).
#' @param crs character. Default for GeoJSON sources is `"OGC:CRS84"`.
#' @param as_wkt logical. Return Well-Known Text (WKT) string as character? Default: `FALSE` returns a 'terra' _SpatVector_.
#' @param ... Additional arguments to `gd_region_to_vect()` are passed to `terra::vect()` when `as_wkt=FALSE` (default).
#' @return `gd_region_to_vect()`: a 'terra' _SpatVector_ object, or _character_ containing Well-Known Text.
#' @export
#' @rdname gd_region
gd_region_to_vect <- function(x, crs = "OGC:CRS84", as_wkt = FALSE, ...) {
  
  if (!inherits(x, "list") ||
      (is.null(x$coordinates) && is.null(x$geometries)) ||
      is.null(x$type)) {
    stop("Expected a GeoJSON-like list containing 'coordinates' (or 'geometries') and 'type' elements.", call. = FALSE)
  }
  
  # recursive handling for GeometryCollection
  if (x$type == "GeometryCollection") {
    
    if (is.null(x$geometries)) stop("GeometryCollection missing 'geometries' member.", call. = FALSE)
    
    if (as_wkt) {
      res <- vapply(x$geometries, gd_region_to_vect, character(1), crs = crs, as_wkt = TRUE, ...)
      return(sprintf("GEOMETRYCOLLECTION(%s)", paste(res, collapse = ",")))
    } else {
      res <- lapply(x$geometries, gd_region_to_vect, crs = crs, as_wkt = FALSE, ...)
      
      if (length(res) == 0) {
        if (!requireNamespace("terra", quietly = TRUE)) {
          stop("package 'terra' is required for GeoJSON conversion.", call. = FALSE)
        }
        return(terra::vect(crs = crs))
      }
      
      # combine SpatVectors
      # This will fail if geometries are mixed types (e.g. Polygon and Point)
      # This behavior is consistent with terra::vect() not supporting mixed collections
      return(do.call(rbind, res))
    }
  }
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("package 'terra' is required for GeoJSON conversion.", call. = FALSE)
  }
  
  # use yyjsonr to serialize list to GeoJSON string
  json_str <- yyjsonr::write_json_str(x, opts = list(auto_unbox = TRUE))
  
  # use terra to parse GeoJSON string
  v <- terra::vect(json_str, crs = crs, ...)
  
  if (as_wkt) {
    # export to WKT via temporary CSV
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f), add = TRUE)
    terra::writeVector(v, f, filetype = "CSV", options = "GEOMETRY=AS_WKT")
    d <- utils::read.csv(f)
    return(d$WKT)
  }
  
  v
}
