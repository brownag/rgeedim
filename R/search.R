# search collection

#' Search an Image Collection
#'
#' @param x `geedim.collection.MaskedCollection` object
#' @param region list / Python GeoJSON object describing region, e.g. as created by `gd_bbox()`
#' @param start_date Default: `'2020-01-01'`
#' @param end_date Default: `Sys.Date()`
#' @param ... additional arguments to `geedim.MaskedCollection.search()` e.g. `cloudless_portion`, `fill_portion`
#' @return `geedim.MaskedCollection` object suitable for querying properties
#' @export
#' @examplesIf gd_is_initialized() && !inherits(requireNamespace("terra", quietly=TRUE), 'try-error')
#' \donttest{
#' b <- terra::vect('POLYGON((-121.355 37.56,-121.355 37.555,
#'                     -121.35 37.555,-121.35 37.56,
#'                     -121.355 37.56))',
#'           crs = "OGC:CRS84")
#' if (gd_is_initialized())
#'   gd_search(gd_collection_from_name("USGS/3DEP/1m"),
#'             region = gd_region(b))
#' }
gd_search <- function(x, region, start_date = '2000-01-01', end_date = as.character(Sys.Date()), ...) {
  y <- try(x$search(start_date = start_date, end_date = end_date, region = gd_region(region),  ...), silent = TRUE)
  if (inherits(y, "try-error")) {
    message(y[1])
    return(invisible(y))
  }
  y
}


#' Get Properties of an Image Collection
#'
#' @param x `geedim.collection.MaskedCollection` object
#' @return `data.frame` containing properties table from `x`; `NULL` if no properties table.
#' @importFrom utils read.table
#' @export
#' @examplesIf gd_is_initialized() && !inherits(requireNamespace("terra", quietly=TRUE), 'try-error')
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
#' if (gd_is_initialized()) {
#'   x <- gd_search(gd_collection_from_name("USGS/3DEP/1m"),
#'                  region = gd_region(b))
#'   gd_properties(x)
#' }
#' }
gd_properties <- function(x) {
  pt <- try(x$properties_table)
  if (inherits(pt, 'try-error')) {
    message(pt[1])
    return(invisible(pt))
  }
  if (pt == "") {
    return(NULL)
  }
  y <- strsplit(pt, "\n")[[1]]

  if (length(y) == 1 && y == "") {
    return(NULL)
  }

  # TODO: are there ever multiple tables? look at some more complicated examples.
  h <- as.character(read.table(text = y[1], header = FALSE)[1,])[-(1:2)]

  # skip header and delimiter, read date/time separately
  z <- read.table(text = y[!grepl("^---", y)][-1], header = FALSE)
  colnames(z) <- c("id", "date", "time", tolower(h))

  # recombine date as object
  z$date <- as.POSIXct(as.Date(paste(trimws(z$date), trimws(z$time))), tz = "UTC")
  z$time <- NULL

  return(z)
}

#' Get Names of Layers in an Earth Engine Image
#'
#' Calls `bandNames()` method from `ee.Image` class.
#'
#' @param x a Google Earth Engine Image object, such as from `gd_image_from_id()`
#'
#' @return character. Vector of names of each layer in an image.
#' @export
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_band_names(gd_image_from_id("USGS/NED"))
#' }
gd_band_names <- function(x) {
  y <- NULL
  if (inherits(x, 'geedim.download.BaseImage')) {
    y <- try(x$ee_image$bandNames()$getInfo(), silent = TRUE)
  } else stop("`x` should inherit from geedim.download.BaseImage", call. = FALSE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  y
}

#' Get Properties of Layers in an Earth Engine Image
#'
#' Gets combined Earth Engine and STAC properties.
#'
#' @param x a Google Earth Engine Image object, such as from `gd_image_from_id()`
#'
#' @return list. Each element is a list that corresponds to a layer in `x`, each with one or more elements for properties of that layer.
#' @export
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_band_properties(gd_image_from_id("USGS/NED"))
#' }
gd_band_properties <- function(x) {
  y <- NULL
  if (inherits(x, 'geedim.download.BaseImage')) {
    y <- try(x$band_properties, silent = TRUE)
  } else stop("`x` should inherit from geedim.download.BaseImage", call. = FALSE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  n <- sapply(y, function(z) z[["name"]])
  names(y) <- n
  y
}

#' Get Footprint of Masked Image
#'
#' Gets GeoJSON-style list containing footprint of a `geedim.mask.MaskedImage` object
#'
#' @param x a `geedim.mask.MaskedImage` object
#' @return list.
#' @export
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_footprint(gd_image_from_id("USGS/NED"))
#' }
gd_footprint <- function(x) {
  y <- NULL
  if (inherits(x, 'geedim.mask.MaskedImage')) {
    y <- try(x$footprint, silent = TRUE)
  } else stop("`x` should inherit from geedim.mask.MaskedImage", call. = FALSE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  y
}
