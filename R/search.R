# search collection

#' Search an Image Collection
#'
#' @param x A `geedim.collection.ImageCollectionAccessor` (for geedim >= 2.0.0) or `geedim.MaskedCollection` (for geedim < 2.0.0) object. See `\link{geedim-versions}` for more details.
#' @param region list / Python GeoJSON object describing region, e.g. as created by `gd_bbox()`
#' @param start_date Default: `'2020-01-01'`
#' @param end_date Default: `Sys.Date()`
#' @param ... additional arguments to `geedim.MaskedCollection.search()` e.g. `cloudless_portion`, `fill_portion`
#' @return A `geedim.collection.ImageCollectionAccessor` (for geedim >= 2.0.0) or `geedim.MaskedCollection` (for geedim < 2.0.0) object suitable for querying properties. See `\link{geedim-versions}` for more details.
#' @export
#' @examplesIf isTRUE(as.logical(Sys.getenv("R_RGEEDIM_RUN_EXAMPLES"))) && gd_is_initialized() && !inherits(requireNamespace("terra", quietly=TRUE), 'try-error')
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
  FUN <- NULL
  if (inherits(x, 'geedim.collection.ImageCollectionAccessor')) {
    FUN <- x$filter
    y <- try(FUN(start_date = start_date, end_date = end_date, region = earthengine()$Geometry(gd_region(region)),  ...), silent = TRUE)
  } else {
    FUN <- x$search
    y <- try(FUN(start_date = start_date, end_date = end_date, region = gd_region(region),  ...), silent = TRUE)
  }
  if (inherits(y, "try-error")) {
    message(y[1])
    return(invisible(y))
  }
  if (inherits(x, 'geedim.collection.ImageCollectionAccessor')) {
    return(y$gd)
  }
  y
}


#' Get Properties of an Image Collection
#'
#' @param x A `geedim.collection.ImageCollectionAccessor` (for geedim >= 2.0.0) or `geedim.collection.MaskedCollection` (for geedim < 2.0.0) object. See `\link{geedim-versions}` for more details.
#' @return `data.frame` containing properties table from `x`; `NULL` if no properties table.
#' @importFrom utils read.table
#' @export
#' @examplesIf isTRUE(as.logical(Sys.getenv("R_RGEEDIM_RUN_EXAMPLES"))) && gd_is_initialized() && !inherits(requireNamespace("terra", quietly=TRUE), 'try-error')
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
  
  pt <- NULL
  if (inherits(x, 'geedim.collection.ImageCollectionAccessor')) {
    pt <- try(x$propertiesTable, silent = TRUE)
  } else if (inherits(x, c("geedim.collection.MaskedCollection",
                           "geedim.download.BaseImage"))) {
    pt <- try(x$properties_table, silent = TRUE)
  } else stop("`x` should inherit from geedim.download.BaseImage", call. = FALSE)
  
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
  z <- read.table(text = y[!grepl("^---", y)][-1], header = FALSE, stringsAsFactors = FALSE)
  z[, 1] <- as.character(z[, 1])
  
  if (ncol(z) >= 3) {
    colnames(z) <- c("id", "date", "time", tolower(h))
    z$date <- as.POSIXct(as.Date(paste(trimws(z$date), trimws(z$time))), tz = "UTC")
    z$time <- NULL
  } else if (ncol(z) == 2) {
    colnames(z) <- c("id", "date")
  } else if (ncol(z) == 1) {
    colnames(z) <- "id"
  }

  if (inherits(x, 'geedim.collection.ImageCollectionAccessor')) {
    cid <- try(x$id, silent = TRUE)
    if (!inherits(cid, "try-error") && !is.null(cid) && cid != "") {
      prefix <- paste0(cid, "/")
      if (nrow(z) > 0 && !startsWith(z$id[1], prefix)) {
        z$id <- paste0(prefix, z$id)
      }
    }
  }

  return(z)
}

#' Get Names of Layers in an Earth Engine Image
#'
#' Calls `bandNames()` method from `ee.Image` class.
#'
#' @param x a `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.download.BaseImage` (for geedim < 2.0.0) object, such as from `gd_image_from_id()`. See `\link{geedim-versions}` for more details.
#'
#' @return character. Vector of names of each layer in an image.
#' @export
#' @examplesIf isTRUE(as.logical(Sys.getenv("R_RGEEDIM_RUN_EXAMPLES"))) && gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_band_names(gd_image_from_id("USGS/SRTMGL1_003"))
#' }
gd_band_names <- function(x) {
  y <- NULL
  if (inherits(x, 'geedim.image.ImageAccessor')) {
    y <- try(x$bandNames, silent = TRUE)
  } else if (inherits(x, 'geedim.download.BaseImage')) {
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
#' @param x a `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.download.BaseImage` (for geedim < 2.0.0) object, such as from `gd_image_from_id()`. See `\link{geedim-versions}` for more details.
#'
#' @return list. Each element is a list that corresponds to a layer in `x`, each with one or more elements for properties of that layer.
#' @export
#' @examplesIf isTRUE(as.logical(Sys.getenv("R_RGEEDIM_RUN_EXAMPLES"))) && gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_band_properties(gd_image_from_id("USGS/SRTMGL1_003"))
#' }
gd_band_properties <- function(x) {
  y <- NULL
  if (inherits(x, 'geedim.image.ImageAccessor')) {
    y <- try(x$bandProps, silent = TRUE)
  } else if (inherits(x, 'geedim.download.BaseImage')) {
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
#' Gets GeoJSON-style list containing footprint of a `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.mask.MaskedImage` (for geedim < 2.0.0) object. See `\link{geedim-versions}` for more details.
#'
#' @param x a `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.mask.MaskedImage` (for geedim < 2.0.0) object. See `\link{geedim-versions}` for more details.
#' @return list.
#' @export
#' @examplesIf isTRUE(as.logical(Sys.getenv("R_RGEEDIM_RUN_EXAMPLES"))) && gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_footprint(gd_image_from_id("USGS/SRTMGL1_003"))
#' }
gd_footprint <- function(x) {
  y <- NULL
  if (inherits(x, 'geedim.image.ImageAccessor')) {
    y <- try(x$geometry, silent = TRUE)
  } else if (inherits(x, 'geedim.mask.MaskedImage')) {
    y <- try(x$footprint, silent = TRUE)
  } else stop("`x` should inherit from geedim.image.ImageAccessor (or geedim.mask.MaskedImage with geedim < 2.0.0)", call. = FALSE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  y
}
