# search collection

#' Search an Image Collection
#'
#' @param x `geedim.MaskedCollection` object
#' @param region list / Python GeoJSON object describing region, e.g. as created by `gd_bbox()`
#' @param start_date Default: `'2020-01-01'`
#' @param end_date Default: `Sys.Date()`
#' @param ... additional arguments to `geedim.MaskedCollection.search()` e.g. `cloudless_portion`, `fill_portion`
#' @return `geedim.MaskedCollection` object suitable for querying properties
#' @export
gd_search <- function(x, region, start_date = '2020-01-01', end_date = as.character(Sys.Date()), ...) {
  x$search(start_date = start_date, end_date = end_date, region = gd_region(region),  ...)
}


#' Get Properties of an Image Collection
#'
#' @param x `geedim.MaskedCollection` object
#'
#' @return `data.frame` containing properties table from `x`
#' @importFrom utils read.table
#' @export
gd_properties <- function(x) {
  y <- strsplit(x$properties_table, "\n")[[1]]

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
gd_bandnames <- function(x) {
  if (inherits(x, 'geedim.download.BaseImage')) {
    x$ee_image$bandNames()$getInfo()
  } else stop("`x` should inherit from geedim.download.BaseImage", call. = FALSE)
}