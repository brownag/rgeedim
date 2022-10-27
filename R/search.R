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
gd_search <- function(x, region, start_date = '2000-01-01', end_date = as.character(Sys.Date()), ...) {
  y <- try(x$search(start_date = start_date, end_date = end_date, region = gd_region(region),  ...), silent = TRUE)
  if (inherits(y, "try-error")){
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
gd_band_properties <- function(x) {
  y <- NULL
  if (inherits(x, 'geedim.download.BaseImage')) {
    y <- try(x$band_properties, silent = TRUE)
  } else stop("`x` should inherit from geedim.download.BaseImage", call. = FALSE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  n <- sapply(y, \(z) z[["name"]])
  names(y) <- n
  y
}
