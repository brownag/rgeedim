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

  # skip header and delimiter, read date/time separately
  z <- read.table(text = y[!grepl("^---", y)][-1], header = FALSE)
  colnames(z) <- c("id", "date", "time", "fill")

  # recombine date as object
  z$date <- as.POSIXct(as.Date(paste(trimws(z$date), trimws(z$time))), tz = "UTC")
  z$time <- NULL

  return(z)
}
