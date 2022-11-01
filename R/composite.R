#' Composite an Image Collection
#'
#' Create a composite image from elements of an image collection.
#'
#' @param x an object inheriting from `geedim.collection.MaskedCollection`, such as from `gd_search()` or `gd_collection_from_list()`
#' @param ... [additional arguments](https://geedim.readthedocs.io/en/latest/_generated/geedim.collection.MaskedCollection.composite.html) to `geedim.collection.MaskedCollection$composite()`
#' @return a composite `geedim.mask.MaskedImage` object
#' @export
#' @examplesIf gd_is_initialized() && requireNamespace("terra")
#' @examples
#' b <- terra::vect('POLYGON((-121.355 37.56,-121.355 37.555,
#'                     -121.35 37.555,-121.35 37.56,
#'                     -121.355 37.56))',
#'           crs = "OGC:CRS84")
#'
#' if (gd_is_initialized())
#'   gd_composite(gd_search(gd_collection_from_name("USGS/3DEP/1m"),
#'                          region = gd_region(b)),
#'                resampling = "bilinear")
gd_composite <- function(x, ...) {
  if (!inherits(x, 'geedim.collection.MaskedCollection')) {
    stop("`x` should be a geedim.collection.MaskedCollection", call. = FALSE)
  }
  y <- try(x$composite(...), silent = TRUE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  y
}
