#' Composite an Image Collection
#'
#' Create a composite image from elements of an image collection.
#'
#' @param x an object inheriting from `geedim.collection.MaskedCollection`, such as from `gd_search()` or `gd_collection_from_list()`
#' @param ... [additional arguments](https://geedim.readthedocs.io/en/latest/_generated/geedim.collection.MaskedCollection.composite.html) to `geedim.collection.MaskedCollection$composite()`
#' @return a composite `geedim.mask.MaskedImage` object
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
#' if (gd_is_initialized())
#'   gd_composite(gd_search(gd_collection_from_name("USGS/3DEP/1m"),
#'                          region = b),
#'                resampling = "bilinear")
#' }
gd_composite <- function(x, ...) {
  if (!inherits(x, 'geedim.collection.MaskedCollection')) {
    stop("`x` should be a geedim.collection.MaskedCollection", call. = FALSE)
  }
  args <- list(...)
  if (!is.null(args$region)) {
    args$region <- gd_region(args$region)
  }
  y <- try(do.call(x$composite, args), silent = TRUE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  y
}
