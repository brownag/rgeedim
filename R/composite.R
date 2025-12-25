#' Composite an Image Collection
#'
#' Create a composite image from elements of an image collection.
#'
#' @param x an object inheriting from `geedim.collection.ImageCollectionAccessor` (for geedim >= 2.0.0) or `geedim.collection.MaskedCollection` (for geedim < 2.0.0), such as from `gd_search()` or `gd_collection_from_list()`. See `\link{geedim-versions}` for more details.
#' @param ... [additional arguments](https://geedim.readthedocs.io/en/stable/reference/api.html#geedim.collection.ImageCollectionAccessor.composite) to `geedim.collection.ImageCollectionAccessor$composite()`
#' @return a composite `ee.image.Image` object
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
  if (!inherits(x, c("geedim.collection.ImageCollectionAccessor",
                      "geedim.collection.MaskedCollection"))) {
    stop("`x` should be a geedim.collection.ImageCollectionAccessor or geedim.collection.MaskedCollection", call. = FALSE)
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
  if (inherits(y, "ee.image.Image")) {
    return(y$gd)
  }
  y
}
