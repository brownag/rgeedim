#' Composite an Image Collection
#'
#' Mosaic the images in a collection to support processing as a single image.
#'
#' @param x an object inheriting from `geedim.collection.MaskedCollection`, such as from `gd_search()` or `gd_collection_from_list()`
#'
#' @return a composite `geedim.MaskedImage` object
#' @export
gd_composite <- function(x) {
  y <- try(x$composite(), silent = TRUE)
  if (inherits(y, 'try-error')) {
    message(y[1])
    return(invisible(y))
  }
  y
}
