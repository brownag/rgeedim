#' Reference Google Earth Engine Image or ImageCollection by ID
#'
#' Calls `geedim.mask.MaskedImage.from_id()` or `geedim.mask.MaskedCollection.from_id()`, respectively.
#'
#' @param x character. ID.
#'
#' @return an object referencing a Python `MaskedImage` or `MaskedCollection`, or `try-error` on error
#' @export
#' @rdname from_id
gd_image_from_id <- function(x) {
  y <- try(geedim_module$MaskedImage$from_id(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @rdname from_id
gd_collection_from_id <- function(x) {
  y <- try(geedim_module$MaskedCollection$from_id(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}
