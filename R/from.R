#' Reference Google Earth Engine Image or Image Collection by ID or Name
#'
#' Create references to Images or Image Collections based on IDs and names, or combine image references into collections.
#'
#' @param x character. ID.
#'
#' @return reference to `geedim.MaskedImage` or `geedimMaskedCollection` object, or `try-error` on error
#' @export
#' @rdname from
gd_image_from_id <- function(x) {
  y <- try(gd$MaskedImage$from_id(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @rdname from
gd_collection_from_name <- function(x) {
  y <- try(gd$MaskedCollection$from_name(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @rdname from
gd_collection_from_list <- function(x) {
  y <- try(gd$MaskedCollection$from_list(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}
