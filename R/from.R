#' Reference Google Earth Engine Image or Image Collection by ID or Name
#'
#' Create references to a Google Earth Engine Image or Image Collection based on IDs or names, or combine Images into Image Collections.
#'
#' @param x character. `id` of Image, `name` of Image Collection, or a vector of Image `id` to create a new Image Collection
#'
#' @return `geedim.MaskedImage` or `geedim.MaskedCollection` object, or `try-error` on error
#'
#' @export
#' @rdname from
#' @examplesIf length(geedim()) > 0 && !inherits(gd_initialize(), "try-error")
#' \donttest{
#' gd_initialize()
#' gd_image_from_id('CSP/ERGo/1_0/Global/SRTM_topoDiversity')
#' }
gd_image_from_id <- function(x) {
  y <- try(gd$MaskedImage$from_id(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @rdname from
#' @examplesIf length(geedim()) > 0 && !inherits(gd_initialize(), "try-error")
#' \donttest{
#' gd_initialize()
#' gd_collection_from_name("USGS/3DEP/1m")
#' }
gd_collection_from_name <- function(x) {
  y <- try(gd$MaskedCollection$from_name(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @rdname from
#' @examplesIf length(geedim()) > 0 && !inherits(gd_initialize(), "try-error")
#' \donttest{
#' gd_initialize()
#' gd_collection_from_name(c("USGS/3DEP/1m", "USGS/NED"))
#' }
gd_collection_from_list <- function(x) {
  y <- try(gd$MaskedCollection$from_list(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}
