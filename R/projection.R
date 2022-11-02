# projection information from server side


#' Get Projection Information from Google Earth Engine Asset
#'
#' @param x character ID referencing asset, or an image object (subclass of `ee.image.Image` or `geedim.download.BaseImage`)
#'
#' @return `ee.Projection` object
#' @export
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_projection(gd_image_from_id('CSP/ERGo/1_0/Global/SRTM_topoDiversity'))
#' }
gd_projection <- function(x) {
  if (!inherits(x, "ee.image.Image")) {
    if (inherits(x, 'geedim.download.BaseImage')) {
      gd$utils$get_projection(x$ee_image)
    } else {
      gd$utils$get_projection(gd$utils$ee$Image(x))
    }
  } else {
    gd$utils$get_projection(x)
  }
}
