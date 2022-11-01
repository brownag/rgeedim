# projection information from server side


#' Get Projection Information from Google Earth Engine Asset
#'
#' @param x character ID referencing asset, or an image object (subclass of `ee.image.Image` or `geedim.download.BaseImage`)
#'
#' @return `ee.Projection` object
#' @export
#' @examplesIf length(geedim()) > 1
#' \donttest{
#' gd_initialize()
#' gd_projection(gd_image_from_id('CSP/ERGo/1_0/Global/SRTM_topoDiversity'))
#' }
gd_projection <- function(x) {
  if (!inherits(x, "ee.image.Image")) {
    if (inherits(x, 'geedim.download.BaseImage')) {
      gd$utils$get_projection(x$ee_image)
    } else {
      gd$utils$get_projection(ee$Image(x))
    }
  } else {
    gd$utils$get_projection(x)
  }
}
