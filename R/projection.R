#' Get Projection Information from Google Earth Engine Asset
#'
#' @param x character ID referencing asset, or an image object (subclass of `ee.image.Image`, `geedim.image.ImageAccessor` for geedim >= 2.0.0, or `geedim.download.BaseImage` for geedim < 2.0.0). See `\link{geedim-versions}` for more details.
#'
#' @return `ee.Projection` object
#' @export
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_projection(gd_image_from_id('CSP/ERGo/1_0/Global/SRTM_topoDiversity'))
#' }
gd_projection <- function(x) {
  .inform_missing_module(gd, "geedim")
  
  if (is.character(x)) {
    x <- gd_image_from_id(x)
  }
  
  if (!inherits(x, "ee.image.Image")) {
    if (inherits(x, 'geedim.download.BaseImage')) {
      return(gd$utils$get_projection(x$ee_image))
    } else {
      if (gd_version() >= "2.0.0") {
        return(x$projection())
      } else {
        return(gd$utils$get_projection(gd$utils$ee$Image(x)))
      }
    }
  } else {
    return(gd$utils$get_projection(x))
  }
}
