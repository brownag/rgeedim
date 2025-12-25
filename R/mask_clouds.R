#' Mask Clouds or Apply Fill Mask
#'
#' Apply the cloud/shadow mask if supported, otherwise apply the fill mask.
#'
#' @param x a `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.mask.MaskedImage` (for geedim < 2.0.0). See `\link{geedim-versions}` for more details.
#'
#' @return a `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.mask.MaskedImage` (for geedim < 2.0.0). See `\link{geedim-versions}` for more details.
#' @export
gd_mask_clouds <- function(x) {
  if (!inherits(x, c('geedim.image.ImageAccessor',
                     'geedim.mask.MaskedImage'))) {
    stop("`x` should be a geedim.image.ImageAccessor or geedim.mask.MaskedImage")
  }
  if (inherits(x, "geedim.image.ImageAccessor")) {
    return(x$maskClouds()$gd) 
  } else {
    return(x$mask_clouds())
  }
}
