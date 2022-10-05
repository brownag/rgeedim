#' Mask Clouds or Apply Fill Mask
#'
#' Apply the cloud/shadow mask if supported, otherwise apply the fill mask.
#'
#' @param x a `geedim.mask.MaskedImage`
#'
#' @return a `geedim.mask.MaskedImage`
#' @export
gd_mask_clouds <- function(x) {
  if (!inherits(x, 'geedim.mask.MaskedImage')) {
    stop("`x` should be a geedim.mask.MaskedImage")
  }
  x$mask_clouds()
}
