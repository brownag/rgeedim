# download Image or ImageCollection

#' Download a Google Earth Engine Image
#'
#' @param x, ID, or a reference to an object inheriting from `geedim.download.BaseImage`
#' @param filename path to output file, defaults to temporary GeoTIFF file path
#' @param region a GeoJSON-like list, or other R spatial object describing region of interest, see `gd_region()` and `gd_bbox()` for details
#' @param overwrite Overwrite existing file? Default: `TRUE`
#' @param silent Silence errors? Default: `TRUE`
#' @param ... Additional arguments (e.g. `scale`) to `geedim.download.BaseImage$download(...)`
#' @seealso `gd_region()` `gd_bbox()`
#' @return Invisible path to downloaded image, or `try-error` on error
#' @export
gd_download <- function(x,
                     filename = tempfile(fileext = ".tif"),
                     region = NULL,
                     overwrite = TRUE,
                     silent = TRUE,
                     ...) {
  stopifnot(inherits(x, "geedim.download.BaseImage"))
  if (is.null(region)) {
    res <- try(x$download(filename = filename, overwrite = overwrite, ...), silent = silent)
  } else {
    res <- try(x$download(filename = filename, region = gd_region(region), overwrite = overwrite, ...), silent = silent)
  }
  if (file.exists(filename) && !inherits(res, 'try-error')) {
    return(filename)
  } else if (inherits(res, 'try-error')) {
    if (!silent) message(res[1])
    return(invisible(res))
  } else {
    return(invisible(NULL))
  }
}
