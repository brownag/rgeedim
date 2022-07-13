# download Image or ImageCollection

#' Download Google Earth Engine Image/ImageCollection
#'
#' @param x, ID, or a reference to `geedim.download.BaseImage`
#' @param filename path to file, defaults to temporary GeoTIFF file
#' @param region Default `NULL` Python object describing region
#' @param overwrite Overwrite existing file? Default: `TRUE`
#' @param silent Silence errors? Default: `TRUE`
#' @param ... Additional arguments (e.g. `scale`) to `geedim.download.BaseImage$download(...)`
#'
#' @return (invisibly) path to downloaded image
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
    res <- try(x$download(filename = filename, region = region, overwrite = overwrite, ...), silent = silent)
  }
  if (file.exists(filename) && !inherits(res, 'try-error')) {
    return(filename)
  }
  invisible(NULL)
}
