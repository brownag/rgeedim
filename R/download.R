# download Image or ImageCollection

#' Download a Google Earth Engine Image
#'
#' @param x, ID or Name, or a reference to an object inheriting from `geedim.download.BaseImage` or `geedim.collection.MaskedCollection`
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
  
  if (is.character(x)) {
    xx <- try(gd_image_from_id(x), silent = TRUE)
    if (inherits(xx, 'try-error')) {
      xx <- try(gd_collection_from_name(x), silent = TRUE)
    }
    if (!inherits(xx, 'try-error')) {
      x <- xx
    } else stop("Could not create Image or Image Collection from '", x, "'", call. = FALSE)
  }
  
  if (inherits(x, "geedim.download.BaseImage")) {
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
  } else if (inherits(x, "geedim.collection.MaskedCollection")) {
    .args <- list(...)
    scale <- .args[["scale"]]
    if (is.null(scale)) 
      stop("Downloading an Image Collection requires that the `scale` argument be set.", call. = FALSE)
    .gd_download_collection(x,
                            destdir = filename,
                            region = region,
                            scale = scale,
                            overwrite = overwrite,
                            silent = silent,
                            ...)
  }
}

#' @noRd
#' @keywords internal
.gd_download_collection <- function(x, destdir, region, overwrite, silent, scale, ...) {
  if (file.exists(destdir))
    destdir <- dirname(destdir)

  if (!dir.exists(destdir)) 
    dir.create(destdir, recursive = TRUE)
  
  sapply(gd_properties(x)$id, function(y) {
    img <- gd_image_from_id(y)
    fp <- sprintf(file.path(destdir, paste0(basename(y), "_%sm.tif")), 
                  ifelse(scale < 1000, scale, paste0(scale / 1000, "k")))
    if (!file.exists(fp)) {
      y <- gd_download(img, fp, scale = scale,  ...)
    }
    # update names; TODO get this fixed in geedim
    # r <- terra::rast(x)
    # names(r) <- gd_bandnames(img)
    # outfile <- terra::sources(img)[1]
    # terra::writeRaster(r, outfile)
    # outfile
    y
  })
}