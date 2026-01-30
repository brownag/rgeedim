# download Image or ImageCollection

#' Download a Google Earth Engine Image
#'
#' @param x, ID or Name, or a reference to an object inheriting from `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.download.BaseImage` (for geedim < 2.0.0) or `geedim.collection.ImageCollectionAccessor` (for geedim >= 2.0.0) or `geedim.collection.MaskedCollection` (for geedim < 2.0.0). See `\link{geedim-versions}` for more details.
#' @param filename path to output file, defaults to temporary GeoTIFF file path; if `composite=FALSE` then this path should be to a parent directory. File names will be calculated from the internal name of the image and the requested scale.
#' @param region a GeoJSON-like list, or other R spatial object describing region of interest, see `gd_region()` and `gd_bbox()` for details. `NULL` region (default) will download the whole image.
#' @param composite logical. Composite Image Collection into single image for download? Default: `TRUE`
#' @param overwrite Overwrite existing file? Default: `TRUE`
#' @param silent Silence errors? Default: `TRUE`
#' @param ... Additional arguments (e.g. `scale`) passed to [`geedim.image.ImageAccessor$toGeoTIFF(...)`](https://geedim.readthedocs.io/en/stable/reference/api.html#geedim.image.ImageAccessor.toGeoTIFF) and, when `composite=TRUE`, [`geedim.collection.ImageCollectionAccessor$composite()`](https://geedim.readthedocs.io/en/stable/reference/api.html#geedim.collection.ImageCollectionAccessor.composite)
#' @details The `region` argument is _optional_ for downloading images. When downloading a composite Image Collection, you must specify `region`, `scale` and `crs` arguments. When downloading an image collection as a set of GeoTIFF files (`composite=FALSE`), then `filename` is the destination directory, and `scale` must be specified.
#'     The default resampling method in `geedim` is `resampling="near"` (Nearest Neighbor). Other options for `resampling` include: `"average"`, `"bicubic"`, `"bilinear"`. See `gd_resampling_methods()`.
#' @seealso `gd_region()` `gd_bbox()`
#' @return Invisible path to downloaded image, or `try-error` on error
#' @export
#' @examplesIf gd_is_initialized()
#' \donttest{
#'  r <- gd_bbox(
#'    xmin = -121,
#'    xmax = -120.5,
#'    ymin = 38.5,
#'    ymax = 39
#'  )
#'
#' if (gd_is_initialized()) {
#'  x <- gd_image_from_id('CSP/ERGo/1_0/Global/SRTM_topoDiversity')
#'  tf <- tempfile(fileext = ".tif")
#'
#'  # fast sample download at 10x aggregation (900m v.s. 90m)
#'  img <- gd_download(x, filename = tf,
#'                     region = r, scale = 900,
#'                     overwrite = TRUE, silent = FALSE)
#'
#'  if (requireNamespace("terra")) {
#'    library(terra)
#'    f <- rast(img)
#'    plot(f[[1]])
#'    # inspect object
#'    f
#'  }
#'  unlink(tf)
#' }
#' }
gd_download <- function(x,
                        filename = tempfile(fileext = ".tif"),
                        region = NULL,
                        composite = TRUE,
                        overwrite = TRUE,
                        silent = TRUE,
                        ...) {

  filename <- path.expand(filename)

  # check additional arguments to download()/composite()
  extra.args <- list(...)

  # check `resampling` method (if undefined "near" is used)
  rsm <- extra.args[["resampling"]]
  if (!is.null(rsm)) {
    rsm <- match.arg(rsm, gd_resampling_methods())
  }

  # check compositing `method` (with composite=TRUE and `x` is a maskedcollection)
  com <- extra.args[["method"]]
  if (!is.null(com)) {
    com <- match.arg(com, gd_composite_methods())
  }

  if (is.character(x)) {
    xx <- try(gd_image_from_id(x), silent = TRUE)
    if (inherits(xx, 'try-error')) {
      xx <- try(gd_collection_from_name(x), silent = TRUE)
    }
    if (!inherits(xx, 'try-error')) {
      x <- xx
    } else stop("Could not create Image or Image Collection from '", x, "'", call. = FALSE)
  }

  if (inherits(x, "geedim.image.ImageAccessor")) {
    if (is.null(region)) {
      x <- x$prepareForExport(...)$gd
    } else {
      x <- x$prepareForExport(region = gd_region(region), ...)$gd
    }
    res <- try(x$toGeoTIFF(file = filename, overwrite = overwrite), silent = silent)
    if (file.exists(filename) && !inherits(res, 'try-error')) {
      return(filename)
    } else if (inherits(res, 'try-error')) {
      message(res[1]) # silent only handles the actual try() blocks, messages can be suppressed if needed
      return(invisible(res))
    } else {
      return(invisible(NULL))
    }
  } else if (inherits(x, "geedim.download.BaseImage")) {
    if (is.null(region)) {
      res <- try(x$download(filename = filename, overwrite = overwrite, ...), silent = silent)
    } else {
      res <- try(x$download(filename = filename, region = gd_region(region), overwrite = overwrite, ...), silent = silent)
    }
    if (file.exists(filename) && !inherits(res, 'try-error')) {
      return(filename)
    } else if (inherits(res, 'try-error')) {
      message(res[1]) # silent only handles the actual try() blocks, messages can be suppressed if needed
      return(invisible(res))
    } else {
      return(invisible(NULL))
    }
  } else if (inherits(x, c("geedim.collection.ImageCollectionAccessor", 
                           "geedim.collection.MaskedCollection"))) {
    .args <- list(...)
    scale <- .args[["scale"]]
    if (is.null(scale)) {
      stop("Downloading an Image Collection requires that the `scale` argument be set.", call. = FALSE)
    }
    .gd_download_collection(x,
                            dest = filename,
                            region = region,
                            composite = composite,
                            overwrite = overwrite,
                            silent = silent,
                            ...)
  }
}

#' @noRd
#' @keywords internal
.gd_download_collection <- function(x, dest, region, scale, crs = NULL, overwrite, silent, composite = TRUE, ...) {
  stopifnot(length(dest) == 1)
  # by default the assumption is a collection can/should be mosaiced/"composited" before download
  # this is true for any "non-seamless" datasets e.g. satellites, 3DEP 1m, etc.
  if (composite) {
    y <- gd_composite(x, ...)
    if (inherits(y, 'try-error')) {
      return(invisible(y))
    }
    if (!file.exists(dest) || overwrite) {
      if (!missing(crs)) {
        return(gd_download(y, dest, region = region, scale = scale, crs = crs, silent = silent, overwrite = overwrite, ...))
      }
      return(gd_download(y, dest, region = region, scale = scale, silent = silent, overwrite = overwrite, ...))
    }
  # otherwise, need a search()-ed properties table, and iterate over IDs
  } else {
    prp <- gd_properties(x)
    if (inherits(prp, 'try-error')) {
      return(invisible(prp))
    }
    if (inherits(x, 'geedim.collection.ImageCollectionAccessor')) {
      prp$id <- paste0(x$id, "/", prp$id)
    }
    if (is.null(prp)) stop("Empty properties table; your collection may have no features; expand your search parameters with `gd_search() or use `gd_collection_from_list()` to create a collection from desired images.", call. = FALSE)
    sapply(prp$id, function(y) {
      if (file.exists(dest) && !dir.exists(dest)) {
        stop("When composite=FALSE, `filename` should be a directory where multiple files will be downloaded", call. = FALSE)
      }
      if (!dir.exists(dest)) {
        if (all(grepl(dest, "\\.tiff?$", ignore.case = TRUE))) {
          message("gd_download: Dropping .tif extension from destination directory name with `composite=FALSE`")
        }
        dir.create(gsub("(.*)(\\.tiff?)", "\\1", dest, ignore.case = TRUE), recursive = TRUE)
      }
      img <- try(gd_image_from_id(y), silent = silent)
      if (inherits(img, 'try-error')) {
        message(img[1])
        return(invisible(img))
      }
      fp <- sprintf(file.path(dest, paste0(basename(y), "_%sm.tif")),
                    ifelse(scale < 1000, scale, paste0(scale / 1000, "k")))
      if (overwrite || !file.exists(fp)) {
        y <- gd_download(img, fp, region = region, scale = scale, silent = silent, overwrite = overwrite, ...)
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
}
