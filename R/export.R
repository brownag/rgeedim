#' Export image to Earth Engine Asset, Google Cloud Storage Bucket, or Google Drive
#' @description Exports an encapsulated image to the destination specified by `type`, `folder` and `filename`
#' @details See the [geedim.image.ImageAccessor.toGoogleCloud() documentation](https://geedim.readthedocs.io/en/stable/reference/api.html#geedim.image.ImageAccessor.toGoogleCloud) for details on additional arguments. Requires 'geedim' >1.6.0.
#' @param x An object that inherits from `geedim.image.ImageAccessor` (for geedim >= 2.0.0) or `geedim.download.BaseImage` (for geedim < 2.0.0). See `\link{geedim-versions}` for more details.
#' @param filename Output filename. If `type` is `"asset"` and `folder` is not specified, `filename` should be a valid Earth Engine asset ID.
#' @param type Export type. Defaults to `"drive"`; other options include `"asset"`, and "`cloud`". See `gd_export_types()`
#' @param folder Destination folder. Defaults to `dirname(filename)`.
#' @param region Region e.g. from `gd_bbox()` or `gd_region()`
#' @param wait Wait for completion? Default: `TRUE`
#' @param ... Additional arguments to `geedim.image.ImageAccessor.toGoogleCloud()` (geedim >= 2.0.0) or `geedim.download.BaseImage.export()` (geedim 1.x.x)
#'
#' @return an `ee.batch.Task` object
#' @export
#' @examples
#' \dontrun{
#' if (gd_is_initialized()) {
#'  r <- gd_bbox(
#'    xmin = -120.6032,
#'    xmax = -120.5377,
#'    ymin = 38.0807,
#'    ymax = 38.1043
#'  )
#'
#'  i <- gd_image_from_id('CSP/ERGo/1_0/US/CHILI')
#'
#'  ## export to Google Drive (default `type="drive"`)
#'  # res <- gd_export(i, filename = "RGEEDIM_TEST", scale = 100, region = r)
#'
#'  ## export to `type="asset"`, then download by ID (stored in project assets)
#'  # res <- gd_export(
#'  #   i,
#'  #   "RGEEDIM_TEST",
#'  #   type = "asset",
#'  #   folder = "rgeedim-demo",
#'  #   scale = 100,
#'  #   region = r
#'  # )
#'  # gd_download("projects/rgeedim-demo/assets/RGEEDIM_TEST", filename = "test.tif")
#'
#'  ## export to Google Cloud Bucket with `type="cloud"`,
#'  ##   where `folder` is the bucket path without `"gs://"`
#'  # res <- gd_export(i, filename = "RGEEDIM_TEST.tif", type = "cloud",
#'  #                  folder = "your-bucket-name", scale = 100, region = r)
#' }
#' }
gd_export <- function(x, filename, type = "drive", folder = dirname(filename), region, wait = TRUE, ...) {
  .inform_missing_module(x, "geedim")
  
  args <- list(...)
  
  if (gd_version() >= "2.0.0") {
    
    # Identify arguments for prepareForExport
    # https://geedim.readthedocs.io/en/stable/reference/api.html#geedim.image.ImageAccessor.prepareForExport
    prepare_params <- c("crs", "crs_transform", "shape", "scale", "resampling", "dtype", "scale_offset", "bands")
    prepare_args <- args[names(args) %in% prepare_params]
    cloud_args <- args[!names(args) %in% prepare_params]
    
    # If region is provided, add it to prepare_args
    if (!missing(region) && !is.null(region)) {
       prepare_args$region <- earthengine()$Geometry(gd_region(region))
    }
    
    # Call prepareForExport if we have relevant arguments
    # resulting object is an ee.Image, but geedim monkey-patches .gd property to get ImageAccessor back
    if (length(prepare_args) > 0) {
       x <- do.call(x$prepareForExport, prepare_args)$gd
    }
    
    # Call toGoogleCloud
    do.call(x$toGoogleCloud, c(list(filename = filename, type = type, folder = folder, wait = wait), cloud_args))

  } else {
    x$export(filename = filename, type = type, folder = folder, region = gd_region(region), wait = wait, ...)
  }
}
