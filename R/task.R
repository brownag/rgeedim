#' Get Earth Engine Task Status
#' 
#' `gd_task_status()` and `gd_task_uri()` are helper functions for working with tasks  scheduled with `gd_export()`
#' 
#' @param x An object of class `"ee.batch.Task"`
#' @return `gd_task_status()`: returns the status from an `"ee.batch.Task"` object
#' @export
#' @rdname gd_task
#' 
#' @examples
#' \dontrun{
#' if (gd_is_initialized()) {
#'   r <- gd_bbox(
#'     xmin = -120.6032,
#'     xmax = -120.5377,
#'     ymin = 38.0807,
#'     ymax = 38.1043
#'   )
#'   
#'   i <- gd_image_from_id('CSP/ERGo/1_0/US/CHILI')
#'   ex <- gd_export(
#'     i,
#'     region = r,
#'     type = "asset",
#'     filename = "RGEEDIM_TEST",
#'     folder = "your-project-name",
#'     scale = 30
#'   )
#'   
#'   gd_task_status(ex)
#'   
#'   r <- gd_download(
#'     gd_task_uri(ex),
#'     filename = "image.tif",
#'     region = r,
#'     overwrite = TRUE
#'   )
#'   
#'   library(terra)
#'   plot(rast(r))
#' }
#' }
gd_task_status <- function(x) {
  if (inherits(x, 'ee.batch.Task')) {
    return(x$status())
  } else NULL
}

#' @param asset_only Default: `TRUE`. For export tasks with `type="asset"`, return only the asset ID, rather than whole URL. Other export task types return a full path to either Google Drive or Google Cloud location. When `FALSE` the path is a HTTPS link to an Earth Engine asset.
#' @return `gd_task_uri()`: returns the destination URI(s) associated with a task. 
#' @export
#' @rdname gd_task
#' @seealso [gd_export()] [gd_download()]
gd_task_uri <- function(x, asset_only = TRUE) {
  if (inherits(x, 'ee.batch.Task')) {
    s <- gd_task_status(x)
    u <- s$destination_uris
    return(switch(s$task_type,
                  "EXPORT_IMAGE" = ifelse(
                    rep(asset_only, length(u)),
                    gsub(".*\\?asset=(.*)", "\\1", u), u
                  ), u))
  }
  NULL
}
