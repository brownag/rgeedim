#' Reference Google Earth Engine Image or Image Collection by ID or Name
#'
#' Create references to a Google Earth Engine Image or Image Collection based on IDs or names, or combine Images into Image Collections.
#'
#' @param x character. `id` of Image, `name` of Image Collection, or a vector of Image `id` to create a new Image Collection
#'
#' @return `geedim.MaskedImage` or `geedim.MaskedCollection` object, or `try-error` on error
#'
#' @export
#' @rdname from
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_image_from_id('CSP/ERGo/1_0/Global/SRTM_topoDiversity')
#' }
gd_image_from_id <- function(x) {
  y <- try(gd$MaskedImage$from_id(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @rdname from
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'
#'   # Find 1m DEMs in arbitrary extent
#'   r <- gd_bbox(xmin = -121.4, xmax = -121.35, ymin = 37.55, ymax = 37.6)
#'
#'   # collection of individual tiles of DEM
#'   x <- gd_collection_from_name("USGS/3DEP/1m")
#'
#'   # search within region
#'   y <- gd_search(x, r)
#'
#'   gd_properties(y)
#'
#' }
gd_collection_from_name <- function(x) {
  y <- try(gd$MaskedCollection$from_name(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @rdname from
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   # Find 1m DEM in arbitrary extent
#'   r <- gd_bbox(xmin = -121.4, xmax = -121.35, ymin = 37.55, ymax = 37.6)
#'
#'   # collection of individual tiles of DEM
#'   x <- gd_collection_from_name("USGS/3DEP/1m")
#'
#'   # search within region
#'   y <- gd_search(x, r)
#'
#'   # select images with fill > 0
#'   z <- subset(gd_properties(y), fill > 0)
#'
#'   # create encapsulated images from IDs returned by search
#'   l <- lapply(z$id, gd_image_from_id)
#'
#'   # create a new collection from the list of images
#'   gd_collection_from_list(l)
#' }
gd_collection_from_list <- function(x) {
  y <- try(gd$MaskedCollection$from_list(x), silent = FALSE)
  if (inherits(y, 'try-error')) return(invisible(y))
  y
}

#' @export
#' @param filename File or Asset Name
#' @param folder Optional: Project Name
#' @rdname from
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_asset_id("RGEEDIM_TEST", "your-project-name")
#' }
gd_asset_id <- function(filename, folder = NULL) {
  gd$utils$asset_id(filename, folder)
}
