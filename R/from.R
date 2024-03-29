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
#'   # select images with some condition of interest

#'   z <- subset(gd_properties(y),
#'               grepl("UpperSouthAmerican_Eldorado_2019", id) > 0)
#'
#'   # create encapsulated images from IDs returned by search
#'   l <- lapply(z$id, gd_image_from_id)
#'
#'   # create a new collection from the list of images
#'   l2 <- gd_collection_from_list(l)
#'   l2
#'
#' ### download composite of custom collection
#' #  gd_download(gd_composite(l2),
#' #              filename = "test.tif",
#' #              region = r,
#' #              crs = "EPSG:5070",
#' #              scale = 30)
#'
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

#' @export
#' @param parent Full path to project folder (with or without `"/assets"` suffix)
#' @rdname from
#' @examplesIf gd_is_initialized()
#' \donttest{
#' if (gd_is_initialized())
#'   gd_list_assets("projects/your-project-name")
#' }
gd_list_assets <- function(parent) {
  if (!endsWith(parent, "/assets"))
    parent <- paste0(parent, "/assets")
  res <- gd$utils$ee$data$listAssets(list(parent = parent))
  if (length(res) == 0 || length(res[[1]]) == 0) {
    return(data.frame(type = character(0), name = character(0), id = character(0), updateTime = structure(numeric(0), class = c("POSIXct", "POSIXt"), tzone = "GMT"), stringsAsFactors = FALSE))
  }
  res <- do.call('rbind', lapply(res, function(x) {
    data.frame(t(do.call('cbind', x)))
  }))
  res[] <- lapply(res, unlist)
  rownames(res) <- NULL
  res$updateTime <- as.POSIXct(res$updateTime, format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
  res
}
