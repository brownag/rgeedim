#' Get, Update, or Delete an Earth Engine Asset by ID
#'
#' @param x Asset ID name
#' @param asset Used only for `gd_update_asset()`: a named list, with names representing elements of `x` to replace. Only `"start_time"`, `"end_time"`, and `"properties"` fields can be updated.
#' @param update Used only for `gd_update_asset()`: A character vector of field names to update. Default: `"start_time"`, and `"end_time"` to update timestamps; and `"properties"` to update all properties.
#' @param silent Silence errors? Default: `FALSE`
#' @return `try-error` on error. `gd_get_asset()`: a named list containing information and properties of an Earth Engine asset
#' @export
#'
#' @rdname existing-assets
#' @examples
#' \dontrun{
#' # get asset from project by ID
#' a <- gd_get_asset("projects/your-project-name/assets/YOUR_ASSET_ID")
#' }
gd_get_asset <- function(x, silent = FALSE) {
  .inform_missing_module(gd, "geedim")
  try(gd$utils$ee$data$getAsset(x), silent = silent)
}

#' @rdname existing-assets
#' @return `gd_update_asset()`: This function is called for side-effects (updates the specified asset fields)
#' @examples
#' \dontrun{
#' # change description in `"properties"`
#' a$properties$description <- "foo"
#'
#' # update asset
#' gd_update_asset("projects/your-project-name/assets/YOUR_ASSET_ID", a, "properties")
#' }
gd_update_asset <- function(x, asset, update = c("start_time", "end_time", "properties"), silent = FALSE) {
  .inform_missing_module(gd, "geedim")
  try(gd$utils$ee$data$updateAsset(x, asset, update), silent = silent)
}

#' @rdname existing-assets
#' @return `gd_delete_asset()`: This function is called for side-effects (deletes the specified asset)
#' @export
#' @examples
#' \dontrun{
#' # remove an asset from project
#' gd_delete_asset("projects/your-project-name/assets/YOUR_ASSET_ID")
#' }
gd_delete_asset <- function(x, silent = FALSE) {
  .inform_missing_module(gd, "geedim")
  try(gd$utils$ee$data$deleteAsset(x), silent = silent)
}
