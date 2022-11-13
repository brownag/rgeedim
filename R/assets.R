#' Get, Update, or Delete an Earth Engine Asset by ID
#'
#' @param x Asset ID name
#' @param asset Used only for `gd_update_asset()`, a list representing elements of `x` to replace. Only the `"start_time"`, `"end_time"`, and `"properties"` fields can be updated. If a value is named in "updateFields", but is unset in `asset`, then that value will be deleted from the asset.
#' @param updateFields Used only for `gd_update_asset()`. A character vector of field names to update. This may contain: "start_time" or "end_time" to update the corresponding timestamp, "properties.PROPERTY_NAME" to update a given property, or "properties" to update all properties. If the list is empty (default), all properties and both timestamps will be updated.
#' @return This function is called for its side-effects (updates or deletes the specified asset)
#' @export
#'
#' @rdname existing-assets
#' @examples
#' \dontrun{
#' a <- gd_get_asset("projects/your-project-name/assets/YOUR_ASSET_ID")
#' }
gd_get_asset <- function(x) {
  gd$utils$ee$data$getAsset(x)
}

#' @rdname existing-assets
#' @examples
#' \dontrun{
#' a$properties$description <- "foo"
#' gd_update_asset("projects/your-project-name/assets/YOUR_ASSET_ID", a, "properties")
#' }
gd_update_asset <- function(x, asset, updateFields = character(0)) {
  if (length(updateFields) == 0) {
    updateFields <- c("start_time", "end_time", "properties")
  }
  gd$utils$ee$data$updateAsset(x, asset, updateFields)
}

#' @rdname existing-assets
#' @export
#' @examples
#' \dontrun{
#' gd_delete_asset("projects/your-project-name/assets/YOUR_ASSET_ID")
#' }
gd_delete_asset <- function(x) {
  gd$utils$ee$data$deleteAsset(x)
}
