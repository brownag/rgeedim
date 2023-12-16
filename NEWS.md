# rgeedim 0.2.6
 * `gd_authenticate()`: Updates
 
   - Added `force` and `scopes` arguments from `earthengine()$Authenticate()`. `force` defaults to `TRUE` for consistency with prior behavior, and because users often want to use `gd_authenticate()` to change existing credentials.
   
   - Updated documentation for other `auth_mode` e.g. `"colab"`
   
 * `gd_initialize()`: Updates
 
   - Added arguments `credentials`, `cloud_api_key`, `url`, `http_transport` and `project`
   
   - Deprecated argument `opt_url` (in favor of `url`)
   
 * Argument updates compatible with older versions of earthengine-api (< 0.1.382) 
 
# rgeedim 0.2.5

 * Update example for `gd_enum_names()`
 
 * Fix different value storage in `gd_enum_elements()` (required for reticulate >= 1.29)
 
 * Update examples in /misc folder: <https://github.com/brownag/rgeedim/tree/main/misc>
  - Add new example using `gd_export()` and the Earth Engine API directly via `earthengine()`
 
 * Add `gd_region_to_vect()` an inverse method for `gd_bbox()`/`gd_region()` that creates a 'terra' _SpatVector_ from a GeoJSON-like list

 * Add `gd_list_assets()` a helper function for listing the assets associated with a particular Earth Engine project.
 
 * `reticulate::configure_environment()` is no longer called on load, this is a precaution to avoid unintended impacts from the automatic routine installing on CRAN or similar.

# rgeedim 0.2.4
 
 * Add `gd_task_status()` and `gd_task_uri()` for working with Task object produced by `gd_export()`
 
 * Fix for `gd_composite()` and `gd_export()` errors when `region` argument is specified as an R spatial object (rather than GeoJSON-like list)
 
 * Fix bug in `gd_enum_elements()` and add `gd_spectral_distance_metrics()`
 
 * `gd_initialize()`: Fix use of illogical use of `GOOGLE_APPLICATION_CREDENTIALS` environment variable contents for Google Cloud service accounts under some conditions
  - `EE_SERVICE_ACC_PRIVATE_KEY` is used for service accounts, whereas the former is used only for application credentials. `GOOGLE_APPLICATION_CREDENTIALS` is respected by `gd_authenticate()` `auth_mode` `"gcloud"` and `"appdefault"`.
 
# rgeedim 0.2.3

 * For `gd_install()` `method="virtualenv"` or `method="conda"` if an environment of `envname` (default: `"r-reticulate"`) does not exist, it gets created before running `py_install()`

 * Add `gd_spectral_distance_metrics()` enum helper

# rgeedim 0.2.2

 * Added `gd_install()` for installation of 'numpy', 'earthengine-api', and 'geedim' Python modules via `reticulate::py_install()` or a `system()` call 
 
 * `gd_bbox()` will now calculate a bounding box extent from one or more {terra} `SpatRaster`, `SpatRasterCollection`, `SpatVector`, `SpatVectorProxy` input (in addition to existing support for `SpatExtent`)
 
   * Note that `gd_region()` allows for more complex boundary input via `SpatVector` or Well-Known Text (WKT) string
 
 * Improved coercion interface for non-{terra} objects 
 
   * The following inputs are now converted to {terra} equivalents (or their extents) as needed: WKT string, Spatial* ({sp} package), Raster* & Extent ({raster} package), sf* and bbox ({sf} package). WKT strings and `SpatExtent`-like objects (`Extent`, `bbox`) are assumed to be in the `"OGC:CRS84"` coordinate reference system.
 
# rgeedim 0.2.1

 * Updates to `.onLoad()` to avoid error messages related to Python 3 configuration discovery for {reticulate}

# rgeedim 0.2.0

* Updates to support new image export to asset functionality in geedim 1.6+
   
   - Added `gd_export()`, a helper method for exporting images to Google Drive, Google Cloud Storage Bucket, or Earth Engine Project assets.
   
   - Added `gd_export_types()` `"ExportType"` enum helper function
   
   - Added `gd_asset_id()`, a helper method for creating asset IDs from a file/asset name and (optional) project name.
   
   - Added `gd_get_asset()`, `gd_update_asset()`, and `gd_delete_asset()`, helper functions for accessing, updating and deleting assets created in a Google Cloud project (i.e. those created via `gd_export(..., type="asset")`)

# rgeedim 0.1.1

* Fix for R <4.1 compatibility (replaced lambda `\(x)` function syntax, replaced `apply(..., simplify=FALSE)` usage)

# rgeedim 0.1.0

* Initial CRAN release

* `gd_region()` now supports more complex SpatVector geometries (no longer uses extent to form bounding box if `x` is SpatVector or can be converted to one)

* Add `gd_is_initialized()` and use for examples and other conditional evaluation of code that requires authentication and initialized 'Google Earth Engine' resources

# rgeedim 0.0.0.9008

* Renamed `gd_bandnames()` -> `gd_band_names()`

* Added `gd_band_properties()` and `gd_footprint()`

# rgeedim 0.0.0.9007

* Fix for enum helper functions `gd_resampling_methods()`, `gd_composite_methods()`, `gd_cloud_mask_methods()` to return values rather than names

* `gd_download()` now supports path expansion for `filename` argument

# rgeedim 0.0.0.9006

* Better handling of additional arguments (i.e. `crs`, `resampling`, `method`) in `gd_download()` when `x` is a `MaskedCollection` and `composite=TRUE`. 

* Added helper methods for working with the `geedim` enums: "CloudMaskMethod", "CompositeMethod" and "ResamplingMethod"

* Added `gd_mask_clouds()` for masking out clouds or to apply a fill mask

* Added a `NEWS.md` file to track changes to the package.
