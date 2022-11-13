# rgeedim 0.1.1

* Fix for R <4.1 compatibility (replaced lambda `\(x)` function syntax, replaced `apply(..., simplify=FALSE)` usage)

* Updates to support new functionality in geedim 1.6+
   
   - Added `gd_export()`, a helper method for exporting images to Google Drive, Google Cloud Storage Bucket, or Earth Engine Project assets.
   
   - Added `gd_export_types()` `"ExportType"` enum helper function

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
