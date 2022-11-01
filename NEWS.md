# rgeedim 0.1.0

* Initial CRAN release

* `gd_region()` now supports more complex SpatVector geometries (no longer uses extent to form bounding box if `x` is SpatVector or can be converted to one)

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
