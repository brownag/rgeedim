# rgeedim 0.0.0.9006

* Better handling of additional arguments (i.e. `crs`, `resampling`, `method`) in `gd_download()` when `x` is a `MaskedCollection` and `composite=TRUE`. 

* Added helper methods for working with the `geedim` enums: "CloudMaskMethod", "CompositeMethod" and "ResamplingMethod"

* Added `gd_mask_clouds()` for masking out clouds or to apply a fill mask

* Added a `NEWS.md` file to track changes to the package.
