#' Handling geedim 1.x.x and 2.x.x APIs
#'
#' rgeedim is designed to work with both geedim 1.x.x and 2.x.x.
#'
#' @section API Changes:
#'
#' `geedim` 2.0.0 introduced a number of breaking changes to the API. The most notable changes are:
#'
#' * The `MaskedImage` and `MaskedCollection` classes are deprecated. The `ee.Image.gd` and `ee.ImageCollection.gd` accessors should be used instead.
#' * The default Landsat cloud mask is more aggressive.
#' * `MaskedCollection.stac` property now returns a STAC dictionary and not a `StacItem` instance.
#' * `MaskedImage` doesn't add mask bands to composite images.
#' * When cloud masking is not supported, `MaskedImage.mask_clouds()` leaves the image unaltered instead of applying a fill mask.
#' * `MaskedImage.scale` is in units of its CRS, not meters.
#'
#' @section New Features:
#'
#' * A new API is provided via `gd` accessors on the `ee.Image` and `ee.ImageCollection` Earth Engine classes.
#' * Client-side access to image and collection properties.
#' * Images and image collections can be exported to GeoTIFF file, NumPy array, Xarray DataArray / Dataset and Google Cloud platforms.
#' * Support for exporting to Cloud Optimised GeoTIFF.
#' * A custom `nodata` value can be set when exporting to GeoTIFF.
#' * Cloud masking support is extended to Landsat C2 collections.
#'
#' @section Backward Compatibility:
#'
#' `rgeedim` provides a backward compatibility layer to ensure that code written for `geedim` 1.x.x continues to work with `geedim` 2.x.x. This is achieved by checking the `geedim` version at runtime and using the appropriate API.
#'
#' If you need to use the old behaviors, you can pin the `geedim` version to `1.9.0` using `reticulate::virtualenv_install()`:
#'
#' ```r
#' reticulate::virtualenv_install("r-rgeedim", "geedim==1.9.0")
#' ```
#'
#' @section Class Equivalents:
#'
#' | geedim 1.x.x             | geedim 2.x.x                |
#' |--------------------------|-----------------------------|
#' | `MaskedImage`            | `ImageAccessor`             |
#' | `MaskedCollection`       | `ImageCollectionAccessor`   |
#'
#' @name geedim-versions
NULL
