#' `geedim` Enums
#'
#' @param enum Enum name, one or more of: `"CloudMaskMethod"`, `"CompositeMethod"`, `"ResamplingMethod"`
#'
#' @return `gd_enum_names()`: character vector containing names of Enums
#' @export
#' @rdname enum
#' @examples
#' \donttest{
#'   gd_enum_names()
#' }
gd_enum_names <- function() {
  n <- names(gd$enums)
  n[which(n != "Enum")]
}

#' @return `gd_enum_elements()`: element values of an Enum
#' @export
#' @rdname enum
#' @examplesIf gd_is_initialized()
#' @examples
#' \donttest{
#'   gd_enum_elements()
#' }
gd_enum_elements <- function(enum = gd_enum_names()) {
  enum <- match.arg(enum, gd_enum_names(), several.ok = TRUE)
  res <- lapply(enum, function(x) {
    y <- gd$enums[[x]]
    do.call('c', lapply(names(y), function(z) {
      zz <- y[[z]]
      if ("value" %in% names(zz)) {
        zz$value
      } else
        NULL
    }))
  })
  names(res) <- enum
  res
}

#' @return `gd_resampling_methods()`: character vector of resampling methods (Enum `"ResamplingMethod"`)
#' @export
#' @rdname enum
#' @examplesIf gd_is_initialized()
#' @examples
#' \donttest{
#'   gd_resampling_methods()
#' }
gd_resampling_methods <- function() {
  gd_enum_elements("ResamplingMethod")[[1]]
}

#' @return `gd_cloud_mask_methods()`: character vector of cloud mask methods (Enum `"CloudMaskMethod"`)
#' @export
#' @rdname enum
#' @examplesIf gd_is_initialized()
#' @examples
#' \donttest{
#'   gd_cloud_mask_methods()
#' }
gd_cloud_mask_methods <- function() {
  gd_enum_elements("CloudMaskMethod")[[1]]
}

#' @return `gd_composite_methods()`: character vector of composite methods (Enum `"CompositeMethod"`)
#' @export
#' @rdname enum
#' @examplesIf gd_is_initialized()
#' @examples
#' \donttest{
#'   gd_composite_methods()
#' }
gd_composite_methods <- function() {
  gd_enum_elements("CompositeMethod")[[1]]
}


#' @return `gd_export_types()`: character vector of export types (Enum `"ExportType"`)
#' @export
#' @rdname enum
#' @examplesIf gd_is_initialized()
#' @examples
#' \donttest{
#'   gd_export_types()
#' }
gd_export_types <- function() {
  gd_enum_elements("ExportType")[[1]]
}

#' @return `gd_spectral_distance_metrics()`: character vector of spectral distance metrics (Enum `"SpectralDistanceMetric"`)
#' @export
#' @rdname enum
#' @examplesIf gd_is_initialized()
#' @examples
#' \donttest{
#'   gd_spectral_distance_metrics()
#' }
gd_spectral_distance_metrics <- function() {
  gd_enum_elements("SpectralDistanceMetric")[[1]]
}
