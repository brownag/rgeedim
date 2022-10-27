#' `geedim` Enums
#'
#' @param enum Enum name, one or more of: `"CloudMaskMethod"`, `"CompositeMethod"`, `"ResamplingMethod"`
#'
#' @return `gd_enum_names()`: character vector containing names of Enums
#' @export
#' @rdname enum
gd_enum_names <- function() {
  n <- names(gd$enums)
  n[which(n != "Enum")]
}

#' @return `gd_enum_elements()`: element values of an Enum
#' @export
#' @rdname enum
gd_enum_elements <- function(enum = gd_enum_names()) {
  enum <- match.arg(enum, gd_enum_names(), several.ok = TRUE)
  res <- lapply(enum, \(x) {
    y <- gd$enums[[x]]
    sapply(names(y), \(z) y[[z]]$value)
  })
  names(res) <- enum
  res
}

#' @return `gd_resampling_methods()`: character vector of resampling methods (Enum `"ResamplingMethod"`)
#' @export
#' @rdname enum
gd_resampling_methods <- function() {
  gd_enum_elements("ResamplingMethod")[[1]]
}

#' @return `gd_cloud_mask_methods()`: character vector of cloud mask methods (Enum `"CloudMaskMethod"`)
#' @export
#' @rdname enum
gd_cloud_mask_methods <- function() {
  gd_enum_elements("CloudMaskMethod")[[1]]
}

#' @return `gd_composite_methods()`: character vector of composite methods (Enum `"CompositeMethod"`)
#' @export
#' @rdname enum
gd_composite_methods <- function() {
  gd_enum_elements("CompositeMethod")[[1]]
}
