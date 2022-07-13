#' Initialize `geedim`
#' Calls `geedim` `Initialize()` method
#' @export
#' @importFrom reticulate py_run_string
#' @examples
#' gd_initialize()
gd_initialize <- function() {
  # python 3.10.x compatibility:
  try(collections_module$Callable <- collections_module$abc$Callable, silent = TRUE)

  geedim_module$Initialize()
}

#' `Module(geedim)` - Get `geedim` Module Instance
#'
#' Gets the `geedim` module instance in use by the package in current **R**/`reticulate` session.
#'
#' @export
gd <- function() {
  geedim_module
}

