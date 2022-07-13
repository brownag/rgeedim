#' Initialize `geedim`
#' Calls `geedim` `Initialize()` method
#' @param quiet Suppress error messages on load? Default: `TRUE`
#' @return try-error (invisibly) on error
#' @export
#' @importFrom reticulate py_run_string
#' @examples
#' gd_initialize()
gd_initialize <- function(quiet = TRUE) {
  # python 3.10.x compatibility:
  try(collections_module$Callable <- collections_module$abc$Callable, silent = TRUE)

  invisible(try(geedim_module$Initialize(), silent = quiet))
}

#' Authenticate with Google Earth Engine using `gcloud`
#'
#' Calls `ee.Authenticate(...)`
#'
#' @param authorization_code Default: `NULL`
#' @param quiet Default: `FALSE`
#' @param code_verifier Default: `NULL`
#' @param auth_mode Default: `NULL`
#' @export
gd_authenticate <- function(authorization_code = NULL, quiet = FALSE, code_verifier = NULL, auth_mode = NULL) {
  invisible(try(ee_module$Authenticate(
    authorization_code = authorization_code,
    quiet = quiet,
    code_verifier = code_verifier,
    auth_mode = auth_mode
  ), silent = quiet))
}

#' `Module(geedim)` - Get `geedim` Module Instance
#'
#' Gets the `geedim` module instance in use by the package in current **R**/`reticulate` session.
#'
#' @export
gd <- function() {
  geedim_module
}

