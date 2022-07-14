gd <- NULL
ee <- NULL
collections_module <- NULL

#' `Module(geedim)` - Get `geedim` Module Instance
#'
#' Gets the `geedim` module instance in use by the package in current **R**/`reticulate` session.
#'
#' @export
geedim <- function() {
  gd
}


#' `Module(earthengine)` - Get `earthengine` Module Instance
#'
#' Gets the `earthengine` module instance in use by the package in current **R**/`reticulate` session.
#'
#' @export
earthengine <- function() {
  ee
}

#' @importFrom reticulate import
.loadModules <-  function() {
  # TODO: store modules in package environment not global variables?

  if (is.null(collections_module)) {
    try(collections_module <<- reticulate::import('collections', delay_load = TRUE), silent = TRUE)
  }

  if (is.null(gd)) {
    try(gd <<- reticulate::import("geedim", delay_load = TRUE), silent = TRUE)
  }

  if (is.null(ee)) {
    try(ee <<- reticulate::import("ee", delay_load = TRUE), silent = TRUE)
  }

  !is.null(gd)
}

#' @importFrom reticulate configure_environment
.onLoad <- function(libname, pkgname) {
  if (!.loadModules()) {
    reticulate::configure_environment(pkgname)
    .loadModules()
  }
}
