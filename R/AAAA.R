geedim_module <- NULL
ee_module <- NULL
collections_module <- NULL

#' @importFrom reticulate import
.loadModules <-  function() {
  # TODO: store modules in package environment not global variables?

  if (is.null(collections_module)) {
    try(collections_module <<- reticulate::import('collections', delay_load = TRUE), silent = TRUE)
  }

  if (is.null(geedim_module)) {
    try(geedim_module <<- reticulate::import("geedim", delay_load = TRUE), silent = TRUE)
  }

  if (is.null(ee_module)) {
    try(ee_module <<- reticulate::import("ee", delay_load = TRUE), silent = TRUE)
  }

  !is.null(geedim_module)
}

#' @importFrom reticulate configure_environment
.onLoad <- function(libname, pkgname) {
  if (!.loadModules()) {
    reticulate::configure_environment(pkgname)
    .loadModules()
  }
}
