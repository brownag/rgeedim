#' Install Required Python Modules
#'
#' This function installs the latest `numpy`, `earthengine-api`, and
#' `geedim` modules. The default uses `pip` for package installation. You can 
#' configure custom environments with `pip=FALSE` and additional arguments
#' that are passed to `reticulate::py_install()`.
#' 
#' @param pip Use `pip` package manager? Default: `TRUE`
#' @param ... Additional arguments passed to `reticulate::py_install()`
#' @return `NULL`, or `try-error` (invisibly) on R code execution error.
#' @export
#' @importFrom reticulate py_install
#' @examples
#' \dontrun{
#' 
#' # install with pip
#' gd_install()
#' 
#' # use virtual environment with default name "r-reticulate"
#' gd_install(pip = FALSE, method = "virtualenv")
#' 
#' # use "conda" environment named "foo"
#' gd_install(pip = FALSE, method = "conda", envname = "foo")
#' 
#' }
gd_install <- function(pip = TRUE, ...) {
  invisible(try(reticulate::py_install(c("numpy", "earthengine-api", "geedim"),
                         pip = pip, pip_ignore_installed = TRUE, ...), silent = FALSE))
}