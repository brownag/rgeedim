#' Install Required Python Modules
#'
#' This function installs the latest `numpy`, `earthengine-api`, and
#' `geedim` modules. The default uses `pip` for package installation. You can 
#' configure custom environments with `pip=FALSE` and additional arguments
#' that are passed to `reticulate::py_install()`.
#' 
#' @param pip Use `pip` package manager? Default: `TRUE`
#' @param system Use a `system()` call to `python -m pip install ...` instead of `reticulate::py_install()`. Default: `FALSE`.
#' @param force Force update (uninstall/reinstall) and ignore existing installed packages? Default: `TRUE`. Applies only to `pip=TRUE`.
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
#' # install with pip
#' 
#' # use virtual environment with default name "r-reticulate"
#' gd_install(pip = FALSE, method = "virtualenv")
#' 
#' # use "conda" environment named "foo"
#' gd_install(pip = FALSE, method = "conda", envname = "foo")
#' 
#' }
gd_install <- function(pip = TRUE, system = FALSE, force = TRUE, ...) {
  
  # alternately, just use a system() call to python -m pip install ...
  if (system && pip) {
    fp <- .find_python()
    if (nchar(fp) > 0) {
      return(invisible(system(
        paste(
          shQuote(fp),
          "-m pip install",
          ifelse(force, "-U --force", ""),
          "geedim earthengine-api numpy"
        )
      )))
    }
  }
  
  invisible(try(reticulate::py_install(
    c("numpy", "earthengine-api", "geedim"),
    pip = pip,
    pip_ignore_installed = force,
    ...
  ),
  silent = FALSE)
  )
}