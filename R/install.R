#' Install Required Python Modules
#'
#' This function installs the latest `numpy`, `earthengine-api`, and
#' `geedim` modules. The default uses `pip` for package installation. You can
#' configure custom environments with `pip=FALSE` and additional arguments
#' that are passed to `reticulate::py_install()`.
#'
#' @param pip Use `pip` package manager? Default: `TRUE`. To use a virtual or conda environment specify `method="virtualenv"` or `method="conda"`, respectively. See details.
#' @param system Use a `system()` call to `python -m pip install --user ...` instead of `reticulate::py_install()`. Default: `FALSE`.
#' @param force Force update (uninstall/reinstall) and ignore existing installed packages? Default: `FALSE`. Applies to `pip=TRUE`.
#' @param ... Additional arguments passed to `reticulate::py_install()`
#' @return `NULL`, or `try-error` (invisibly) on R code execution error.
#' @export
#' @details This function provides a basic wrapper around `reticulate::py_install()`, except it defaults to using the Python package manager `pip`. If you specify `method="virtualenv"` or `method="conda` then the default `envname` is `"r-rgeedim"` unless you set it to something else. If an environment of that name does not exist it is created.
#' @importFrom reticulate py_install virtualenv_exists virtualenv_create conda_list conda_create
#' @examples
#' \dontrun{
#'
#' # install with pip (with reticulate)
#' gd_install()
#'
#' # use virtual environment with default name "r-rgeedim"
#' gd_install(method = "virtualenv")
#'
#' # use "conda" environment named "foo"
#' gd_install(method = "conda", envname = "foo")
#'
#' # install with pip (system() call)
#' gd_install(system = TRUE)
#'
#' }
gd_install <- function(pip = TRUE, system = FALSE, force = FALSE, ...) {

  args <- list(...)

  # if virtualenv/conda method specified, pip=FALSE
  if (!is.null(args[["method"]])) {
    pip <- FALSE
  }

  # alternately, just use a system() call to python -m pip install ...
  if (system && pip) {
    fp <- .find_python()
    if (nchar(fp) > 0) {
      return(invisible(system(
        paste(
          shQuote(fp),
          "-m pip install --user",
          ifelse(force, "-U --force", ""),
          "geedim earthengine-api numpy"
        )
      )))
    }
  }
  
  if (!"envname" %in% names(args)) {
    args[["envname"]] <- "r-rgeedim"
  }

  if ("method" %in% names(args)) {

    if (args[["method"]] == "virtualenv") {
      # create suitable virtualenv if does not exist
      if (!reticulate::virtualenv_exists(envname = args[["envname"]])) {
        reticulate::virtualenv_create(envname = args[["envname"]])
      }
    } else if (args[["method"]] == "conda") {
      cl <- try(reticulate::conda_list())
      if (inherits(cl, 'data.frame')) {
        # create suitable conda environment if does not exist
        if (!args[["envname"]] %in% cl$name) {
          reticulate::conda_create(envname = args[["envname"]])
        }
      }
    }
  }

  invisible(try({
    do.call(reticulate::py_install, c(list(
      c("numpy", "earthengine-api", "geedim"),
      pip = pip,
      pip_ignore_installed = force
    ), args))
  }, silent = FALSE))
}
