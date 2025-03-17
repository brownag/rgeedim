gd <- NULL
collections_module <- NULL

gd_python_envname <- function(envname = "") {
  if (!missing(envname)) {
    reticulate::use_virtualenv(envname, required = FALSE)
    en <- envname
  } else if (is.null(envname)) {
    en <- ""
  } else {
    en <- getOption("rgeedim.python.envname",
                    default = Sys.getenv("R_RGEEDIM_PYTHON_ENVNAME", 
                                         unset = "r-rgeedim"))
  }
  en
}

#' `Module(geedim)` - Get `geedim` Module Instance
#'
#' Gets the `geedim` module instance in use by the package in current **R**/`reticulate` session.
#'
#' @export
geedim <- function() {
  gd
}

#' `gd_version()`: Gets the `geedim` version
#' @rdname geedim
#' @return character. Version Number.
#' @export
#' @importFrom reticulate py_eval
gd_version <- function() {
  try(reticulate::py_eval("version('geedim')"), silent = TRUE)
}

#' Get Earth Engine `Module(earthengine-api)` Instance
#'
#' Gets the `earthengine-api` module instance in use by `geedim` package in current session.
#'
#' @export
earthengine <- function() {
  gd$utils$ee
}

#' @description `gd_ee_version()` Gets the `earthengine-api` version using `importlib.metadata.version()`
#'
#' @rdname earthengine
#' @return character. Version Number.
#' @export
#' @importFrom reticulate py_eval
gd_ee_version <- function() {
  try(reticulate::py_eval("version('earthengine-api')"), silent = TRUE)
}


#' @importFrom reticulate import
#' @importFrom reticulate py_run_string
.loadModules <-  function() {
  # TODO: store modules in package environment not global variables?
  envname <- gd_python_envname()
  if (reticulate::virtualenv_exists(envname)) {
    reticulate::use_virtualenv(envname, required = FALSE)
  } else if (reticulate::condaenv_exists(envname)) {
    reticulate::use_condaenv(envname, required = FALSE)
  }
  
  if (packageVersion("reticulate") >= "1.41.0") {
    reticulate::py_require(c("earthengine-api", "geedim"))
  }
  
  suppressWarnings({
    if (is.null(collections_module)) {
      try(collections_module <<- reticulate::import('collections', delay_load = TRUE), silent = TRUE)
    }
  
    if (is.null(gd)) {
      try(gd <<- reticulate::import("geedim", delay_load = TRUE), silent = TRUE)
    }
  
    # note: requires Python >= 3.8; but is not essential for functioning of package
    try(reticulate::py_run_string("from importlib.metadata import version"), silent = TRUE)
  })
  !is.null(gd)
}

#' @importFrom reticulate py_discover_config
.has_python3 <- function() {
  # get reticulate python information
  # NB: reticulate::py_config() calls configure_environment() etc.
  #     .:. use py_discover_config()
  x <- try(suppressWarnings(reticulate::py_discover_config(use_environment = gd_python_envname())), silent = TRUE)

  # need python 3 for reticulate
  # need python 3.6+ for geedim
  if (length(x) > 0 && !inherits(x, 'try-error')) {
    if (numeric_version(x$version) >= "3.6") {
      return(TRUE)
    } else if (numeric_version(x$version) >= "3.0") {
      # message about geedim dependency? not on load.
      return(FALSE)
    }
  }
  FALSE
}

#' @importFrom reticulate configure_environment
.onLoad <- function(libname, pkgname) {
  if (.has_python3()) {
    if (!.loadModules()) {
      # x <- try(reticulate::configure_environment(pkgname), silent = TRUE)
      # if (!inherits(x, 'try-error')) {
      #  .loadModules()
      # }
    }
  }
}

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  gdv <- suppressWarnings(gd_version())
  gev <- suppressWarnings(gd_ee_version())
  if (inherits(gdv, 'try-error'))
    gdv <- "<Not Found>"
  if (inherits(gev, 'try-error'))
    gev <- "<Not Found>"
  packageStartupMessage(
    "rgeedim v",
    utils::packageVersion("rgeedim"),
    " -- using geedim ",
    gdv,
    " w/ earthengine-api ",
    gev
  )
}

.find_python <- function() {
  # find python
  py_path <- Sys.which("python")
  if (nchar(py_path) == 0) {
    py_path <- Sys.which("python3")
  }
  py_path
}

.inform_missing_module <- function(object, module_name = "geedim") {
  if (is.null(object) || inherits(object, 'try-error')) {
    stop(sprintf("Failed to load '%s' Python module. Check reticulate environment settings and ensure '%s' module is installed. If prompted to set up a default reticulate environment, choose 'Yes'. See `gd_install()` for more details.", module_name, module_name), call. = FALSE)
  }
}
