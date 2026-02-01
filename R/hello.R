#' Initialize `geedim`
#'
#' Calls `geedim` `Initialize()` method. This method should be called at the beginning of each session.
#' @param private_key_file character. Optional: Path to JSON file containing service account credentials. (Deprecated in v0.3.0: use `GOOGLE_APPLICATION_CREDENTIALS` environment variable instead. If provided and `GOOGLE_APPLICATION_CREDENTIALS` is not set, the file path will be used to set that environment variable.)
#' @param credentials Default: `NULL` uses Google Application Default Credentials (ADC) to find credentials automatically. Can be set to a pre-existing credential object if needed.
#' @param cloud_api_key An optional API key to use the Cloud API. Default: `NULL`.
#' @param url The base url for the EarthEngine REST API to connect to. Defaults to "High Volume" endpoint: `"https://earthengine-highvolume.googleapis.com"`
#' @param http_transport The HTTP transport method to use when making requests. Default: `NULL`
#' @param project The client project ID or number to use when making API calls. Default: `NULL`
#' @param quiet Suppress error messages on load? Default: `FALSE`
#'
#' @details Authentication is handled automatically by Google Application Default Credentials (ADC). When `credentials` is `NULL` (the default), the underlying Python libraries will automatically search for credentials in the following order:
#' \enumerate{
#'   \item `GOOGLE_APPLICATION_CREDENTIALS` environment variable (if set)
#'   \item User credentials from `gcloud auth application-default login`
#'   \item Attached service account (when running on Google Cloud infrastructure)
#' }
#'
#' The deprecated `private_key_file` parameter is provided for backward compatibility. If specified and `GOOGLE_APPLICATION_CREDENTIALS` is not already set, the file path will be used to set that environment variable for the Python libraries to discover.
#'
#' @return `gd_initialize()`: try-error (invisibly) on error.
#' @export
#' @importFrom reticulate py_run_string
#' @seealso `gd_authenticate()`
#' @examples
#' \dontrun{
#' gd_initialize()
#' }
gd_initialize <- function(private_key_file = NULL,
                          credentials = NULL,
                          cloud_api_key = NULL,
                          url = 'https://earthengine-highvolume.googleapis.com',
                          http_transport = NULL,
                          project = NULL,
                          quiet = FALSE) {

  # Handle deprecated private_key_file parameter
  if (!is.null(private_key_file)) {
    warning("The `private_key_file` argument is deprecated as of rgeedim v0.3.0. ",
            "Use the `GOOGLE_APPLICATION_CREDENTIALS` environment variable instead.",
            call. = FALSE)
    
    # Only use private_key_file if GOOGLE_APPLICATION_CREDENTIALS is not already set
    if (Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS") == "") {
      if (file.exists(private_key_file)) {
        Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = private_key_file)
      } else {
        if (!quiet) {
          warning("private_key_file specified but file does not exist: ", private_key_file,
                  call. = FALSE)
        }
      }
    }
  }

  # python 3.10.x compatibility:
  try(collections_module$Callable <- collections_module$abc$Callable, silent = TRUE)

  res <- .inform_missing_module(gd, "geedim", quiet = quiet)
  if (inherits(res, "try-error")) {
    return(invisible(res))
  }

  eev <- gd$utils$ee$`__version__`

  args <- list(
    cloud_api_key = cloud_api_key,
    opt_url = url,
    http_transport = http_transport,
    project = project
  )

  # Only add credentials if not NULL.
  # When credentials is NULL, Python's ee.Initialize() will automatically use
  # Google Application Default Credentials (ADC) logic via google.auth.default().
  if (!is.null(credentials)) {
    args$credentials <- credentials
  }

  # reticulate does not work w/ opt_ prefix decorators introduced in 0.1.381
  if (!is.null(eev) && eev >= "0.1.381") {
    args <- args[names(args) != "opt_url"] # remove opt_url=
    args <- c(args, list(url = url)) # add url=
  }

  return(invisible(try(do.call(gd$utils$ee$Initialize, args), silent = quiet)))
}


#' @export
#' @param ... Additional arguments passed to `gd_initialize()`
#' @param quiet Suppress error messages? Default: `TRUE`
#' @return `gd_is_initialized()`: logical. `TRUE` if initialized successfully.
#' @rdname gd_initialize
#' @examples
#' gd_is_initialized()
gd_is_initialized <- function(..., quiet = TRUE) {
  return(length(geedim()) > 0 && !inherits(gd_initialize(..., quiet = quiet), "try-error"))
}

#' Authenticate with Google Earth Engine using `gcloud`, "Notebook Authenticator" or other method
#'
#' Calls `ee.Authenticate(...)` to create a local instance of persistent credentials for  Google Earth Engine. These credentials are used on subsequent calls to `ee.Initialize(...)` via `gd_initialize()`.
#'
#' @details This method should be called once to set up a machine/project with a particular authentication method.
#'
#'  - `auth_mode="gcloud"` (default) fetches credentials using `gcloud`. Requires installation of command-line Google Cloud tools; see \url{https://cloud.google.com/cli} for details. This mode will open a web page where you can sign into your Google Account, then a local JSON file will be stored in `gcloud` configuration folder with your credentials. These credentials will be used by any library that requests Application Default Credentials (ADC) which are preferred for long-term storage.
#'
#'  - `auth_mode="notebook"` argument is intended primarily for interactive or other short-term use. This mode will open a web page where you can sign into your Google Account to generate a short-term, revocable token to paste into the console prompt.
#'
#'  - `auth_mode="appdefault"` mode uses locally stored credentials `gcloud` configuration stored in 'application_default_credentials.json' or JSON file specified by `GOOGLE_APPLICATION_CREDENTIALS` environment variable.

#' @param authorization_code Default: `NULL`
#' @param quiet Suppress warnings, errors, messages? Default: `FALSE`
#' @param code_verifier Code verifier (required if `authorization_code` is specified). Default: `NULL`
#' @param auth_mode One of `"notebook"`, `"colab"`, `"gcloud"`, `"gcloud-legacy"` or (default) `NULL` to guess based on the current environment.
#' @param scopes List of scopes to use for authentication. Defaults `NULL` corresponds to `c('https://www.googleapis.com/auth/earthengine', 'https://www.googleapis.com/auth/devstorage.full_control')`
#' @param force Force authentication even if valid credentials exist? Default: `TRUE`
#' @return This function is primarily used for the side-effect of authentication with the 'Google Earth Engine' servers. Invisibly returns `try-error` on error.
#' @export
#' @examples
#' \dontrun{
#' # opens web page to complete authentication/provide authorization code
#' gd_authenticate(auth_mode = "notebook")
#' }
gd_authenticate <- function(authorization_code = NULL,
                            quiet = FALSE,
                            code_verifier = NULL,
                            auth_mode = NULL,
                            scopes = NULL,
                            force = TRUE) {
  .inform_missing_module(gd, "geedim")

  eev <- gd$utils$ee$`__version__`

  args <- list(
    authorization_code = authorization_code,
    quiet = quiet,
    code_verifier = code_verifier,
    auth_mode = auth_mode
  )

  if (!is.null(eev) && eev >= "0.1.312") {
    args <- c(args, list(scopes = scopes))
  }

  if (!is.null(eev) && eev >= "0.1.382") {
    args <- c(args, list(force = force))
  }

  invisible(try(do.call(gd$utils$ee$Authenticate, args), silent = quiet))
}
