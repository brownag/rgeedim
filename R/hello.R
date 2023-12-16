#' Initialize `geedim`
#'
#' Calls `geedim` `Initialize()` method. This method should be called at the beginning of each session.
#'
#' @param private_key_file character. Optional: Path to JSON file containing client information and private key. Alternately, the contents of a JSON file. Instead of setting this argument you may specify `EE_SERVICE_ACC_PRIVATE_KEY` environment variable with path to JSON file.
#' @param opt_url Base URL for API requests; defaults to "High Volume": `"https://earthengine-highvolume.googleapis.com"`
#' @param quiet Suppress error messages on load? Default: `FALSE`
#'
#' @return `gd_initialize()`: try-error (invisibly) on error.
#' @export
#' @importFrom reticulate py_run_string
#' @importFrom jsonlite read_json
#' @seealso `gd_authenticate()`
#' @examples
#' \donttest{
#' gd_initialize()
#' }
gd_initialize <- function(private_key_file = NULL, opt_url = 'https://earthengine-highvolume.googleapis.com', quiet = TRUE) {
  # python 3.10.x compatibility:
  try(collections_module$Callable <- collections_module$abc$Callable, silent = TRUE)

  if (!is.null(private_key_file) && file.exists(private_key_file)) {
    ek <- private_key_file
  } else {
    # check places people would have a ref to service account key
    ek <- NULL
    ev <- Sys.getenv('EE_SERVICE_ACC_PRIVATE_KEY', unset = NA_character_)
    if (!is.na(ev)) {
      ek <- ev
    }
  }
  if (!is.null(ek) && length(ek) == 1) {

    if (file.exists(ek) && grepl("\\.json$", ek[1], ignore.case = TRUE)) {
      kd <- jsonlite::read_json(ek)
    } else {
      kd <- jsonlite::parse_json(ek)
    }

    sac <- try(gd$utils$ee$ServiceAccountCredentials(kd[['client_email']],
                                                     key_data = kd[['private_key']]),
               silent = quiet)

    if (inherits(sac, 'try-error')) {
      return(invisible(sac))
    }

    return(invisible(try(gd$utils$ee$Initialize(sac, opt_url = opt_url), silent = quiet)))
  } else {
    return(invisible(try(gd$utils$ee$Initialize(opt_url = opt_url), silent = quiet)))
  }
}

#' @export
#' @return `gd_is_initialized()`: logical. `TRUE` if initialized successfully.
#' @rdname gd_initialize
#' @examples
#' gd_is_initialized()
gd_is_initialized <- function() {
  return(length(geedim()) > 0 && !inherits(gd_initialize(), "try-error"))
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
