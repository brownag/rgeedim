#' Initialize `geedim`
#'
#' Calls `geedim` `Initialize()` method. This method should be called at the beginning of each session.
#'
#' @param private_key_file Optional: Path to JSON file containing client information and private key.
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
    # check possible places people would have a ref to private key
    env_keys <- c('GOOGLE_APPLICATION_CREDENTIALS', 'EE_SERVICE_ACC_PRIVATE_KEY')
    ek <- NULL
    for (e in env_keys) {
      ev <- Sys.getenv(e, unset = NA_character_)
      if (!is.na(ev)) {
        ek <- ev
      }
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
#' Calls `ee.Authenticate(...)` to authenticate with Earth Engine.
#' @details This method should be called once to set up a machine/project with a particular authentication method. The `auth_mode="notebook"` argument is very convenient for interactive R use and will take you to a web page where you can sign into your Google Account and get a token to paste into a console prompt.
#' @param authorization_code Default: `NULL`
#' @param quiet Suppress warnings, errors, messages? Default: `FALSE`
#' @param code_verifier Optional code verifier for security Default: `NULL`
#' @param auth_mode One of `"notebook"`, `"gcloud"`, `"appdefault"` or (default) `NULL` to guess based on the environment
#' @return This function is primarily used for the side-effect of authentication with the 'Google Earth Engine' servers. Invisibly returns `try-error` on error.
#' @export
#' @examples
#' \dontrun{
#' # opens web page to complete authentication/provide authorization code
#' gd_authenticate(auth_mode = "notebook")
#' }
gd_authenticate <- function(authorization_code = NULL, quiet = FALSE, code_verifier = NULL, auth_mode = NULL) {
  invisible(try(gd$utils$Authenticate(
    authorization_code = authorization_code,
    quiet = quiet,
    code_verifier = code_verifier,
    auth_mode = auth_mode
  ), silent = quiet))
}
