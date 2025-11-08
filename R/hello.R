#' Initialize `geedim`
#'
#' Calls `geedim` `Initialize()` method. This method should be called at the beginning of each session.
#' @param private_key_file character. Optional: Path to JSON file containing service account credentials. This is the recommended way to provide explicit service account authentication. (Deprecated: use `GOOGLE_APPLICATION_CREDENTIALS` environment variable instead.)
#' @param credentials Default: `NULL` uses Application Default Credentials (ADC) from the environment. Can also be set to `'persistent'` to use credentials stored in the filesystem.
#' @param cloud_api_key An optional API key to use the Cloud API. Default: `NULL`.
#' @param url The base url for the EarthEngine REST API to connect to. Defaults to "High Volume" endpoint: `"https://earthengine-highvolume.googleapis.com"`
#' @param http_transport The HTTP transport method to use when making requests. Default: `NULL`
#' @param project The client project ID or number to use when making API calls. Default: `NULL`
#' @param quiet Suppress error messages on load? Default: `FALSE`
#'
#' @details Authentication priority (in order):
#' \enumerate{
#'   \item `private_key_file` parameter (explicit service account JSON file; deprecated in v0.3.0)
#'   \item `GOOGLE_APPLICATION_CREDENTIALS` environment variable (service account JSON or ADC)
#'   \item Application Default Credentials (ADC) via `google.auth.default()`
#' }
#'
#' @return `gd_initialize()`: try-error (invisibly) on error.
#' @export
#' @importFrom reticulate py_run_string
#' @importFrom jsonlite read_json
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

  if (!is.null(private_key_file)) {
    message("Note: The `private_key_file` argument is deprecated as of rgeedim v0.3.0. ",
            "Use the `GOOGLE_APPLICATION_CREDENTIALS` environment variable instead.")
  }
  
  ev <- Sys.getenv('EE_SERVICE_ACC_PRIVATE_KEY', unset = NA_character_)
  if (!is.na(ev)) {
    message("Note: The `EE_SERVICE_ACC_PRIVATE_KEY` environment variable is deprecated as of rgeedim v0.3.0. ",
            "Use the standard `GOOGLE_APPLICATION_CREDENTIALS` environment variable instead.")
  }

  # python 3.10.x compatibility:
  try(collections_module$Callable <- collections_module$abc$Callable, silent = TRUE)

  .inform_missing_module(gd, "geedim")

  eev <- gd$utils$ee$`__version__`

  args <- list(
    cloud_api_key = cloud_api_key,
    opt_url = url,
    http_transport = http_transport,
    project = project
  )
  
  # Only add credentials if not NULL (to allow ADC when credentials is NULL)
  if (!is.null(credentials)) {
    args <- c(list(credentials = credentials), args)
  }

  # reticulate does not work w/ opt_ prefix decorators introduced in 0.1.381
  if (!is.null(eev) && eev >= "0.1.381") {
    args <- args[names(args) != "opt_url"] # remove opt_url=
    args <- c(args, list(url = url)) # add url=
  }

  .load_service_account_credentials <- function(key_source, quiet = FALSE, return_error = FALSE) {
    result <- tryCatch({
      if (file.exists(key_source) && grepl("\\.json$", key_source, ignore.case = TRUE)) {
        kd <- jsonlite::read_json(key_source)
      } else {
        kd <- jsonlite::parse_json(key_source)
      }
      
      if (!is.null(kd[['client_email']]) && !is.null(kd[['private_key']])) {
        sac <- gd$utils$ee$ServiceAccountCredentials(kd[['client_email']],
                                                     key_data = kd[['private_key']])
        return(sac)
      } else {
        stop("Invalid service account JSON: missing required fields", call. = FALSE)
      }
    }, error = function(e) {
      if (return_error) {
        return(try(stop(e$message), silent = TRUE))
      } else {
        if (!quiet) {
          message(sprintf("Warning: Failed to load service account credentials from %s: %s", 
                         key_source, e$message))
        }
        return(NULL)
      }
    })
    return(result)
  }

  if (!is.null(private_key_file) && file.exists(private_key_file)) {
    ek <- private_key_file
  } else {
    ek <- NULL
  }
  if (!is.null(ek) && length(ek) == 1) {
    sac <- .load_service_account_credentials(ek, quiet, return_error = TRUE)
    if (inherits(sac, 'try-error')) {
      return(invisible(sac))
    }
    if (!is.null(sac)) {
      args$credentials <- sac
    }
  }
  
  # Try to load from GOOGLE_APPLICATION_CREDENTIALS if credentials not yet set
  if (is.null(credentials) && is.null(args$credentials)) {
    creds_file <- Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS', unset = NA_character_)
    if (!is.na(creds_file) && file.exists(creds_file)) {
      sac <- .load_service_account_credentials(creds_file, quiet, return_error = FALSE)
      if (!is.null(sac)) {
        args$credentials <- sac
      } else {
        # Fall back to ADC
        tryCatch({
          adc_result <- google_auth_module$default()
          args$credentials <- adc_result[[1]]
          if (is.null(project)) {
            args$project <- adc_result[[2]]
          }
        }, error = function(e) {
          if (!quiet) {
            message(sprintf("Warning: Failed to load ADC: %s", e$message))
          }
        })
      }
    }
  }
  return(invisible(try(do.call(gd$utils$ee$Initialize, args), silent = quiet)))
}


#' @export
#' @param ... Additional arguments passed to `gd_initialize()`
#' @return `gd_is_initialized()`: logical. `TRUE` if initialized successfully.
#' @rdname gd_initialize
#' @examples
#' gd_is_initialized()
gd_is_initialized <- function(...) {
  return(length(geedim()) > 0 && !inherits(gd_initialize(...), "try-error"))
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
