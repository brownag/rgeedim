#' Initialize `geedim`
#'
#' Calls `geedim` `Initialize()` method. This method should be called at the beginning of each session.
#' @param private_key_file character. Optional: Path to JSON file containing client information and private key. Alternately, the contents of a JSON file. Instead of setting this argument you may specify `EE_SERVICE_ACC_PRIVATE_KEY` environment variable with path to JSON file.
#' @param credentials Default: `NULL` uses Application Default Credentials (ADC) from the environment. Set to `'persistent'` to use credentials stored in the filesystem.
#' @param cloud_api_key An optional API key to use the Cloud API. Default: `NULL`.
#' @param url The base url for the EarthEngine REST API to connect to. Defaults to "High Volume" endpoint: `"https://earthengine-highvolume.googleapis.com"`
#' @param opt_url (deprecated) Use `url`.
#' @param http_transport The HTTP transport method to use when making requests. Default: `NULL`
#' @param project The client project ID or number to use when making API calls. Default: `NULL`
#' @param quiet Suppress error messages on load? Default: `FALSE`
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
                          opt_url = NULL,
                          http_transport = NULL,
                          project = NULL,
                          quiet = FALSE) {

  if (!missing(opt_url)) {
    .Deprecated(msg = "`gd_initialize(opt_url=...)` is deprecated, use `url=` instead")
    url <- opt_url
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
    # check places people would have a ref to service account key
    ek <- NULL
    ev <- Sys.getenv('EE_SERVICE_ACC_PRIVATE_KEY', unset = NA_character_)
    if (!is.na(ev)) {
      ek <- ev
    }
  }
  if (!is.null(ek) && length(ek) == 1) {
    sac <- .load_service_account_credentials(ek, quiet, return_error = TRUE)
    if (inherits(sac, 'try-error')) {
      return(invisible(sac))
    }
    args$credentials <- sac
  } else if (is.null(credentials)) {
    creds_file <- Sys.getenv('GOOGLE_APPLICATION_CREDENTIALS', unset = NA_character_)
    if (!is.na(creds_file) && file.exists(creds_file)) {
      sac <- .load_service_account_credentials(creds_file, quiet)
      if (!is.null(sac)) {
        args$credentials <- sac
      } else {
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
