#' Set API secret for authorization with the Sendgrid API.
#' @description The `apisecret` must have read access or higher for the `API Keys` permission. See the Settings > API Keys menu in sendgrid to create & modify key permissions.
#' @param apisecret sendgrid api secret. Can be set as Environment variable `SENDGRID_SECRET` or provided interactively via prompt input.
#' @export
#' @return None
auth_set <- function(apisecret = Sys.getenv("SENDGRID_SECRET")) {
  stopifnot("apisecret must be provided if not set in .Renviron as `SENDGRID_SECRET`" = !nzchar(apisecret))

  if (!nzchar(Sys.getenv("SENDGRID_SECRET"))) {
    UU::creds_to_renviron(SENDGRID_SECRET = apisecret)
  }
}

#' Check for a working API secret to authorize with Sendgrid.
#'
#' @return TRUE/FALSE check work fine return TRUE.
#' @importFrom usethis ui_info
#' @export
auth_check <- function() {
  if (!auth_exist()) {
    usethis::ui_info("Api key is unset")
    return(FALSE)
  }

  return(auth_check_work())
}

#' @importFrom httr GET add_headers status_code
auth_check_work <- function() {
  tar <- "https://api.sendgrid.com/v3/api_keys"
  ahd <-
    httr::add_headers("Authorization" = paste0("Bearer ", auth_secret()),
                      "content-type" = "application/json")
  chk <- httr::status_code(httr::GET(tar, ahd))
  return(chk == 200)
}


auth_exist <- function() {
  chk <- try(auth_secret(), silent = T)
  return(!inherits(chk, "try-error"))
}

#' Retrieve the SendGrid API Secret for authorization with the Sendgrid API
#' @return The sendgrid API Secret
auth_secret <- function() {
  out <- Sys.getenv("SENDGRID_SECRET")
  stopifnot("SENDGRID_SECRET must be set as an Environment Variable with the Sendgrid API key secret value." = nzchar(out))
  out
}

#' Returns the `SENDGRID_KEY` environment variable for unlocking non-system default keyring on Linux systems.
#'
#' @return \code{chr} The `SENDGRID_KEY` environment variable value
#' @export
#'

auth_key <- function() {
  out <- Sys.getenv("SENDGRID_KEY", unset = "")
  if (!nzchar(out)) {
    stop("Please set `SENDGRID_KEY` in your .Renviron to be used as a password for the sendgrid keyring.")
  }
  return(out)
}

#' Environment to store keyring related variables
kr <- rlang::new_environment(
  data = list(
    keyring = "sendgrid",
    service = "apikey",
    username = "sendgridr"
  )
)

