#' Set API secret for authorization with the Sendgrid API.
#' @description The `apisecret` must have read access or higher for the `API Keys` permission. See the Settings > API Keys menu in sendgrid to create & modify key permissions.
#' @param apisecret sendgrid api secret. Can be set as Environment variable `SENDGRID_SECRET` or provided interactively via prompt input.
#' @importFrom keyring key_set
#' @export
#' @return None
auth_set <- function(apisecret = Sys.getenv("SENDGRID_SECRET")) {
  stopifnot("This system does not support keyring. See `keyring::has_keyring_support` for details." = keyring::has_keyring_support(),
            "apisecret must be provided if not set in .Renviron as `SENDGRID_SECRET`" = !nzchar(apisecret))

  kr_list <- keyring::keyring_list()
  if (!is.data.frame(kr_list) || !as.logical(nrow(kr_list))) {
    keyring::keyring_create(kr$keyring, password = auth_key())
  } else {
    kr$keyring <- kr_list$keyring[1]
  }

  if (missing(apisecret)) {
    usethis::ui_todo(
      "Your API Key is at {usethis::ui_value('https://app.sendgrid.com/settings/api_keys')}"
    )

    Sys.sleep(1)

    keyring::key_set(service = kr$service,
                     username = kr$username)
  } else {
    if (keyring::keyring_is_locked(kr$keyring))
      keyring::keyring_unlock(kr$keyring, password = auth_key())
    keyring::key_set_with_value(kr$service,
                                username = "sendgridr",
                                password = apisecret,
                                keyring = kr$keyring)
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

#' @importFrom keyring key_get
auth_exist <- function() {
  chk <- try(auth_secret(), silent = T)
  return(!inherits(chk, "try-error"))
}

#' Retrieve the SendGrid API Secret for authorization with the Sendgrid API
#' @return The sendgrid API Secret
auth_secret <- function() {
  keyring::key_get(service = kr$service,
                   username = kr$username,
                   keyring = kr$keyring)
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

