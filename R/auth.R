#' Set API secret for authorization with the Sendgrid API.
#' @description The `apisecret` must have read access or higher for the `API Keys` permission. See the Settings > API Keys menu in sendgrid to create & modify key permissions.
#' @param apisecret sendgrid api secret. Can be set as Environment variable `SENDGRID_SECRET` or provided interactively via prompt input.
#' @importFrom keyring key_set
#' @export
#' @return None
auth_set <- function(apisecret = Sys.getenv("SENDGRID_SECRET")) {
  stopifnot("This system does not support keyring. See `keyring::has_keyring_support` for details." = keyring::has_keyring_support(),
            "apikey must be provided if not set in .Renviron as `SENDGRID_SECRET`" = nzchar(apisecret))

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

    keyring::key_set(service = "apikey",
                     username = "sendgridr")
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
    httr::add_headers("Authorization" = paste0("Bearer ", auth_key()),
                      "content-type" = "application/json")
  chk <- httr::status_code(httr::GET(tar, ahd))
  return(chk == 200)
}

#' @importFrom keyring key_get
auth_exist <- function() {
  chk <- try(auth_key(), silent = T)
  return(!inherits(chk, "try-error"))
}

#' @importFrom keyring key_get
auth_key <- function() {
  keyring::key_get(service = "apikey",
                   username = "sendgridr")
}

#' Environment to store keyring related variables
kr <- rlang::new_environment(
  data = list(
    keyring = "sendgrid",
    service = "apikey",
    username = "sendgridr"
  )
)

