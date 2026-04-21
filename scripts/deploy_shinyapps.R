required_env <- c("SHINYAPPS_TOKEN", "SHINYAPPS_SECRET")
missing_env <- required_env[!nzchar(Sys.getenv(required_env, unset = ""))]

if (length(missing_env) > 0) {
  stop(
    sprintf(
      "Missing required shinyapps.io environment variables: %s",
      paste(missing_env, collapse = ", ")
    )
  )
}

app_files <- c(
  "app.R",
  if (dir.exists("app_src")) {
    list.files("app_src", recursive = TRUE, full.names = TRUE)
  },
  if (dir.exists("www")) {
    list.files("www", recursive = TRUE, full.names = TRUE)
  }
)
app_files <- unique(app_files[file.exists(app_files)])

if (!"app.R" %in% app_files) {
  stop("Could not find app.R for shinyapps.io deployment.")
}

resolve_shinyapps_account_name <- function(configured_name, token, secret) {
  client <- rsconnect:::clientForAccount(
    list(
      token = token,
      secret = secret,
      server = "shinyapps.io"
    )
  )

  user_id <- client$currentUser()$id
  accounts <- client$accountsForUser(user_id)
  account_names <- vapply(accounts, function(account) account$name, character(1))

  if (length(account_names) == 0) {
    stop("The supplied shinyapps.io token did not return any publishable accounts.")
  }

  if (nzchar(configured_name)) {
    if (configured_name %in% account_names) {
      return(configured_name)
    }

    stop(
      sprintf(
        "Configured SHINYAPPS_NAME '%s' was not found for this token. Available accounts: %s",
        configured_name,
        paste(account_names, collapse = ", ")
      )
    )
  }

  if (length(account_names) == 1) {
    message(sprintf("Resolved shinyapps.io account name from token: %s", account_names[[1]]))
    return(account_names[[1]])
  }

  stop(
    sprintf(
      "This token can access multiple shinyapps.io accounts (%s). Set SHINYAPPS_NAME to the one you want to deploy to.",
      paste(account_names, collapse = ", ")
    )
  )
}

account_name <- resolve_shinyapps_account_name(
  configured_name = Sys.getenv("SHINYAPPS_NAME", unset = ""),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# Work around renv snapshot validation failures in CI by using rsconnect's
# documented legacy dependency capture path for this deployment script.
Sys.setenv(RSCONNECT_PACKRAT = "TRUE")

rsconnect::setAccountInfo(
  name = account_name,
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files,
  appName = Sys.getenv("SHINYAPPS_APP_NAME", unset = "bdsfinal"),
  appTitle = Sys.getenv("SHINYAPPS_APP_TITLE", unset = "Survival Analysis Studio"),
  appMode = "shiny",
  account = account_name,
  server = "shinyapps.io",
  recordDir = tempdir(),
  launch.browser = FALSE,
  # Work around rsconnect 1.8.0 crashing in verbose mode with the httr2 backend.
  logLevel = "normal"
)
