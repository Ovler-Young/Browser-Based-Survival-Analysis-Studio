required_env <- c("SHINYAPPS_NAME", "SHINYAPPS_TOKEN", "SHINYAPPS_SECRET")
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

rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_NAME"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files,
  appName = Sys.getenv("SHINYAPPS_APP_NAME", unset = "bdsfinal"),
  appTitle = Sys.getenv("SHINYAPPS_APP_TITLE", unset = "Survival Analysis Studio"),
  appMode = "shiny",
  account = Sys.getenv("SHINYAPPS_NAME"),
  server = "shinyapps.io",
  recordDir = tempdir(),
  launch.browser = FALSE,
  logLevel = "verbose"
)
