required_packages <- c(
  "shiny",
  "shinylive",
  "bslib",
  "shinybusy",
  "DT",
  "survival",
  "survminer",
  "ggplot2",
  "ggplotify",
  "patchwork",
  "dplyr",
  "readr",
  "readxl",
  "writexl",
  "glue"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    sprintf(
      "Missing required packages: %s",
      paste(missing_packages, collapse = ", ")
    )
  )
}

site_dir <- file.path(getwd(), "site")
if (dir.exists(site_dir)) {
  unlink(site_dir, recursive = TRUE, force = TRUE)
}

dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

shinylive::export(
  appdir = ".",
  destdir = site_dir,
  quiet = FALSE,
  wasm_packages = FALSE,
  package_cache = TRUE
)

writeLines(
  c(
    "# Redirect legacy shinylive preview paths back to the exported single-app root.",
    "/app_* / 302"
  ),
  con = file.path(site_dir, "_redirects"),
  useBytes = TRUE
)

message(sprintf("Shinylive export complete: %s", site_dir))
