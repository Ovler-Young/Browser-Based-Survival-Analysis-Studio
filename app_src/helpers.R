`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    y
  } else {
    x
  }
}

blank_to_null <- function(x) {
  if (is.null(x) || identical(x, "") || (length(x) == 1 && is.na(x))) {
    NULL
  } else {
    x
  }
}

safe_label <- function(x) {
  gsub("[^A-Za-z0-9_]+", "_", x)
}

format_pct <- function(x) {
  sprintf("%.1f%%", x * 100)
}

capture_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

current_choices <- function(input_value, choices, fallback = "") {
  if (!is.null(input_value) && length(input_value) == 1 && input_value %in% choices) {
    input_value
  } else if (fallback %in% choices) {
    fallback
  } else if (length(choices) > 0) {
    choices[[1]]
  } else {
    ""
  }
}

current_multi_choices <- function(input_value, choices, fallback = character()) {
  picked <- input_value[input_value %in% choices]
  if (length(picked) > 0) {
    picked
  } else {
    fallback[fallback %in% choices]
  }
}

missing_summary_table <- function(df, used_vars) {
  tibble::tibble(
    variable = names(df),
    class = vapply(df, function(x) class(x)[[1]], character(1)),
    missing_n = vapply(df, function(x) sum(is.na(x)), numeric(1)),
    missing_pct = vapply(df, function(x) mean(is.na(x)), numeric(1)),
    unique_non_missing = vapply(df, function(x) length(unique(stats::na.omit(x))), numeric(1)),
    used_in_analysis = names(df) %in% used_vars
  ) |>
    dplyr::mutate(missing_pct_label = format_pct(missing_pct))
}

event_frequency_table <- function(x) {
  vals <- as.character(x)
  vals[is.na(vals)] <- "<NA>"
  freq <- sort(table(vals), decreasing = TRUE)
  tibble::tibble(
    value = names(freq),
    count = as.integer(freq),
    proportion = as.numeric(freq) / sum(freq)
  ) |>
    dplyr::mutate(proportion_label = format_pct(proportion))
}

find_quarto_binary <- function() {
  candidates <- c(
    unname(Sys.which("quarto")),
    Sys.getenv("RSTUDIO_QUARTO", unset = ""),
    Sys.getenv("QUARTO_BIN_PATH", unset = "")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    ""
  } else {
    normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
  }
}

quarto_typst_available <- function() {
  nzchar(find_quarto_binary())
}
