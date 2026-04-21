load_example_dataset <- function(name) {
  switch(
    name,
    "lung" = get("lung", envir = asNamespace("survival")),
    "veteran" = get("veteran", envir = asNamespace("survival")),
    "ovarian" = get("ovarian", envir = asNamespace("survival")),
    stop("Unknown example dataset.")
  )
}

default_mapping_for_dataset <- function(name, df) {
  if (identical(name, "lung")) {
    return(
      list(
        time = "time",
        event = "status",
        event_value = "2",
        km_group = "sex",
        km_facet = "ph.ecog",
        cox_predictors = c("age", "sex", "ph.ecog"),
        categorical_predictors = c("sex", "ph.ecog"),
        cox_strata = character(),
        ties_method = "efron",
        reference_levels = c(sex = "1", ph.ecog = "0")
      )
    )
  }

  if (identical(name, "veteran")) {
    return(
      list(
        time = "time",
        event = "status",
        event_value = "1",
        km_group = "trt",
        km_facet = "prior",
        cox_predictors = c("trt", "celltype", "karno"),
        categorical_predictors = c("trt", "celltype"),
        cox_strata = character(),
        ties_method = "efron",
        reference_levels = c(trt = "1", celltype = "adeno")
      )
    )
  }

  if (identical(name, "ovarian")) {
    return(
      list(
        time = "futime",
        event = "fustat",
        event_value = "1",
        km_group = "rx",
        km_facet = "resid.ds",
        cox_predictors = c("rx", "ecog.ps", "age"),
        categorical_predictors = c("rx", "ecog.ps"),
        cox_strata = character(),
        ties_method = "efron",
        reference_levels = c(rx = "1", ecog.ps = "1")
      )
    )
  }

  infer_default_mapping(df)
}

guess_time_column <- function(df) {
  cols <- names(df)
  priority <- grep("time|follow|futime|surv|days|months", cols, ignore.case = TRUE, value = TRUE)
  if (length(priority) > 0) {
    return(priority[[1]])
  }

  numeric_cols <- cols[vapply(df, is.numeric, logical(1))]
  numeric_cols[[1]] %||% cols[[1]]
}

guess_event_column <- function(df, time_col) {
  cols <- setdiff(names(df), time_col)
  priority <- grep("status|event|death|dead|fail|censor|fustat", cols, ignore.case = TRUE, value = TRUE)
  if (length(priority) > 0) {
    return(priority[[1]])
  }

  small_unique <- cols[vapply(
    df[cols],
    function(x) length(unique(stats::na.omit(x))) <= 5,
    logical(1)
  )]
  small_unique[[1]] %||% cols[[1]]
}

guess_event_value <- function(x) {
  vals <- unique(as.character(stats::na.omit(x)))
  if ("1" %in% vals && "0" %in% vals) {
    return("1")
  }
  if ("2" %in% vals && "1" %in% vals) {
    return("2")
  }
  sort(vals, na.last = TRUE)[[length(vals)]]
}

infer_default_mapping <- function(df) {
  time_col <- guess_time_column(df)
  event_col <- guess_event_column(df, time_col)

  remaining <- setdiff(names(df), c(time_col, event_col))
  small_unique <- remaining[vapply(
    df[remaining],
    function(x) {
      n_unique <- length(unique(stats::na.omit(x)))
      n_unique >= 2 && n_unique <= 6
    },
    logical(1)
  )]

  numeric_predictors <- remaining[vapply(
    df[remaining],
    function(x) is.numeric(x) && length(unique(stats::na.omit(x))) > 6,
    logical(1)
  )]

  km_group <- small_unique[[1]] %||% ""
  km_facet <- small_unique[[2]] %||% ""

  predictors <- unique(c(km_group, numeric_predictors[[1]], small_unique[[2]]))
  predictors <- predictors[!is.na(predictors) & nzchar(predictors)]
  predictors <- predictors[predictors %in% remaining]

  categorical_predictors <- predictors[vapply(
    df[predictors],
    function(x) !is.numeric(x) || length(unique(stats::na.omit(x))) <= 6,
    logical(1)
  )]

  reference_levels <- setNames(
    vapply(
      categorical_predictors,
      function(var) {
        vals <- sort(unique(as.character(stats::na.omit(df[[var]]))))
        vals[[1]] %||% ""
      },
      character(1)
    ),
    categorical_predictors
  )

  list(
    time = time_col,
    event = event_col,
    event_value = guess_event_value(df[[event_col]]),
    km_group = km_group,
    km_facet = km_facet,
    cox_predictors = predictors,
    categorical_predictors = categorical_predictors,
    cox_strata = character(),
    ties_method = "efron",
    reference_levels = reference_levels
  )
}

build_formula_text <- function(time_col, predictors, strata_vars) {
  rhs_terms <- c(predictors, if (length(strata_vars) > 0) sprintf("strata(%s)", strata_vars))
  rhs <- if (length(rhs_terms) == 0) "1" else paste(rhs_terms, collapse = " + ")
  sprintf("Surv(%s, analysis_event) ~ %s", time_col, rhs)
}

build_cox_rhs_text <- function(predictors, strata_vars) {
  rhs_terms <- c(predictors, if (length(strata_vars) > 0) sprintf("strata(%s)", strata_vars))
  if (length(rhs_terms) == 0) "1" else paste(rhs_terms, collapse = " + ")
}
