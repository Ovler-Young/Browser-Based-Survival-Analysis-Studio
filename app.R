library(shiny)
library(bslib)
library(shinybusy)
library(DT)
library(survival)
library(survminer)
library(ggplot2)
library(ggplotify)
library(patchwork)
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(glue)

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

build_formula_text <- function(time_col, predictors, strata_vars) {
  rhs_terms <- c(predictors, if (length(strata_vars) > 0) sprintf("strata(%s)", strata_vars))
  rhs <- if (length(rhs_terms) == 0) "1" else paste(rhs_terms, collapse = " + ")
  sprintf("Surv(%s, analysis_event) ~ %s", time_col, rhs)
}

build_cox_rhs_text <- function(predictors, strata_vars) {
  rhs_terms <- c(predictors, if (length(strata_vars) > 0) sprintf("strata(%s)", strata_vars))
  if (length(rhs_terms) == 0) "1" else paste(rhs_terms, collapse = " + ")
}

ui <- page_sidebar(
  title = div(
    class = "app-title-wrap",
    h2("Browser-Based Survival Analysis Studio"),
    p(
      class = "lead-text",
      "Kaplan-Meier curves, log-rank tests, and Cox models that run entirely in your browser via shinylive + webR."
    )
  ),
  theme = bs_theme(
    version = 5,
    bg = "#f6f1e8",
    fg = "#1d2428",
    primary = "#1e5f74",
    secondary = "#d8b26e",
    success = "#3b7a57",
    base_font = font_google("Public Sans"),
    heading_font = font_google("Space Grotesk"),
    code_font = font_google("JetBrains Mono")
  ),
  sidebar = sidebar(
    width = 390,
    div(
      class = "sidebar-note",
      strong("Privacy first."),
      " Uploaded data stays in the browser. No server-side processing is required."
    ),
    div(
      class = "sidebar-note warm",
      strong("First load tip."),
      " The initial browser-side package download can take 10-20 seconds on slower connections."
    ),
    shinybusy::add_busy_spinner(
      spin = "double-bounce",
      color = "#1e5f74",
      timeout = 80,
      position = "full-page"
    ),
    selectInput(
      "data_mode",
      "Data source",
      choices = c("Example dataset", "Upload file"),
      selected = "Example dataset"
    ),
    conditionalPanel(
      condition = "input.data_mode === 'Example dataset'",
      selectInput(
        "example_dataset",
        "Built-in example",
        choices = c("Lung cancer (`survival::lung`)" = "lung", "Veteran (`survival::veteran`)" = "veteran", "Ovarian (`survival::ovarian`)" = "ovarian"),
        selected = "lung"
      )
    ),
    conditionalPanel(
      condition = "input.data_mode === 'Upload file'",
      fileInput(
        "data_file",
        "Upload CSV or Excel",
        accept = c(".csv", ".xlsx")
      )
    ),
    hr(),
    uiOutput("mapping_ui"),
    hr(),
    h4("Downloads"),
    downloadButton("download_analysis_csv", "Filtered analysis data (.csv)", class = "btn-primary w-100"),
    br(),
    br(),
    downloadButton("download_results_xlsx", "Results workbook (.xlsx)", class = "btn-outline-primary w-100"),
    br(),
    br(),
    downloadButton("download_km_plot", "Kaplan-Meier plot (.png)", class = "btn-outline-primary w-100"),
    br(),
    br(),
    downloadButton("download_forest_plot", "Forest plot (.png)", class = "btn-outline-primary w-100"),
    br(),
    br(),
    downloadButton("download_repro_script", "Reproducible script (.R)", class = "btn-outline-primary w-100")
  ),
  navset_card_pill(
    id = "main_tabs",
    nav_panel(
      "Overview",
      layout_columns(
        card(
          full_screen = FALSE,
          card_header("Analysis snapshot"),
          uiOutput("snapshot_cards")
        ),
        card(
          full_screen = FALSE,
          card_header("Current configuration"),
          uiOutput("configuration_summary")
        ),
        col_widths = c(7, 5)
      ),
      card(
        full_screen = TRUE,
        card_header("Data preview"),
        DTOutput("data_preview")
      ),
      card(
        full_screen = FALSE,
        card_header("Dataset validation"),
        uiOutput("validation_messages")
      )
    ),
    nav_panel(
      "Kaplan-Meier",
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Overall Kaplan-Meier curve"),
          plotOutput("overall_km_plot", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Stratified Kaplan-Meier curve"),
          plotOutput("stratified_km_plot", height = "720px")
        ),
        col_widths = c(6, 6)
      ),
      layout_columns(
        card(
          full_screen = FALSE,
          card_header("Median survival summary"),
          DTOutput("median_survival_table")
        ),
        card(
          full_screen = FALSE,
          card_header("Log-rank test"),
          uiOutput("logrank_status"),
          DTOutput("logrank_table")
        ),
        col_widths = c(7, 5)
      )
    ),
    nav_panel(
      "Cox Model",
      layout_columns(
        card(
          full_screen = FALSE,
          card_header("Model fit and concordance"),
          uiOutput("cox_status"),
          DTOutput("cox_results_table")
        ),
        card(
          full_screen = TRUE,
          card_header("Hazard ratio forest plot"),
          plotOutput("forest_plot", height = "560px")
        ),
        col_widths = c(5, 7)
      ),
      layout_columns(
        card(
          full_screen = FALSE,
          card_header("Proportional hazards test"),
          DTOutput("ph_table")
        ),
        card(
          full_screen = FALSE,
          card_header("Events-per-variable guidance"),
          uiOutput("epv_message")
        ),
        col_widths = c(8, 4)
      ),
      accordion(
        accordion_panel(
          "Schoenfeld residual plots",
          plotOutput("schoenfeld_plot_grid", height = "760px")
        )
      )
    ),
    nav_panel(
      "Diagnostics",
      layout_columns(
        card(
          full_screen = FALSE,
          card_header("Filtering summary"),
          DTOutput("filter_summary_table")
        ),
        card(
          full_screen = FALSE,
          card_header("Event values and frequencies"),
          DTOutput("event_frequency_table")
        ),
        col_widths = c(6, 6)
      ),
      card(
        full_screen = TRUE,
        card_header("Missing-data summary"),
        DTOutput("missing_data_table")
      )
    )
  ),
  tags$style(HTML("
    body {
      background:
        radial-gradient(circle at top left, rgba(216,178,110,0.18), transparent 35%),
        radial-gradient(circle at top right, rgba(30,95,116,0.14), transparent 30%),
        #f6f1e8;
    }
    .app-title-wrap {
      padding-top: 0.2rem;
    }
    .app-title-wrap h2 {
      margin-bottom: 0.2rem;
      letter-spacing: -0.03em;
    }
    .lead-text {
      margin-bottom: 0;
      max-width: 58rem;
      color: #44525a;
    }
    .sidebar-note {
      border-left: 4px solid #1e5f74;
      background: rgba(255,255,255,0.72);
      border-radius: 0.8rem;
      padding: 0.8rem 0.9rem;
      margin-bottom: 0.8rem;
    }
    .sidebar-note.warm {
      border-left-color: #d8b26e;
    }
    .summary-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
      gap: 0.9rem;
    }
    .metric-card {
      background: linear-gradient(180deg, rgba(255,255,255,0.94), rgba(255,255,255,0.76));
      border: 1px solid rgba(30,95,116,0.12);
      border-radius: 1rem;
      padding: 1rem;
      min-height: 118px;
    }
    .metric-label {
      font-size: 0.85rem;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      color: #5f6a71;
      margin-bottom: 0.4rem;
    }
    .metric-value {
      font-size: 1.8rem;
      font-family: 'Space Grotesk', sans-serif;
      line-height: 1.1;
    }
    .metric-sub {
      margin-top: 0.4rem;
      color: #5f6a71;
      font-size: 0.92rem;
    }
    .config-list dt {
      color: #44525a;
      font-weight: 700;
      margin-top: 0.65rem;
    }
    .config-list dd {
      margin-left: 0;
      margin-bottom: 0;
      color: #1d2428;
    }
    .status-block {
      border-radius: 0.85rem;
      padding: 0.9rem 1rem;
      margin-bottom: 1rem;
      background: rgba(30,95,116,0.08);
      border: 1px solid rgba(30,95,116,0.14);
    }
    .status-block.warn {
      background: rgba(216,178,110,0.18);
      border-color: rgba(216,178,110,0.45);
    }
    .status-block.error {
      background: rgba(179,67,54,0.12);
      border-color: rgba(179,67,54,0.28);
    }
  "))
)

server <- function(input, output, session) {
  current_dataset <- reactive({
    if (identical(input$data_mode, "Upload file")) {
      req(input$data_file)
      ext <- tolower(tools::file_ext(input$data_file$name))

      if (identical(ext, "csv")) {
        df <- readr::read_csv(input$data_file$datapath, show_col_types = FALSE)
        return(
          list(
            data = as.data.frame(df),
            source_label = input$data_file$name,
            source_type = "upload_csv",
            source_name = input$data_file$name
          )
        )
      }

      if (identical(ext, "xlsx")) {
        df <- readxl::read_excel(input$data_file$datapath, sheet = 1)
        return(
          list(
            data = as.data.frame(df),
            source_label = input$data_file$name,
            source_type = "upload_xlsx",
            source_name = input$data_file$name
          )
        )
      }

      stop("Unsupported file format.")
    }

    list(
      data = as.data.frame(load_example_dataset(input$example_dataset)),
      source_label = sprintf("survival::%s", input$example_dataset),
      source_type = "example",
      source_name = input$example_dataset
    )
  })

  default_mapping <- reactive({
    dataset <- current_dataset()
    default_mapping_for_dataset(dataset$source_name, dataset$data)
  })

  output$mapping_ui <- renderUI({
    df <- current_dataset()$data
    defaults <- default_mapping()
    cols <- names(df)
    non_event_cols <- setdiff(cols, c(defaults$time, defaults$event))
    predictor_choices <- setdiff(cols, c(input$time_col %||% defaults$time, input$event_col %||% defaults$event))

    time_selected <- current_choices(input$time_col, cols, defaults$time)
    event_selected <- current_choices(input$event_col, cols, defaults$event)

    event_values <- unique(as.character(stats::na.omit(df[[event_selected]])))
    event_values <- sort(event_values)
    event_value_selected <- current_choices(input$event_value, event_values, defaults$event_value)

    km_choices <- c("None" = "", predictor_choices)
    km_group_selected <- current_choices(input$km_group, km_choices, defaults$km_group)
    km_facet_selected <- current_choices(input$km_facet, km_choices, defaults$km_facet)

    cox_predictors_selected <- current_multi_choices(input$cox_predictors, predictor_choices, defaults$cox_predictors)
    categorical_choices <- cox_predictors_selected
    categorical_selected <- current_multi_choices(
      input$categorical_predictors,
      categorical_choices,
      defaults$categorical_predictors
    )
    strata_selected <- current_multi_choices(input$cox_strata, predictor_choices, defaults$cox_strata)
    ties_selected <- current_choices(input$ties_method, c("efron", "breslow", "exact"), defaults$ties_method)

    tagList(
      h4("Analysis mapping"),
      selectInput("time_col", "Time column", choices = cols, selected = time_selected),
      selectInput("event_col", "Event column", choices = cols, selected = event_selected),
      selectInput("event_value", "Value treated as the event", choices = event_values, selected = event_value_selected),
      selectInput("km_group", "KM grouping variable", choices = km_choices, selected = km_group_selected),
      selectInput("km_facet", "Optional KM facet variable", choices = km_choices, selected = km_facet_selected),
      selectInput(
        "cox_predictors",
        "Cox predictors",
        choices = predictor_choices,
        selected = cox_predictors_selected,
        multiple = TRUE
      ),
      selectInput(
        "categorical_predictors",
        "Treat selected Cox predictors as categorical",
        choices = categorical_choices,
        selected = categorical_selected,
        multiple = TRUE
      ),
      uiOutput("reference_levels_ui"),
      selectInput(
        "cox_strata",
        "Cox strata variables",
        choices = predictor_choices,
        selected = strata_selected,
        multiple = TRUE
      ),
      p(
        class = "text-muted small",
        "Strata variables are used for stratification and are not estimated as coefficients in the HR table or forest plot."
      ),
      selectInput(
        "ties_method",
        "Cox ties handling",
        choices = c("Efron" = "efron", "Breslow" = "breslow", "Exact" = "exact"),
        selected = ties_selected
      )
    )
  })

  output$reference_levels_ui <- renderUI({
    df <- current_dataset()$data
    defaults <- default_mapping()
    categorical_vars <- intersect(input$categorical_predictors %||% character(), input$cox_predictors %||% character())

    if (length(categorical_vars) == 0) {
      return(
        div(
          class = "text-muted small",
          "Reference-level controls will appear here after you mark Cox predictors as categorical."
        )
      )
    }

    controls <- lapply(categorical_vars, function(var) {
      values <- sort(unique(as.character(stats::na.omit(df[[var]]))))
      control_id <- paste0("ref_", safe_label(var))
      selected <- current_choices(input[[control_id]], values, defaults$reference_levels[[var]] %||% values[[1]])
      selectInput(control_id, sprintf("Reference level for %s", var), choices = values, selected = selected)
    })

    tagList(
      h5("Reference levels"),
      controls
    )
  })

  analysis_base <- reactive({
    dataset <- current_dataset()
    df <- as.data.frame(dataset$data)
    req(input$time_col, input$event_col, input$event_value)

    km_group <- blank_to_null(input$km_group)
    km_facet <- blank_to_null(input$km_facet)
    cox_predictors <- input$cox_predictors %||% character()
    cox_strata <- input$cox_strata %||% character()

    used_vars <- unique(c(input$time_col, input$event_col, km_group, km_facet, cox_predictors, cox_strata))
    used_vars <- used_vars[nzchar(used_vars)]

    missing_summary <- missing_summary_table(df, used_vars)
    event_freq <- event_frequency_table(df[[input$event_col]])

    errors <- character()
    warnings <- character()

    raw_time <- df[[input$time_col]]
    time_chr <- as.character(raw_time)
    time_chr[trimws(time_chr) == ""] <- NA_character_
    time_num <- suppressWarnings(as.numeric(time_chr))
    coercion_failures <- sum(is.na(time_num) & !is.na(time_chr))
    if (coercion_failures > 0) {
      errors <- c(errors, sprintf("The selected time column could not be safely converted to numeric for %s rows.", coercion_failures))
    }

    raw_event_chr <- as.character(df[[input$event_col]])
    raw_event_chr[trimws(raw_event_chr) == ""] <- NA_character_
    mapped_event <- ifelse(is.na(raw_event_chr), NA_integer_, ifelse(raw_event_chr == input$event_value, 1L, 0L))

    missing_rows <- if (length(used_vars) > 0) {
      !stats::complete.cases(df[used_vars])
    } else {
      rep(FALSE, nrow(df))
    }
    missing_rows <- missing_rows | is.na(time_num) | is.na(mapped_event)
    nonpositive_rows <- !is.na(time_num) & time_num <= 0
    keep_rows <- !(missing_rows | nonpositive_rows)

    analysis_df <- df[keep_rows, , drop = FALSE]
    analysis_df[[input$time_col]] <- time_num[keep_rows]
    analysis_df$analysis_event <- mapped_event[keep_rows]

    if (nrow(analysis_df) == 0) {
      errors <- c(errors, "No rows remain after filtering missing values and nonpositive follow-up time.")
    } else {
      event_levels <- sort(unique(analysis_df$analysis_event))
      if (!identical(event_levels, c(0L, 1L))) {
        errors <- c(errors, "The mapped event coding must yield both censored and event observations after filtering.")
      }
    }

    if (!is.null(km_group) && length(unique(stats::na.omit(analysis_df[[km_group]]))) > 12) {
      warnings <- c(warnings, sprintf("KM grouping variable `%s` has many unique values; the stratified plot may be crowded.", km_group))
    }

    filter_summary <- tibble::tibble(
      metric = c("Rows in source data", "Rows removed for missingness", "Rows removed for time <= 0", "Rows analyzed"),
      count = c(
        nrow(df),
        sum(missing_rows),
        sum(nonpositive_rows & !missing_rows),
        sum(keep_rows)
      )
    )

    list(
      dataset = dataset,
      raw_data = df,
      used_vars = used_vars,
      missing_summary = missing_summary,
      event_frequency = event_freq,
      filter_summary = filter_summary,
      analysis_data = analysis_df,
      errors = unique(errors),
      warnings = unique(warnings),
      selected = list(
        time = input$time_col,
        event = input$event_col,
        event_value = input$event_value,
        km_group = km_group,
        km_facet = km_facet,
        cox_predictors = cox_predictors,
        cox_strata = cox_strata,
        ties_method = input$ties_method %||% "efron"
      )
    )
  })

  cox_prepared <- reactive({
    base <- analysis_base()
    data <- base$analysis_data
    categorical_vars <- intersect(input$categorical_predictors %||% character(), base$selected$cox_predictors)
    ref_levels <- list()
    warnings <- character()

    for (var in categorical_vars) {
      data[[var]] <- factor(as.character(data[[var]]))
      ref_id <- paste0("ref_", safe_label(var))
      ref_value <- input[[ref_id]]
      ref_levels[[var]] <- ref_value

      if (!is.null(ref_value) && ref_value %in% levels(data[[var]])) {
        data[[var]] <- stats::relevel(data[[var]], ref = ref_value)
      } else if (!is.null(ref_value) && nzchar(ref_value)) {
        warnings <- c(warnings, sprintf("Reference level `%s` was not available for `%s` after filtering.", ref_value, var))
      }
    }

    if (length(base$selected$cox_predictors) == 0) {
      epv <- NA_real_
      coefficient_count <- 0L
    } else {
      coefficient_count <- tryCatch({
        mm <- stats::model.matrix(
          stats::as.formula(paste("~", paste(base$selected$cox_predictors, collapse = " + "))),
          data = data
        )
        mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
        ncol(mm)
      }, error = function(e) {
        length(base$selected$cox_predictors)
      })

      epv <- if (coefficient_count > 0) {
        sum(data$analysis_event) / coefficient_count
      } else {
        NA_real_
      }
    }

    list(
      data = data,
      categorical_predictors = categorical_vars,
      reference_levels = ref_levels,
      epv = epv,
      coefficient_count = coefficient_count,
      warnings = unique(warnings)
    )
  })

  overall_fit <- reactive({
    base <- analysis_base()
    req(length(base$errors) == 0)
    fit_formula <- stats::as.formula(sprintf("Surv(%s, analysis_event) ~ 1", base$selected$time))
    fit <- survival::survfit(fit_formula, data = base$analysis_data)
    fit$call$formula <- fit_formula
    fit
  })

  overall_km_plot_obj <- reactive({
    fit <- overall_fit()
    base <- analysis_base()
    survminer::ggsurvplot(
      fit,
      data = base$analysis_data,
      risk.table = TRUE,
      conf.int = TRUE,
      surv.median.line = "hv",
      ggtheme = theme_minimal(base_size = 12),
      palette = c("#1e5f74")
    )
  })

  stratified_fit <- reactive({
    base <- analysis_base()
    req(length(base$errors) == 0)
    req(!is.null(base$selected$km_group))

    fit_formula <- stats::as.formula(sprintf("Surv(%s, analysis_event) ~ `%s`", base$selected$time, base$selected$km_group))
    fit <- survival::survfit(fit_formula, data = base$analysis_data)
    fit$call$formula <- fit_formula
    fit
  })

  stratified_km_plot_obj <- reactive({
    base <- analysis_base()
    req(length(base$errors) == 0)
    req(!is.null(base$selected$km_group))

    fit <- stratified_fit()
    facet_var <- base$selected$km_facet
    analysis_df <- base$analysis_data

    if (!is.null(facet_var)) {
      return(
        survminer::ggsurvplot_facet(
          fit,
          data = analysis_df,
          facet.by = facet_var,
          risk.table = TRUE,
          conf.int = TRUE,
          pval = TRUE,
          ggtheme = theme_minimal(base_size = 12),
          palette = c("#1e5f74", "#d08c33", "#7c9c5d", "#b34d36", "#587792")
        )
      )
    }

    survminer::ggsurvplot(
      fit,
      data = analysis_df,
      risk.table = TRUE,
      conf.int = TRUE,
      pval = TRUE,
      surv.median.line = "hv",
      ggtheme = theme_minimal(base_size = 12),
      palette = c("#1e5f74", "#d08c33", "#7c9c5d", "#b34d36", "#587792")
    )
  })

  median_survival_table_data <- reactive({
    base <- analysis_base()
    req(length(base$errors) == 0)

    if (!is.null(base$selected$km_group)) {
      med <- survminer::surv_median(stratified_fit())
      med$strata <- med$strata %||% "Overall"
      return(as.data.frame(med))
    }

    med <- survminer::surv_median(overall_fit())
    out <- as.data.frame(med)
    out$strata <- "Overall"
    out
  })

  logrank_table_data <- reactive({
    base <- analysis_base()
    req(length(base$errors) == 0)
    req(!is.null(base$selected$km_group))

    fit <- survival::survdiff(
      stats::as.formula(sprintf("Surv(%s, analysis_event) ~ `%s`", base$selected$time, base$selected$km_group)),
      data = base$analysis_data
    )

    tibble::tibble(
      chisq = unname(fit$chisq),
      df = max(length(fit$n) - 1, 1),
      p_value = stats::pchisq(unname(fit$chisq), df = max(length(fit$n) - 1, 1), lower.tail = FALSE)
    )
  })

  cox_result <- reactive({
    base <- analysis_base()
    prep <- cox_prepared()

    if (length(base$errors) > 0) {
      return(list(ok = FALSE, error = paste(base$errors, collapse = " ")))
    }

    if (length(base$selected$cox_predictors) == 0 && length(base$selected$cox_strata) == 0) {
      return(list(ok = FALSE, error = "Select at least one Cox predictor or one Cox strata variable."))
    }

    formula_text <- build_formula_text(
      time_col = base$selected$time,
      predictors = base$selected$cox_predictors,
      strata_vars = base$selected$cox_strata
    )

    fit_capture <- tryCatch(
      capture_warnings(
        survival::coxph(
          stats::as.formula(formula_text),
          data = prep$data,
          ties = base$selected$ties_method,
          model = TRUE,
          x = TRUE,
          y = TRUE
        )
      ),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )

    if (!is.null(fit_capture$error)) {
      return(
        list(
          ok = FALSE,
          error = sprintf("Cox model failed: %s", fit_capture$error),
          warnings = c(base$warnings, prep$warnings)
        )
      )
    }

    fit <- fit_capture$value
    fit_summary <- summary(fit)
    coef_df <- if (nrow(fit_summary$coefficients) > 0) {
      tibble::tibble(
        term = rownames(fit_summary$coefficients),
        coefficient = fit_summary$coefficients[, "coef"],
        hazard_ratio = fit_summary$conf.int[, "exp(coef)"],
        conf_low = fit_summary$conf.int[, "lower .95"],
        conf_high = fit_summary$conf.int[, "upper .95"],
        p_value = fit_summary$coefficients[, "Pr(>|z|)"]
      )
    } else {
      tibble::tibble(
        term = character(),
        coefficient = numeric(),
        hazard_ratio = numeric(),
        conf_low = numeric(),
        conf_high = numeric(),
        p_value = numeric()
      )
    }

    zph_capture <- tryCatch(
      capture_warnings(survival::cox.zph(fit)),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )

    ph_table <- tibble::tibble()
    zph_obj <- NULL
    zph_warnings <- character()

    if (!is.null(zph_capture$error)) {
      zph_warnings <- sprintf("PH diagnostics could not be computed: %s", zph_capture$error)
    } else {
      zph_obj <- zph_capture$value
      zph_warnings <- zph_capture$warnings
      ph_table <- tibble::as_tibble(as.data.frame(zph_obj$table), rownames = "term") |>
        dplyr::rename(chisq = chisq, p_value = p)
    }

    forest_capture <- tryCatch(
      capture_warnings(
        survminer::ggforest(
          fit,
          data = prep$data,
          main = "Hazard ratios from Cox proportional hazards model",
          cpositions = c(0.02, 0.22, 0.4),
          fontsize = 0.95,
          refLabel = "Reference",
          noDigits = 2
        )
      ),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )

    forest_plot <- NULL
    forest_warnings <- character()
    if (!is.null(forest_capture$error)) {
      forest_warnings <- sprintf("Forest plot could not be generated: %s", forest_capture$error)
    } else {
      forest_plot <- forest_capture$value
      forest_warnings <- forest_capture$warnings
    }

    list(
      ok = TRUE,
      fit = fit,
      formula_text = formula_text,
      coefficient_table = coef_df,
      concordance = fit_summary$concordance,
      warnings = unique(c(base$warnings, prep$warnings, fit_capture$warnings, zph_warnings, forest_warnings)),
      ph_table = ph_table,
      zph = zph_obj,
      forest_plot = forest_plot,
      epv = prep$epv,
      coefficient_count = prep$coefficient_count,
      prep_data = prep$data,
      reference_levels = prep$reference_levels
    )
  })

  schoenfeld_plot_patchwork <- reactive({
    result <- cox_result()
    req(result$ok, !is.null(result$zph))

    ph_terms <- rownames(result$zph$table)
    ph_terms <- ph_terms[ph_terms != "GLOBAL"]
    req(length(ph_terms) > 0)
    zph_obj <- result$zph

    plot_list <- lapply(seq_along(ph_terms), function(i) {
      term_name <- ph_terms[[i]]
      ggplotify::as.ggplot(function() {
        plot(zph_obj, var = i, main = term_name, resid = TRUE, se = TRUE)
      })
    })

    patchwork::wrap_plots(plot_list, ncol = 2)
  })

  build_results_workbook <- reactive({
    base <- analysis_base()
    result <- cox_result()

    workbook <- list(
      diagnostics = base$filter_summary,
      event_values = base$event_frequency,
      missing_data = base$missing_summary,
      median_survival = median_survival_table_data()
    )

    workbook$logrank <- if (!is.null(base$selected$km_group) && length(base$errors) == 0) {
      logrank_table_data()
    } else {
      tibble::tibble(message = "Log-rank test not available without a KM grouping variable.")
    }

    workbook$cox_results <- if (result$ok) {
      result$coefficient_table
    } else {
      tibble::tibble(message = result$error)
    }

    workbook$cox_model_summary <- if (result$ok) {
      tibble::tibble(
        metric = c("concordance", "concordance_se", "events_per_variable", "estimated_coefficients"),
        value = c(result$concordance[[1]], result$concordance[[2]], result$epv, result$coefficient_count)
      )
    } else {
      tibble::tibble(message = result$error)
    }

    workbook$ph_test <- if (result$ok && nrow(result$ph_table) > 0) {
      result$ph_table
    } else {
      tibble::tibble(message = "PH test not available.")
    }

    workbook
  })

  build_repro_script <- reactive({
    dataset <- current_dataset()
    base <- analysis_base()
    result <- cox_result()

    categorical_vars <- intersect(input$categorical_predictors %||% character(), base$selected$cox_predictors)
    reference_lines <- vapply(
      categorical_vars,
      function(var) {
        ref_id <- paste0("ref_", safe_label(var))
        ref_value <- input[[ref_id]] %||% ""
        sprintf("df$`%s` <- stats::relevel(factor(as.character(df$`%s`)), ref = %s)", var, var, deparse(ref_value))
      },
      character(1)
    )

    source_lines <- if (identical(dataset$source_type, "example")) {
      c(
        "library(survival)",
        sprintf("df <- get(%s, envir = asNamespace('survival'))", deparse(dataset$source_name))
      )
    } else if (identical(dataset$source_type, "upload_csv")) {
      c(
        "library(readr)",
        "data_path <- 'path/to/your_data.csv'",
        "df <- readr::read_csv(data_path, show_col_types = FALSE)"
      )
    } else {
      c(
        "library(readxl)",
        "data_path <- 'path/to/your_data.xlsx'",
        "df <- readxl::read_excel(data_path, sheet = 1)"
      )
    }

    required_cols <- unique(c(
      base$selected$time,
      base$selected$event,
      base$selected$km_group,
      base$selected$km_facet,
      base$selected$cox_predictors,
      base$selected$cox_strata
    ))
    required_cols <- required_cols[!is.null(required_cols) & !is.na(required_cols) & nzchar(required_cols)]

    rhs_text <- build_cox_rhs_text(base$selected$cox_predictors, base$selected$cox_strata)
    km_formula <- if (!is.null(base$selected$km_group)) {
      sprintf("Surv(%s, analysis_event) ~ `%s`", base$selected$time, base$selected$km_group)
    } else {
      sprintf("Surv(%s, analysis_event) ~ 1", base$selected$time)
    }

    metadata_lines <- c(
      sprintf("# Generated from Browser-Based Survival Analysis Studio on %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
      sprintf("# Data source: %s", dataset$source_label),
      sprintf("# Time column: %s", base$selected$time),
      sprintf("# Event column: %s", base$selected$event),
      sprintf("# Event value: %s", base$selected$event_value),
      sprintf("# KM grouping variable: %s", base$selected$km_group %||% "None"),
      sprintf("# KM facet variable: %s", base$selected$km_facet %||% "None"),
      sprintf("# Cox predictors: %s", if (length(base$selected$cox_predictors) > 0) paste(base$selected$cox_predictors, collapse = ", ") else "None"),
      sprintf("# Categorical Cox predictors: %s", if (length(categorical_vars) > 0) paste(categorical_vars, collapse = ", ") else "None"),
      sprintf(
        "# Reference levels: %s",
        if (length(categorical_vars) > 0) {
          paste(vapply(categorical_vars, function(var) sprintf("%s=%s", var, input[[paste0('ref_', safe_label(var))]] %||% ""), character(1)), collapse = "; ")
        } else {
          "None"
        }
      ),
      sprintf("# Cox strata variables: %s", if (length(base$selected$cox_strata) > 0) paste(base$selected$cox_strata, collapse = ", ") else "None"),
      sprintf("# Ties method: %s", base$selected$ties_method)
    )

    script_lines <- c(
      metadata_lines,
      "",
      "library(survival)",
      "library(survminer)",
      source_lines,
      "",
      sprintf("df$`%s` <- suppressWarnings(as.numeric(as.character(df$`%s`)))", base$selected$time, base$selected$time),
      sprintf("df$analysis_event <- ifelse(is.na(df$`%s`), NA_integer_, ifelse(as.character(df$`%s`) == %s, 1L, 0L))", base$selected$event, base$selected$event, deparse(base$selected$event_value)),
      sprintf("required_cols <- c(%s)", paste(sprintf("'%s'", required_cols), collapse = ", ")),
      "analysis_df <- df[stats::complete.cases(df[, required_cols, drop = FALSE]), , drop = FALSE]",
      "analysis_df <- analysis_df[!is.na(analysis_df$analysis_event), , drop = FALSE]",
      sprintf("analysis_df <- analysis_df[!is.na(analysis_df$`%s`) & analysis_df$`%s` > 0, , drop = FALSE]", base$selected$time, base$selected$time)
    )

    if (length(reference_lines) > 0) {
      script_lines <- c(
        script_lines,
        gsub("^df\\$", "analysis_df$", reference_lines)
      )
    }

    script_lines <- c(
      script_lines,
      "",
      sprintf("km_fit <- survival::survfit(stats::as.formula(%s), data = analysis_df)", deparse(km_formula)),
      "median_survival <- survminer::surv_median(km_fit)",
      if (!is.null(base$selected$km_group)) {
        sprintf("logrank <- survival::survdiff(stats::as.formula(%s), data = analysis_df)", deparse(km_formula))
      } else {
        "logrank <- NULL"
      },
      sprintf("cox_formula <- stats::as.formula(%s)", deparse(sprintf("Surv(%s, analysis_event) ~ %s", base$selected$time, rhs_text))),
      sprintf("cox_fit <- survival::coxph(cox_formula, data = analysis_df, ties = %s, model = TRUE, x = TRUE, y = TRUE)", deparse(base$selected$ties_method)),
      "cox_summary <- summary(cox_fit)",
      "ph_test <- survival::cox.zph(cox_fit)",
      "print(km_fit)",
      "print(median_survival)",
      "print(cox_summary)",
      "print(ph_test)"
    )

    if (!is.null(base$selected$km_facet) && !is.null(base$selected$km_group)) {
      script_lines <- c(
        script_lines,
        sprintf(
          "survminer::ggsurvplot_facet(km_fit, data = analysis_df, facet.by = %s, risk.table = TRUE, conf.int = TRUE, pval = TRUE)",
          deparse(base$selected$km_facet)
        )
      )
    } else if (!is.null(base$selected$km_group)) {
      script_lines <- c(
        script_lines,
        "survminer::ggsurvplot(km_fit, data = analysis_df, risk.table = TRUE, conf.int = TRUE, pval = TRUE)"
      )
    } else {
      script_lines <- c(
        script_lines,
        "survminer::ggsurvplot(km_fit, data = analysis_df, risk.table = TRUE, conf.int = TRUE)"
      )
    }

    if (result$ok && nrow(result$coefficient_table) > 0) {
      script_lines <- c(
        script_lines,
        "survminer::ggforest(cox_fit, data = analysis_df)"
      )
    }

    paste(script_lines, collapse = "\n")
  })

  output$snapshot_cards <- renderUI({
    base <- analysis_base()
    result <- cox_result()

    analyzed_rows <- base$filter_summary$count[base$filter_summary$metric == "Rows analyzed"]
    events_n <- if (length(base$errors) == 0) sum(base$analysis_data$analysis_event) else NA_integer_
    concordance_value <- if (result$ok && length(result$concordance) >= 1) sprintf("%.3f", result$concordance[[1]]) else "Unavailable"
    epv_label <- if (is.finite(result$epv)) sprintf("%.1f", result$epv) else "N/A"

    div(
      class = "summary-grid",
      div(class = "metric-card", div(class = "metric-label", "Source"), div(class = "metric-value", base$dataset$source_label), div(class = "metric-sub", sprintf("%s rows, %s columns", nrow(base$raw_data), ncol(base$raw_data)))),
      div(class = "metric-card", div(class = "metric-label", "Rows analyzed"), div(class = "metric-value", analyzed_rows), div(class = "metric-sub", "After missing-data and time filters")),
      div(class = "metric-card", div(class = "metric-label", "Events"), div(class = "metric-value", events_n %||% "N/A"), div(class = "metric-sub", sprintf("Event value mapped from `%s`", base$selected$event_value))),
      div(class = "metric-card", div(class = "metric-label", "C-index"), div(class = "metric-value", concordance_value), div(class = "metric-sub", "From the fitted Cox model")),
      div(class = "metric-card", div(class = "metric-label", "EPV"), div(class = "metric-value", epv_label), div(class = "metric-sub", "Events per estimated Cox coefficient"))
    )
  })

  output$configuration_summary <- renderUI({
    base <- analysis_base()
    categorical_vars <- intersect(input$categorical_predictors %||% character(), base$selected$cox_predictors)
    ref_text <- if (length(categorical_vars) > 0) {
      paste(
        vapply(categorical_vars, function(var) sprintf("%s = %s", var, input[[paste0("ref_", safe_label(var))]] %||% ""), character(1)),
        collapse = "; "
      )
    } else {
      "None"
    }

    tags$dl(
      class = "config-list",
      tags$dt("Time / Event"),
      tags$dd(sprintf("%s / %s == %s", base$selected$time, base$selected$event, base$selected$event_value)),
      tags$dt("KM view"),
      tags$dd(sprintf("Group: %s | Facet: %s", base$selected$km_group %||% "None", base$selected$km_facet %||% "None")),
      tags$dt("Cox predictors"),
      tags$dd(if (length(base$selected$cox_predictors) > 0) paste(base$selected$cox_predictors, collapse = ", ") else "None"),
      tags$dt("Categorical / reference levels"),
      tags$dd(ref_text),
      tags$dt("Cox strata / ties"),
      tags$dd(sprintf("%s | %s", if (length(base$selected$cox_strata) > 0) paste(base$selected$cox_strata, collapse = ", ") else "None", base$selected$ties_method))
    )
  })

  output$data_preview <- renderDT({
    current_dataset()$data |>
      head(50) |>
      DT::datatable(options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  output$validation_messages <- renderUI({
    base <- analysis_base()
    blocks <- list()

    if (length(base$errors) == 0) {
      blocks[[length(blocks) + 1]] <- div(
        class = "status-block",
        strong("Ready to analyze."),
        " The current variable mapping passed the core validation checks."
      )
    } else {
      blocks[[length(blocks) + 1]] <- div(
        class = "status-block error",
        strong("Validation issue."),
        tags$ul(lapply(base$errors, tags$li))
      )
    }

    if (length(base$warnings) > 0) {
      blocks[[length(blocks) + 1]] <- div(
        class = "status-block warn",
        strong("Heads up."),
        tags$ul(lapply(base$warnings, tags$li))
      )
    }

    do.call(tagList, blocks)
  })

  output$overall_km_plot <- renderPlot({
    base <- analysis_base()
    validate(need(length(base$errors) == 0, paste(base$errors, collapse = "\n")))
    print(overall_km_plot_obj())
  }, res = 96)

  output$stratified_km_plot <- renderPlot({
    base <- analysis_base()
    validate(need(length(base$errors) == 0, paste(base$errors, collapse = "\n")))
    validate(need(!is.null(base$selected$km_group), "Select a KM grouping variable to render the stratified Kaplan-Meier plot and log-rank test."))
    print(stratified_km_plot_obj())
  }, res = 96)

  output$median_survival_table <- renderDT({
    base <- analysis_base()
    validate(need(length(base$errors) == 0, paste(base$errors, collapse = "\n")))
    DT::datatable(
      median_survival_table_data(),
      options = list(scrollX = TRUE, dom = "tip"),
      rownames = FALSE
    )
  })

  output$logrank_status <- renderUI({
    base <- analysis_base()
    if (is.null(base$selected$km_group)) {
      return(
        div(
          class = "status-block warn",
          strong("Log-rank test not available."),
          " Choose a KM grouping variable to compare survival curves."
        )
      )
    }

    div(
      class = "status-block",
      strong("Comparing groups defined by "),
      code(base$selected$km_group),
      "."
    )
  })

  output$logrank_table <- renderDT({
    base <- analysis_base()
    validate(need(length(base$errors) == 0, paste(base$errors, collapse = "\n")))
    validate(need(!is.null(base$selected$km_group), "No KM grouping variable selected."))
    DT::datatable(logrank_table_data(), options = list(dom = "tip"), rownames = FALSE)
  })

  output$cox_status <- renderUI({
    result <- cox_result()

    if (!result$ok) {
      return(
        div(
          class = "status-block error",
          strong("Cox model not available."),
          p(result$error, class = "mb-0")
        )
      )
    }

    blocks <- list(
      div(
        class = "status-block",
        strong("Model formula"),
        tags$div(code(result$formula_text)),
        tags$p(
          class = "mb-0 mt-2",
          sprintf(
            "Concordance (C-index): %.3f (SE %.3f)",
            result$concordance[[1]],
            result$concordance[[2]]
          )
        )
      )
    )

    if (length(result$warnings) > 0) {
      blocks[[length(blocks) + 1]] <- div(
        class = "status-block warn",
        strong("Model notes"),
        tags$ul(lapply(result$warnings, tags$li))
      )
    }

    do.call(tagList, blocks)
  })

  output$cox_results_table <- renderDT({
    result <- cox_result()
    validate(need(result$ok, result$error))

    DT::datatable(
      result$coefficient_table,
      options = list(scrollX = TRUE, dom = "tip"),
      rownames = FALSE
    )
  })

  output$forest_plot <- renderPlot({
    result <- cox_result()
    validate(need(result$ok, result$error))
    validate(need(!is.null(result$forest_plot), "Forest plot is unavailable for the current model."))
    print(result$forest_plot)
  }, res = 96)

  output$ph_table <- renderDT({
    result <- cox_result()
    validate(need(result$ok, result$error))
    validate(need(nrow(result$ph_table) > 0, "PH test results are unavailable for the current model."))
    DT::datatable(result$ph_table, options = list(dom = "tip"), rownames = FALSE)
  })

  output$epv_message <- renderUI({
    result <- cox_result()
    base <- analysis_base()
    analyzed_events <- if (length(base$errors) == 0) sum(base$analysis_data$analysis_event) else NA_real_

    if (!is.finite(result$epv)) {
      return(
        div(
          class = "status-block warn",
          strong("EPV unavailable."),
          " Select at least one Cox predictor to estimate events per variable."
        )
      )
    }

    caution <- result$epv < 10
    div(
      class = paste("status-block", if (caution) "warn" else ""),
      strong(sprintf("Events per variable: %.2f", result$epv)),
      p(sprintf("%s analyzed events / %s estimated coefficients.", analyzed_events, result$coefficient_count), class = "mb-0"),
      if (caution) p("This is informational only, but low EPV can make the Cox model unstable or difficult to interpret.", class = "mt-2 mb-0")
    )
  })

  output$schoenfeld_plot_grid <- renderPlot({
    result <- cox_result()
    validate(need(result$ok, result$error))
    validate(need(!is.null(result$zph), "Schoenfeld residual plots are unavailable for the current model."))
    print(schoenfeld_plot_patchwork())
  }, res = 96)

  output$filter_summary_table <- renderDT({
    DT::datatable(analysis_base()$filter_summary, options = list(dom = "tip"), rownames = FALSE)
  })

  output$event_frequency_table <- renderDT({
    DT::datatable(analysis_base()$event_frequency, options = list(dom = "tip"), rownames = FALSE)
  })

  output$missing_data_table <- renderDT({
    DT::datatable(
      analysis_base()$missing_summary,
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE
    )
  })

  output$download_analysis_csv <- downloadHandler(
    filename = function() {
      sprintf("analysis_data_%s.csv", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      base <- analysis_base()
      readr::write_csv(base$analysis_data, file)
    }
  )

  output$download_results_xlsx <- downloadHandler(
    filename = function() {
      sprintf("analysis_results_%s.xlsx", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      writexl::write_xlsx(build_results_workbook(), path = file)
    }
  )

  output$download_km_plot <- downloadHandler(
    filename = function() {
      sprintf("kaplan_meier_%s.png", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      base <- analysis_base()
      plot_obj <- if (!is.null(base$selected$km_group)) stratified_km_plot_obj() else overall_km_plot_obj()
      png(file, width = 1800, height = 1400, res = 180)
      print(plot_obj)
      grDevices::dev.off()
    }
  )

  output$download_forest_plot <- downloadHandler(
    filename = function() {
      sprintf("cox_forest_%s.png", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      result <- cox_result()
      validate(need(result$ok, result$error))
      validate(need(!is.null(result$forest_plot), "Forest plot unavailable."))
      png(file, width = 1800, height = 1200, res = 180)
      print(result$forest_plot)
      grDevices::dev.off()
    }
  )

  output$download_repro_script <- downloadHandler(
    filename = function() {
      sprintf("reproducible_analysis_%s.R", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      writeLines(build_repro_script(), con = file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
