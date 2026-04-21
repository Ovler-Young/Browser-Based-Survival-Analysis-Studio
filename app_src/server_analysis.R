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
        conf.int = FALSE,
        pval = FALSE,
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
        paste(vapply(categorical_vars, function(var) sprintf("%s=%s", var, input[[paste0("ref_", safe_label(var))]] %||% ""), character(1)), collapse = "; ")
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
        "survminer::ggsurvplot_facet(km_fit, data = analysis_df, facet.by = %s, risk.table = TRUE, conf.int = FALSE, pval = FALSE)",
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

build_repro_qmd <- reactive({
  dataset <- current_dataset()
  base <- analysis_base()
  generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  script_lines <- strsplit(build_repro_script(), "\n", fixed = TRUE)[[1]]

  yaml_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  reference_text <- if (length(intersect(input$categorical_predictors %||% character(), base$selected$cox_predictors)) > 0) {
    paste(
      vapply(
        intersect(input$categorical_predictors %||% character(), base$selected$cox_predictors),
        function(var) sprintf("%s = %s", var, input[[paste0("ref_", safe_label(var))]] %||% ""),
        character(1)
      ),
      collapse = "; "
    )
  } else {
    "None"
  }

  qmd_lines <- c(
    "---",
    paste0("title: ", yaml_quote("Reproducible Survival Analysis Report")),
    paste0("subtitle: ", yaml_quote("Generated by Browser-Based Survival Analysis Studio")),
    paste0("date: ", yaml_quote(generated_at)),
    "format:",
    "  typst:",
    "    toc: true",
    "execute:",
    "  echo: true",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "# Overview",
    "",
    "This report captures the current analysis selections from Browser-Based Survival Analysis Studio and is ready to render with Quarto's Typst output.",
    "",
    "## Configuration",
    "",
    paste0("- Data source: ", dataset$source_label),
    paste0("- Time column: `", base$selected$time, "`"),
    paste0("- Event column: `", base$selected$event, "` with event value `", base$selected$event_value, "`"),
    paste0("- KM grouping variable: ", base$selected$km_group %||% "None"),
    paste0("- KM facet variable: ", base$selected$km_facet %||% "None"),
    paste0(
      "- Cox predictors: ",
      if (length(base$selected$cox_predictors) > 0) {
        paste(sprintf("`%s`", base$selected$cox_predictors), collapse = ", ")
      } else {
        "None"
      }
    ),
    paste0(
      "- Categorical Cox predictors: ",
      if (length(intersect(input$categorical_predictors %||% character(), base$selected$cox_predictors)) > 0) {
        paste(sprintf("`%s`", intersect(input$categorical_predictors %||% character(), base$selected$cox_predictors)), collapse = ", ")
      } else {
        "None"
      }
    ),
    paste0("- Reference levels: ", reference_text),
    paste0(
      "- Cox strata variables: ",
      if (length(base$selected$cox_strata) > 0) {
        paste(sprintf("`%s`", base$selected$cox_strata), collapse = ", ")
      } else {
        "None"
      }
    ),
    paste0("- Ties method: `", base$selected$ties_method, "`"),
    "",
    "# Analysis",
    "",
    "```{r analysis}",
    script_lines,
    "```"
  )

  paste(qmd_lines, collapse = "\n")
})
