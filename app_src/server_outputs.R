pdf_download_available <- reactive({
  quarto_typst_available()
})

output$download_repro_pdf_ui <- renderUI({
  if (!pdf_download_available()) {
    return(NULL)
  }

  downloadButton("download_repro_pdf", "Rendered report (.pdf)", class = "btn-outline-primary w-100")
})

output$download_repro_pdf_note <- renderUI({
  if (pdf_download_available()) {
    return(NULL)
  }

  div(
    class = "status-block warn mt-3",
    strong("PDF rendering unavailable here."),
    p(
      "Typst PDF generation requires a local Shiny session with Quarto installed and available on the system path.",
      class = "mb-1"
    ),
    p(
      "Download the portable `.qmd` source report when PDF rendering is unavailable in the current deployment target.",
      class = "mb-0"
    )
  )
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

output$stratified_km_status <- renderUI({
  base <- analysis_base()

  if (is.null(base$selected$km_facet)) {
    return(NULL)
  }

  div(
    class = "status-block warm-compact",
    strong("Faceted view note."),
    " Confidence bands and per-panel p-values are hidden in faceted KM mode to keep the plot stable and readable. Use the separate log-rank table below for inference."
  )
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
    options = list(dom = "tip", pageLength = 8, autoWidth = TRUE),
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
    options = list(dom = "tip", pageLength = 10, autoWidth = TRUE),
    rownames = FALSE
  )
})

forest_plot_height_px <- function(result, min_height = 560) {
  coefficient_rows <- if (isTRUE(result$ok)) nrow(result$coefficient_table) else 0
  max(min_height, 220 + (coefficient_rows * 72))
}

output$forest_plot_ui <- renderUI({
  plotOutput("forest_plot", height = sprintf("%spx", forest_plot_height_px(cox_result())))
})

output$forest_plot <- renderPlot({
  result <- cox_result()
  validate(need(result$ok, result$error))
  validate(need(!is.null(result$forest_plot), "Forest plot is unavailable for the current model."))
  print(result$forest_plot)
},
height = function() {
  forest_plot_height_px(cox_result())
},
res = 96)

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
    options = list(pageLength = 15, autoWidth = TRUE),
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
    png(
      file,
      width = 1800,
      height = ceiling(forest_plot_height_px(result, min_height = 640) * (180 / 96)),
      res = 180
    )
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

output$download_repro_report <- downloadHandler(
  filename = function() {
    sprintf("reproducible_report_%s.qmd", format(Sys.Date(), "%Y%m%d"))
  },
  content = function(file) {
    writeLines(build_repro_qmd(), con = file, useBytes = TRUE)
  }
)

output$download_repro_pdf <- downloadHandler(
  filename = function() {
    sprintf("reproducible_report_%s.pdf", format(Sys.Date(), "%Y%m%d"))
  },
  content = function(file) {
    validate(
      need(
        pdf_download_available(),
        "PDF rendering requires Quarto to be installed on the current host. Download the .qmd report if PDF rendering is unavailable in this deployment."
      )
    )

    quarto_bin <- find_quarto_binary()
    validate(need(nzchar(quarto_bin), "Could not find the Quarto executable on this machine."))

    render_dir <- tempfile("quarto-render-")
    dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(render_dir, recursive = TRUE, force = TRUE), add = TRUE)

    dataset <- current_dataset()
    render_data_path <- NULL

    if (identical(dataset$source_type, "upload_csv")) {
      render_data_path <- "uploaded_data.csv"
      ok <- file.copy(input$data_file$datapath, file.path(render_dir, render_data_path), overwrite = TRUE)
      validate(need(ok, "Failed to stage the uploaded CSV file for PDF rendering."))
    } else if (identical(dataset$source_type, "upload_xlsx")) {
      render_data_path <- "uploaded_data.xlsx"
      ok <- file.copy(input$data_file$datapath, file.path(render_dir, render_data_path), overwrite = TRUE)
      validate(need(ok, "Failed to stage the uploaded Excel file for PDF rendering."))
    }

    qmd_path <- file.path(render_dir, "reproducible_report.qmd")
    pdf_path <- file.path(render_dir, "reproducible_report.pdf")

    writeLines(
      build_repro_qmd_for_render(render_data_path = render_data_path),
      con = qmd_path,
      useBytes = TRUE
    )

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(render_dir)

    render_log <- system2(
      quarto_bin,
      args = c("render", basename(qmd_path), "--to", "typst", "--output", basename(pdf_path)),
      stdout = TRUE,
      stderr = TRUE,
      env = c(sprintf("QUARTO_R=%s", normalizePath(R.home("bin"), winslash = "/", mustWork = TRUE)))
    )

    render_status <- attr(render_log, "status")
    if (!is.null(render_status) && render_status != 0) {
      stop(
        sprintf(
          "Quarto Typst rendering failed%s",
          if (length(render_log) > 0) {
            paste0(": ", paste(render_log, collapse = "\n"))
          } else {
            "."
          }
        )
      )
    }

    validate(need(file.exists(pdf_path), "Quarto completed without creating the expected PDF file."))
    ok <- file.copy(pdf_path, file, overwrite = TRUE)
    validate(need(ok, "Failed to copy the rendered PDF into the download response."))
  }
)
