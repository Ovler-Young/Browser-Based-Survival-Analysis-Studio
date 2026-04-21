ui <- page_sidebar(
  title = div(
    class = "app-title-wrap",
    h2("Survival Analysis Studio"),
    p(
      class = "lead-text",
      "Kaplan-Meier curves, log-rank tests, and Cox models in a guided workflow that runs in GitHub Pages directly in your browser."
    )
  ),
  theme = bs_theme(
    version = 5,
    bg = "#f6f1e8",
    fg = "#1d2428",
    primary = "#1e5f74",
    secondary = "#d8b26e",
    success = "#3b7a57",
    base_font = font_collection("system-ui", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = font_collection("Avenir Next", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
    code_font = font_collection("ui-monospace", "SFMono-Regular", "SF Mono", "Consolas", "Liberation Mono", "Menlo", "monospace")
  ),
  sidebar = sidebar(
    width = 390,
    div(
      class = "sidebar-note",
      strong("Deployment note."),
      " This app is published as a browser-based Shinylive build, so analysis runs on your own machine rather than on a hosted Shiny server."
    ),
    div(
      class = "sidebar-note warm",
      strong("Startup tip."),
      " The GitHub Pages browser build may take 10-20 seconds on first load while webR packages download and cache."
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
    downloadButton("download_repro_script", "Reproducible script (.R)", class = "btn-outline-primary w-100"),
    br(),
    br(),
    downloadButton("download_repro_report", "Reproducible report (.qmd)", class = "btn-outline-primary w-100"),
    br(),
    br(),
    uiOutput("download_repro_pdf_ui"),
    uiOutput("download_repro_pdf_note")
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
          uiOutput("stratified_km_status"),
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
          uiOutput("forest_plot_ui")
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
      font-family: 'Avenir Next', 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
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
    .status-block.warm-compact {
      background: rgba(216,178,110,0.12);
      border: 1px solid rgba(216,178,110,0.32);
      padding: 0.7rem 0.9rem;
      margin-bottom: 0.8rem;
    }
    .status-block.error {
      background: rgba(179,67,54,0.12);
      border-color: rgba(179,67,54,0.28);
    }
    .bslib-card {
      height: auto !important;
      max-height: none !important;
      overflow: visible !important;
    }
    .bslib-card .card-body,
    .accordion-body {
      flex: 0 0 auto;
      height: auto !important;
      max-height: none !important;
      overflow: visible !important;
    }
    .shiny-datatable {
      overflow-x: auto;
    }
  "))
)
