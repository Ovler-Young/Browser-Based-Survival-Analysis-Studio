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

source(file.path("app_src", "helpers.R"))
source(file.path("app_src", "data_config.R"))
source(file.path("app_src", "ui.R"))

server <- function(input, output, session) {
  sys.source(file.path("app_src", "server_analysis.R"), envir = environment())
  sys.source(file.path("app_src", "server_outputs.R"), envir = environment())
}

shinyApp(ui, server)
