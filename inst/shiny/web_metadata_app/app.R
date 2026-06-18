library(shiny)
library(ggplot2)
library(dplyr)

source("main_server.R")
source("main_ui.R")
source("data_request_summary.R")
source("pos_summary.R")
source("age_summary.R")
source("sex_summary.R")

shinyApp(
    ui = main_ui("main"),
    server = function(input, output, session) {
        main_server("main")
    }
)
