library(shiny)
library(bslib)
library(shinyFiles)

library(shinybusy)
library(promises)
library(future)

source("main_ui.R")
source("main_server.R")
source("folder_selector.R")
source("manage_loggers.R")
source("log_display.R")
source("manage_metadata.R")
source("manage_partner_metadata.R")
source("manage_db_upload.R")
source("manage_db_upload_metadata.R")
source("manage_db_upload_recordings.R")
source("mod_dt_tabs.R")
source("connect_db.R")

plan(multisession)

library(logger)
readRenviron(".Renviron")
library(seatrackRtools)

setup_app_settings <- function(settings_path = NULL) {
    if (is.null(settings_path)) {
        settings_path <- getShinyOption("settings_path", "seatrackRtools_app")
    }
    if (is.null(settings_path)) {
        settings_path <- getwd()
    }
    if (!dir.exists(settings_path)) {
        dir.create(settings_path, recursive = TRUE)
    }
    return(settings_path)
}

setup_app_settings()
app_log_names <- seatrackRtools:::setup_app_logs()
shiny::shinyOptions(app_log_names = app_log_names)
shinyApp(
    ui = main_ui("main"),
    server = function(input, output, session) {
        # bs_themer()
        main_server("main")
    }
)

