#' Run the Shiny App
#'
#' @export
#' @concept shiny_app
run_app <- function(log_path = file.path(getwd(), "seatrackRtools_logs"), test = FALSE) {
    app_dir <- system.file("shiny/metadata_app", package = "seatrackRtools")
    if (app_dir == "") stop("Could not find Shiny app directory.", call. = FALSE)
    shiny::shinyOptions(logging_path = log_path, test = test)
    shiny::runApp(app_dir)
}

#' Set up logs for shiny app
#'
#' @export
#' @concept shiny_app
setup_app_logs <- function(silent = FALSE,
                           log_path = NULL,
                           log_names = list(
                               global = paste0("seatrackRtools_log_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log"),
                               error = paste0("seatrackRtools_log_ERROR_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log"),
                               success = paste0("seatrackRtools_log_SUCCESS_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log")
                           )) {

    if (is.null(log_path)) {
        log_path <- getShinyOption("logging_path", "seatrackRtools_logs")
    }

    if (is.null(log_path)) {
        log_path <- getwd()
    }

    if (!dir.exists(log_path)) {
        dir.create(log_path, recursive = TRUE)
    }

    start_logging(log_path, log_names$global, file_safe = TRUE, silent = silent)
    start_logging(log_path, log_names$error, log_level = logger::WARN, log_index = 2, file_safe = TRUE, silent = silent)
    start_logging(log_path, log_names$success, log_level = logger::SUCCESS, log_index = 3, file_safe = TRUE, silent = silent)
    
    # logger::log_errors()
    return(log_names)
}
