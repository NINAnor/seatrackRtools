#' Run the Shiny App
#'
#' This function launches the Shiny application for seatrackRtools. It sets up the necessary options for the app, including paths for settings and logs, and then runs the app from the specified directory within the package.
#' @param settings_path A string specifying the path to the settings directory for the app. Defaults to a "seatrackRtools_app" directory in the current working directory.
#' @param log_path A string specifying the path to the logs directory for the app. Defaults to a "logs" directory within the "seatrackRtools_app" directory in the current working directory.
#' @param test A boolean indicating whether to run the app in test mode. Defaults to FALSE. If TRUE, the app will use the test database.
#' @return None. This function launches the Shiny app and does not return a value.
#' @export
#' @concept shiny_app
run_app <- function(settings_path = file.path(getwd(), "seatrackRtools_app"), log_path = file.path(getwd(), "seatrackRtools_app", "logs"), test = FALSE) {
    # settings_path

    app_dir <- system.file("shiny/metadata_app", package = "seatrackRtools")
    if (app_dir == "") stop("Could not find Shiny app directory.", call. = FALSE)
    shiny::shinyOptions(settings_path = settings_path, logging_path = log_path, test = test)
    shiny::runApp(app_dir)
}



#' Set up logs for shiny app
#' This function initializes logging for the Shiny app, creating separate log files for general logs, errors, and successes. It ensures that the specified log directory exists and starts logging with appropriate settings for each log type.
#'
#' @param silent A boolean indicating whether to start logs silently.
#' @param log_path A string specifying the path to the directory where log files should be stored. If NULL, it defaults to a "seatrackRtools_app/seatrackRtools_logs" directory in the current working directory.
#' @param log_names A list of strings specifying the names of the log files for general logs, errors, and successes. Each name should include a timestamp to ensure uniqueness. Defaults to a list with names "seatrackRtools_log_<timestamp>.log", "seatrackRtools_log_ERROR_<timestamp>.log", and "seatrackRtools_log_SUCCESS_<timestamp>.log".
#' @return A list of log file names that were set up for the app.
#' @export
#' @concept shiny_app
#' @keywords internal
setup_app_logs <- function(silent = FALSE,
                           log_path = NULL,
                           log_names = list(
                               global = paste0("seatrackRtools_log_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log"),
                               error = paste0("seatrackRtools_log_ERROR_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log"),
                               success = paste0("seatrackRtools_log_SUCCESS_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log")
                           )) {
    if (is.null(log_path)) {
        log_path <- getShinyOption("logging_path", file.path("seatrackRtools_app", "seatrackRtools_logs"))
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
