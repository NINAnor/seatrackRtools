#' Run the Shiny App
#'
#' @export
#' @concept shiny_app
run_app <- function(log_path = "app.log") {
    app_dir <- system.file("shiny/metadata_app", package = "seatrackRmetadata")
    if (app_dir == "") stop("Could not find Shiny app directory.", call. = FALSE)
    shiny::shinyOptions(logging_path = log_path)
    shiny::runApp(app_dir)
}
