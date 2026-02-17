ReadLastLines <- function(x, n, ...) {
    if (!file.exists(x)) {
        return("Waiting for logging file..")
    }
    con <- file(x)
    open(con)
    out <- scan(con, n, what = "char(0)", sep = "\n", quiet = TRUE, ...)

    while (TRUE) {
        tmp <- scan(con, 1, what = "char(0)", sep = "\n", quiet = TRUE)
        if (length(tmp) == 0) {
            close(con)
            break
        }
        out <- c(out[-1], tmp)
    }
    out
}

color_log_lines <- function(lines) {
    prev_class <- "log-info"
    all_html <- c()

    for (line in lines) {
        cls <- dplyr::case_when(
            grepl("\\bINFO\\b", line) ~ "log-info",
            grepl("\\bSUCCESS\\b", line) ~ "log-success",
            grepl("\\bWARN\\b", line) ~ "log-warn",
            grepl("\\bERROR\\b", line) ~ "log-error",
            TRUE ~ NA
        )
        if (is.na(cls)) {
            cls <- prev_class
        }
        prev_class <- cls

        all_html <- c(all_html, sprintf("<div class='%s'>%s</div>", cls, htmltools::htmlEscape(line)))
    }
    return(all_html)
}

logger_dependency <- function() {
    htmltools::htmlDependency(
        name = "logger",
        version = "1.0.0",
        src = c(file = "www"),
        stylesheet = "logger_styles.css"
    )
}

logger_ui <- function(id) {
    ns <- NS(id)

    tagList(
        logger_dependency(),
        uiOutput(ns("logpath")),
        div(
            class = "log-container",
            uiOutput(ns("log"))
        )
    )
}

logger_server <- function(id, line_n = NULL, interval_millis = NULL) {
    moduleServer(id, function(input, output, session) {
        if (is.null(line_n)) {
            line_n <- reactiveVal(5)
        }
        if (is.null(interval_millis)) {
            interval_millis <- reactiveVal(1000)
        }


        log_dir <- getShinyOption("logging_path", "seatrackRtools_logs")
        log_names <- getShinyOption("app_log_names")
        log_path <- file.path(log_dir, log_names$global)

        log_file <- reactiveFileReader(
            session = session,
            filePath = log_path,
            intervalMillis = interval_millis,
            readFunc = function(filepath) {
                lines <- ReadLastLines(filepath, line_n())
                HTML(paste(color_log_lines(lines), collapse = ""))
            }
        )

        output$logpath <- renderUI(h4(
            HTML(
                paste(
                    "Log", em(log_names$global)
                )
            )
        ))
        output$log <- renderUI(log_file())
    })
}
