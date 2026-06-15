# Set up logs for shiny app This function initializes logging for the Shiny app, creating separate log files for general logs, errors, and successes. It ensures that the specified log directory exists and starts logging with appropriate settings for each log type.

Set up logs for shiny app This function initializes logging for the
Shiny app, creating separate log files for general logs, errors, and
successes. It ensures that the specified log directory exists and starts
logging with appropriate settings for each log type.

## Usage

``` r
setup_app_logs(
  silent = FALSE,
  log_path = NULL,
  log_names = list(global = paste0("seatrackRtools_log_", format(Sys.time(),
    "%Y-%m-%d_%H%M%S"), ".log"), error = paste0("seatrackRtools_log_ERROR_",
    format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log"), success =
    paste0("seatrackRtools_log_SUCCESS_", format(Sys.time(), "%Y-%m-%d_%H%M%S"),
    ".log"))
)
```

## Arguments

- silent:

  A boolean indicating whether to start logs silently.

- log_path:

  A string specifying the path to the directory where log files should
  be stored. If NULL, it defaults to a
  "seatrackRtools_app/seatrackRtools_logs" directory in the current
  working directory.

- log_names:

  A list of strings specifying the names of the log files for general
  logs, errors, and successes. Each name should include a timestamp to
  ensure uniqueness. Defaults to a list with names
  "seatrackRtools_log\_.log", "seatrackRtools_log_ERROR\_.log", and
  "seatrackRtools_log_SUCCESS\_.log".

## Value

A list of log file names that were set up for the app.
