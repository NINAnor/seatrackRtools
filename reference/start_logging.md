# Start logging to a file

This function initializes logging to a specified directory.

## Usage

``` r
start_logging(
  log_dir = "logs",
  log_file = paste0("seatrackRtools_log_", Sys.Date(), ".txt"),
  silent = FALSE,
  log_namespace = "global",
  log_level = logger::INFO,
  file_safe = FALSE
)
```

## Arguments

- log_dir:

  A character string specifying the directory where the log file will be
  saved. If NULL, the log file will be saved in the current working
  directory.

- log_file:

  A character string specifying the name of the log file. Default is
  "seatrack_functions_log.txt".

- silent:

  Boolean. If FALSE, logger will not log that it has start logging.

- log_namespace:

  A character string specifying the namespace for the logger. Default is
  "global".

- log_level:

  The logging level. Default is logger::INFO.

- file_safe:

  Boolean. If TRUE, uses a file-safe appender that handles file writing
  errors gracefully.

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
start_logging("/path/to/log/directory")
} # }
```
