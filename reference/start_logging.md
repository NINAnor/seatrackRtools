# Start logging to a file

This function initializes logging to a specified directory.

## Usage

``` r
start_logging(
  log_dir = NULL,
  log_file = paste0("seatrack_functions_log_", Sys.Date(), ".txt")
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

## Value

None

## Examples

``` r
if (FALSE) { # \dontrun{
start_logging("/path/to/log/directory")
} # }
```
