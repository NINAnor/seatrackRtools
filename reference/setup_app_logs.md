# Set up logs for shiny app

Set up logs for shiny app

## Usage

``` r
setup_app_logs(
  silent = FALSE,
  log_path = NULL,
  log_names = list(global = paste0("seatrackRtools_log_", format(Sys.time(),
    "%Y-%m-%d_%H%M%S"), ".txt"), error = paste0("seatrackRtools_log_ERROR_",
    format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".txt"), success =
    paste0("seatrackRtools_log_SUCCESS_", format(Sys.time(), "%Y-%m-%d_%H%M%S"),
    ".txt"))
)
```
