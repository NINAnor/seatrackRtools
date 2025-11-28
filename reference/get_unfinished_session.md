# Find a logger's unfinished session in the master startup data frame

This function finds the unfinished session for a given logger in the
master startup data frame.

## Usage

``` r
get_unfinished_session(master_startup, logger_id, logger_download_stop_date)
```

## Arguments

- master_startup:

  A data frame containing the master startup and shutdown information.

- logger_id:

  A character string specifying the logger ID.

- logger_download_stop_date:

  A Date object specifying the reported download/stop date of the
  logger.

## Value

A list containing the index of the unfinished session and the session
data frame, or NULL if no unfinished session is found.

## Examples

``` r
if (FALSE) { # \dontrun{
unfinished_session <- get_unfinished_session(master_startup, "Logger123", as.Date("2023-01-15"))
} # }
```
