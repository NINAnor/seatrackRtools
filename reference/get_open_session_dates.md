# Get open session dates Convenience function to get the open date for a set of logger sessions. If the shutdown date is available, use that. If not, use the download date. If both exist, use the earlier date. If there is another session from the same logger, use the start of that. If neither are available, use a date far in the future to indicate the session is still open.

Get open session dates Convenience function to get the open date for a
set of logger sessions. If the shutdown date is available, use that. If
not, use the download date. If both exist, use the earlier date. If
there is another session from the same logger, use the start of that. If
neither are available, use a date far in the future to indicate the
session is still open.

## Usage

``` r
get_open_session_dates(sessions)
```

## Arguments

- sessions:

  A tibble containing session information from master import
  startup_shutdown.

## Value

A vector of POSIXct dates representing the open dates for the sessions.
