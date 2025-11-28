# Get open session dates Convenience function to get the open date for a set of logger sessions. If the download date is available, use that. If not, use the shutdown date. If both exist, use the later date. If neither are available, use a date far in the future to indicate the session is still open.

Get open session dates Convenience function to get the open date for a
set of logger sessions. If the download date is available, use that. If
not, use the shutdown date. If both exist, use the later date. If
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
