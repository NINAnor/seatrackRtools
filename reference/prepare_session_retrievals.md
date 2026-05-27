# Get session retrievals

Function to get retrievals associated with a set of logger sessions. The
function will look for retrievals that occurred between the session
start time and the session end time (download/shutdown). If the session
is open (no download/shutdown date), the function will use a date far in
the future to indicate the session is still open. If there are
ambiguities (multiple retrievals for a single session), those sessions
will be removed and a warning will be logged.

## Usage

``` r
prepare_session_retrievals(
  sessions,
  metadata,
  filter_sessions = FALSE,
  report_missing = FALSE,
  duplicate_warnings = TRUE
)
```

## Arguments

- sessions:

  A tibble containing session information from master import
  startup_shutdown.

- metadata:

  A dataframe containing metadata information for the sessions.

- filter_sessions:

  Logical indicating whether to filter out sessions with no retrievals.

- report_missing:

  Logical indicating whether to report retrievals that are not
  associated with any sessions.

- duplicate_warnings:

  Logical indicating whether to report duplicate retrievals

## Value

A list containing a dataframe of retrievals and a tibble of sessions.
