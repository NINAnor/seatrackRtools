# Get session deployments

Function to get deployments associated with a set of logger sessions.
The function will look for deployments that occurred between the session
start time and the session end time (download/shutdown). If the session
is open (no download/shutdown date), the function will use a date far in
the future to indicate the session is still open. If there are
ambiguities (multiple deployments for a single session), those sessions
will be removed and a warning will be logged.

## Usage

``` r
prepare_session_deployments(session_batch, metadata)
```

## Arguments

- session_batch:

  A SessionBatch object containing session information from master
  import startup_shutdown.

- metadata:

  A dataframe containing metadata information for the sessions.

## Value

A list containing a dataframe of deployments and a tibble of sessions.
