# Check existing active sessions in database

Function to check which startups from the master import sheet correspond
to active sessions in the database. It constructs a dataframe of session
IDs and uses the `check_db_metadata_import` function to determine which
startups are associated with active sessions.

## Usage

``` r
check_startups_active_db(startup_shutdown, filter_startups = FALSE)
```

## Arguments

- startup_shutdown:

  A tibble containing session information from master import
  startup_shutdown.

- filter_startups:

  A logical indicating whether to filter out startups associated with
  active sessions. Default is TRUE.

## Value

If filter_startups is TRUE, a tibble of startups that are associated
with active sessions in the database. Otherwise
