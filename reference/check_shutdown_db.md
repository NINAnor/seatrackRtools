# Check existing shutdowns in database

Function to check which shutdowns from the master import sheet are not
already present in the database. It constructs a dataframe of session
IDs and download dates, and uses the `check_db_metadata_import` function
to determine which shutdowns are new and need to be added to the
database.

## Usage

``` r
check_shutdown_db(startup_shutdown, check_active = FALSE)
```

## Arguments

- startup_shutdown:

  A tibble containing session information from master import
  startup_shutdown.

- check_active:

  A logical indicating whether to first check for active sessions and
  filter out associated startups. Default is FALSE.

## Value

A tibble of shutdowns that are not already present in the database.
