# Check existing startups in database

Function to check which startups from the master import sheet are not
already present in the database. It constructs a dataframe of session
IDs and start times, and uses the `check_db_metadata_import` function to
determine which startups are new and need to be added to the database.

## Usage

``` r
check_startups_db(startup_shutdown)
```

## Arguments

- startup_shutdown:

  A tibble containing session information from master import
  startup_shutdown.

## Value

A tibble of startups that are not already present in the database.
