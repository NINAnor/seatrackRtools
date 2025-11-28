# Check existing deployments in database

Function to check which deployments from the master import sheet are not
already present in the database. It constructs a dataframe of deployment
dates and individ IDs, and uses the `check_db_metadata_import` function
to determine which deployments are new and need to be added to the
database.

## Usage

``` r
check_deployment_db(deployments)
```

## Arguments

- deployments:

  A tibble containing deployment information from master import
  metadata.

## Value

A tibble of deployments that are not already present in the database.
