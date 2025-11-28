# Check existing retrievals in database

Function to check which retrievals from the master import sheet are not
already present in the database. It constructs a dataframe of retrieval
dates and individ IDs, and uses the `check_db_metadata_import` function
to determine which retrievals are new and need to be added to the
database.

## Usage

``` r
check_retrieval_db(retrievals)
```

## Arguments

- retrievals:

  A tibble containing retrieval information from master import metadata.

## Value

A tibble of retrievals that are not already present in the database.
