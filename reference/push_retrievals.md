# Push retrievals to database Function to push retrievals from a master import sheet to the database. It checks the database for existing retrievals and only imports new retrievals to avoid duplicates. It also checks that the loggers being retrieved have an associated deployment in the database.

Push retrievals to database

Function to push retrievals from a master import sheet to the database.
It checks the database for existing retrievals and only imports new
retrievals to avoid duplicates. It also checks that the loggers being
retrieved have an associated deployment in the database.

## Usage

``` r
push_retrievals(retrievals)
```

## Arguments

- retrievals:

  A tibble containing retrieval information from a DBImportCollection
  object.
