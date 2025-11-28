# Push deployments to database

Function to push deployments from a master import sheet to the database.
It checks the database for existing deployments and only imports new
deployments to avoid duplicates. It also checks that the loggers being
deployed have an associated session in the database.

## Usage

``` r
push_deployments(deployments)
```

## Arguments

- deployments:

  A tibble containing deployment information from a DBImportCollection
  object.
