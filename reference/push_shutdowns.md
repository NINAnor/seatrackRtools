# Push shutdowns to database

Function to push shutdowns from a master import sheet to the database.
It checks the database for existing shutdowns and only imports new
shutdowns to avoid duplicates. It also checks that the loggers being
shutdown have an associated retrieval in the database if they have a
deployment. It also checks that loggers without deployments are not
being shutdown if they are marked as 'Successfully downloaded' or
'Nonresponsive'.

## Usage

``` r
push_shutdowns(shutdown)
```

## Arguments

- shutdown:

  A tibble containing session information from a DBImportCollection
  object.
