# Push startups to database

Function to push startups from a master import sheet to the database. It
checks the database for existing startups and only imports new startups
to avoid duplicates.

## Usage

``` r
push_startup(startup)
```

## Arguments

- startup:

  A tibble containing session information from a DBImportCollection
  object.
