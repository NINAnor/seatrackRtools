# Push multiple database import collections to database

Function to push multiple DBImportCollection objects to the database.
Each collection is processed in turn using the
`push_db_import_collection` function.

## Usage

``` r
push_db_import_collections(db_import_collections, dry_run = FALSE)
```

## Arguments

- db_import_collections:

  A list of DBImportCollection objects.

- dry_run:

  Logical indicating whether to perform a dry run (default is FALSE). If
  TRUE, no data will be written to the database.
