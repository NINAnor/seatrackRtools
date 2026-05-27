# Push master import file to database

Function to push a master import file to the database. The function
loads the master import file, prepares the data for database import, and
then pushes the data to the database using the
`push_db_import_collections` function.

## Usage

``` r
push_master_import_file_to_db(
  colony = NULL,
  file_path = NULL,
  master_sheets = NULL,
  dry_run = FALSE
)
```

## Arguments

- colony:

  Optional colony name to filter the master import file.

- file_path:

  Optional file path to a master import Excel file.

- master_sheets:

  Optional list of master import sheets. If not provided, the function
  will load the master import file using the provided colony and
  file_path.

- dry_run:

  Logical indicating whether to perform a dry run (default is FALSE). If
  TRUE, no data will be written to the database.
