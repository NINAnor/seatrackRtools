# Check existing rows in database given certain columns from a metadata dataframe

This function checks for existing rows in a database table based on
specified columns from a metadata dataframe. It constructs a SQL query
to compare the values in the dataframe with those in the database table.
IF a row from the dataframe does not exist in the database table based
on the specified columns, it is marked as missing.

## Usage

``` r
check_db_metadata_import(
  metadata_df,
  table,
  col_names = NULL,
  db_col_names = NULL
)
```

## Arguments

- metadata_df:

  A dataframe containing metadata to be checked against the database.

- table:

  The name of the database table to check against.

- col_names:

  A vector of column names from the metadata dataframe to be used for
  comparison. If NULL, all columns are used.

- db_col_names:

  A vector of column names in the database table corresponding to
  `col_names`. If NULL, `col_names` are used.

## Value

A logical vector indicating which rows in the metadata dataframe do not
exist in the database table.
