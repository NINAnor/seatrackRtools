# Convert a dataframe of R values to a string of database values for SQL insertion

This function takes a dataframe of R values and converts each value to
its corresponding database representation. It then constructs a string
of database values formatted for SQL insertion.

## Usage

``` r
R_df_to_db_values(db_cols, db_types = rep("", length(db_cols)))
```

## Arguments

- db_cols:

  A dataframe containing the columns to be converted.

- db_types:

  A named vector specifying the database types for each column. Default
  is an empty string for each column.

## Value

A string of database values formatted for SQL insertion.
