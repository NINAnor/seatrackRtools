# Set a value in a specific cell of master startup

This function updates the value of a specified cell in the
`master_startup` data frame.

## Usage

``` r
set_master_startup_value(master_startup, index, column, value)
```

## Arguments

- master_startup:

  Master starup tibble.

- index:

  Integer. The row index of the cell to update.

- column:

  Character or integer. The column name or index of the cell to update.

- value:

  The new value to assign to the specified cell.

## Value

The updated `master_startup` data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
set_master_startup_value(master_startup, 2, "download_type", "Succesfully downloaded")
} # }
```
