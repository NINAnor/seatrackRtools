# Set reconstructed download type for nonresponsive loggers

This function checks the master startup sheets for loggers marked as
nonresponsive and updates their download type to "Reconstructed" in both
the master startup sheets and the database.

## Usage

``` r
set_reconstructed(all_master_sheets = NULL)
```

## Arguments

- all_master_sheets:

  Optional list of master startup sheets. If NULL, the function will
  load all master startup sheets using
  [`load_all_master_import()`](https://ninanor.github.io/seatrackRtools/reference/load_all_master_import.md).

## Value

No return value.
