# Load master import file for a given colony

This function loads the master import file for a specified colony or
directly from a file path. Either a colony name of a file path must be
provided. It iterates through the appropriate sheets and combines the
data into a list of data frames.

## Usage

``` r
load_master_import(colony = NULL, file_path = NULL, use_stored = TRUE)
```

## Arguments

- colony:

  A character string specifying the name of the colony.

- file_path:

  File path of master import file

- use_stored:

  If TRUE, use a pre-existing path, rather than searching for a new one.
  Defaults to TRUE.

## Value

A LoadedWB object.

## Examples

``` r
if (FALSE) { # \dontrun{
load_master_import("ColonyA")
} # }
```
