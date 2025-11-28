# Get the path of the master import file

This function constructs a path to the master import file for a given
colony. On finding the path to a colony's master import sheets, it will
be stored in the internal environment for later use.

## Usage

``` r
get_master_import_path(colony, use_stored = TRUE)
```

## Arguments

- colony:

  A character string specifying the name of the colony.

- use_stored:

  If TRUE, use a pre-existing path, rather than searching for a new one.
  Defaults to TRUE.

## Value

A character string representing the path to the master import file.

## Examples

``` r
if (FALSE) { # \dontrun{
get_master_import_folder("ColonyA")
} # }
```
