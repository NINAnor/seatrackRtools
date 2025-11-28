# Load multiple nonresponsive logger sheets

This function loads nonresponsive logger sheets for multiple file paths
and manufacturers.

## Usage

``` r
load_nonresponsive(file_paths, manufacturers)
```

## Arguments

- file_paths:

  A character vector of file paths to load.

- manufacturers:

  A character vector of manufacturers, same length as file_paths.

## Value

A named list of tibbles, each containing nonresponsive logger data for
the corresponding manufacturer.

## Examples

``` r
if (FALSE) { # \dontrun{
file_paths <- c("lotek.xlsx", "migratetech.xlsx")
manufacturers <- c("Lotek", "Migrate Technology")
nonresponsive_list <- load_nonresponsive(file_paths, manufacturers)
} # }
```
