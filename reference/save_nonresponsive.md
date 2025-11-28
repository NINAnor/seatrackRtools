# Save multiple nonresponsive logger sheets to Excel files

This function iterates through a list of nonresponsive logger sheets and
a vector of file paths, saving each sheet to its corresponding file
path.

## Usage

``` r
save_nonresponsive(file_paths, nonresponsive_list)
```

## Arguments

- file_paths:

  A character vector of file paths to save each sheet.

- nonresponsive_list:

  A named list of tibbles, each containing nonresponsive logger data.

## Value

No return value.

## Examples

``` r
if (FALSE) { # \dontrun{
save_multiple_nonresponsive(nonresponsive_list, file_paths)
} # }
```
