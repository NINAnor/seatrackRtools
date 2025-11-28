# Load partner provided metadata from an Excel file

This function reads metadata provided by partners from an Excel file. It
iterates through the appropriate sheets and combines the data into a
list of data frames.

## Usage

``` r
load_partner_metadata(file_path)
```

## Arguments

- file_path:

  A character string specifying the absolute path to the Excel file.

## Value

A list of data frames, each corresponding to a sheet in the Excel file.

## Examples

``` r
if (FALSE) { # \dontrun{
load_partner_metadata("path/to/partner_metadata.xlsx")
} # }
```
