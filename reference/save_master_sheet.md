# Save a master sheet to an Excel file.

This function writes the provided data frame (`new_master_sheets`) to an
Excel file specified by `filename`.

## Usage

``` r
save_master_sheet(new_master_sheets, filepath = NULL, modified_only = FALSE)
```

## Arguments

- new_master_sheets:

  A LoadedWB object.

- filepath:

  A string specifying the path and name of the Excel file to be created.
  If NULL will be path of the loaded sheet.

- modified_only:

  Only save the file if the sheet has been flagged as modified.

## Value

No return value.

## Examples

``` r
if (FALSE) { # \dontrun{
save_master_sheet(new_master_sheets, "output.xlsx")
} # }
```
