# Load nonresponsive logger sheet for current year

This function loads the record of unresponsive loggers. If the filepath
provided does not exist, it initialises new sheets.

## Usage

``` r
load_nonresponsive_sheet(file_path)
```

## Arguments

- file_path:

  String indicating from where the file should be loaded from.

## Value

A LoadedWB containing the unresponsive logger data.
