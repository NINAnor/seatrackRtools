# Find logger instances in files

This function tries to find a logger ID in all master import files. You
can either provide an already loaded list of master import sheets, or
the function will load them itself.

## Usage

``` r
get_logger_from_metadata(logger_id, all_master_import_list = NULL)
```

## Arguments

- logger_id:

  logger ID of desired logger

- all_master_import_list:

  list of master import sheets, as returned by
  `load_all_master_import(combine = FALSE)`. If not provided, this will
  be generated inside the function

## Value

list of all matches, where each element of the list is a list containing
the row of data, the file path and the index.
