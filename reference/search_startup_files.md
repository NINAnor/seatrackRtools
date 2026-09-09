# Search for logger instances in startup files

This function searches for logger instances in startup files based on
the provided logger IDs and existing ID-date combinations.

## Usage

``` r
search_startup_files(target_logger_ids, existing_id_date, master_startup)
```

## Arguments

- target_logger_ids:

  A character vector of logger IDs to search for in the startup files.

- existing_id_date:

  A character vector of existing logger ID-date combinations to avoid
  duplicates.

## Value

A tibble containing the logger instances found in the startup files that
are not already present in the existing ID-date combinations.
