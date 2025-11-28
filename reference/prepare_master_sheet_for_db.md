# Prepare master sheet for database import

This function prepares the master import sheet for database import by
processing the metadata and startup/shutdown information. It checks the
database for existing sessions and categorizes them into sessions to be
closed, opened, or both opened and closed. It then prepares session
batches for database import. Metadata is linked to sessions within these
batches.

## Usage

``` r
prepare_master_sheet_for_db(master_sheets)
```

## Arguments

- master_sheets:

  A LoadedWBCollection object containing the master import sheets.

## Value

A vector of DBImportCollection objects representing the prepared session
batches for database import.
