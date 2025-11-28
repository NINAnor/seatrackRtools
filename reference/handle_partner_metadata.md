# Add partner provided metadata to the master import file

This function adds metadata provided by partners to a master import file
of the appropriate colony. It firstly adds missing sessions by checking
start up files. It then appends the reported encounter data, avoiding
duplicate rows. Finally it updates sessions based on reported logger
returns. This includes generating new sessions for loggers restarted in
the field.

## Usage

``` r
handle_partner_metadata(
  colony,
  new_metadata,
  master_import,
  nonresponsive_list = LoadedWBCollection$new()
)
```

## Arguments

- colony:

  A character string specifying the name of the colony.

- new_metadata:

  List of tibbles, each corresponding to a sheet in the partner provided
  information file.

- master_import:

  List of tibbles, each corresponding to a sheet in the master import
  file.

- nonresponsive_list:

  A named list of tibbles, each containing nonresponsive logger data for
  different manufacturers.

## Value

An updated version of the master import file, as a list where each
element is a sheet from the excel file.
