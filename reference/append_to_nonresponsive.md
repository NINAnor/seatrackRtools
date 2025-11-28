# Append to nonresponsive list

This function appends to the approrpiate sheet in a list of
nonresponsive sheets. It will check for duplicate logger IDs and ensure
column ordering matches.

## Usage

``` r
append_to_nonresponsive(nonresponsive_list, new_nonresponsive, manufacturer)
```

## Arguments

- nonresponsive_list:

  A list containing tibbles of unresponsive loggers for different
  manufacturers. The name of the list element should match the producer
  name in master_startup (e.g., "Lotek", "MigrateTech"). This can be
  generated with the `load_nonresponsive` function

- new_nonresponsive:

  Tibble containing new rows to be appended.

- manufacturer:

  Character string of name of manufacturer whose nonresponsive sheet is
  to be appended to.

## Value

Modified nonresponsive_list
