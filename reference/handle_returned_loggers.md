# Handle restarted loggers

This function processes logger return information and updates the master
import data frame accordingly.

## Usage

``` r
handle_returned_loggers(
  colony,
  master_startup,
  logger_returns,
  restart_times,
  nonresponsive_list = list()
)
```

## Arguments

- colony:

  A character string specifying the name of the colony.

- master_startup:

  A data frame containing the master startup and shutdown information.

- logger_returns:

  A data frame containing logger return information.

- restart_times:

  A data frame containing logger restart information.

- nonresponsive_list:

  A list containing tibbles of unresponsive loggers for different
  manufacturers. The name of the list element should match the producer
  name in master_startup (e.g., "Lotek", "MigrateTech").

## Value

A list consisting of two elements:

- \`master_startup“: An updated dataframe containing the modified master
  import data frame.

- `nonresponsive_list`: An updated list containing the modified
  nonresponsive logger data frames.

## Examples

``` r
if (FALSE) { # \dontrun{
updated_master_startup <- handle_returned_loggers(master_startup, logger_returns, restart_times)
} # }
```
