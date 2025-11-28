# Get historical deployment and retrieval data

This function retrieves historical deployment and retrieval data from
the SEATRACK database for a specified year and event type (Deployment or
Retrieval). It filters the data based on the event type, age class, and
logger type, and returns a data frame with relevant information. A
connection to the SEATRACK database is required. This should probably be
moved to seatrackRdb at some point.

## Usage

``` r
get_history_table(history_year, event_type = c("Deployment", "Retrieval"))
```

## Arguments

- history_year:

  The year for which to retrieve historical data (e.g., 2023)

- event_type:

  A vector specifying the event type(s) to filter by (defaults to
  c("Deployment", "Retrieval"))

## Value

A data frame containing historical deployment and retrieval data

## Examples

``` r
if (FALSE) { # \dontrun{
seatrackR::connectSeatrack()
history_data <- get_history_table(2023, c("Deployment", "Retrieval"))
} # }
```
