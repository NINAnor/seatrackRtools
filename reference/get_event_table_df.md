# Get event table data frame This function retrieves the data for the event table based on the field plan and database, filtered by the specified event type, logger type, target age, and target species. It processes the data to calculate the number of events, number of sites, and event success rate for each species and LME, and formats it into a data frame suitable for creating the event table.

Get event table data frame This function retrieves the data for the
event table based on the field plan and database, filtered by the
specified event type, logger type, target age, and target species. It
processes the data to calculate the number of events, number of sites,
and event success rate for each species and LME, and formats it into a
data frame suitable for creating the event table.

## Usage

``` r
get_event_table_df(
  current_field_plan,
  target_species,
  event_type = c("deployed", "retrieved"),
  logger_type = c("GLS", "GPS", "GPS-GSM"),
  target_age = c("A", "C")
)
```

## Arguments

- current_field_plan:

  The current field plan data frame containing information about the
  planned and actual events.

- target_species:

  A vector of target species to include in the table.

- event_type:

  The type of event to generate the table for, either "deployed" or
  "retrieved".

- logger_type:

  The type of logger to filter the data by. Either "GLS", "GPS", or
  "GPS-GSM".

- target_age:

  The age class to filter the data by. Either "A" or "C".

## Value

A data frame containing the number of events, number of sites, and event
success rate for each species and LME, formatted for creating the event
table.
