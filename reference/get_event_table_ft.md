# Get event table as flextable This function generates a flextable object for an event table based on the field plan and database. It retrieves the relevant data for the specified event type, logger type, target age, and target species, and formats it into a flextable with appropriate styling and formatting for use in the annual report.

Get event table as flextable This function generates a flextable object
for an event table based on the field plan and database. It retrieves
the relevant data for the specified event type, logger type, target age,
and target species, and formats it into a flextable with appropriate
styling and formatting for use in the annual report.

## Usage

``` r
get_event_table_ft(
  current_field_plan,
  name,
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

- name:

  The name of the event table to be generated, used for labeling the
  table.

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

A flextable object representing the event table.
