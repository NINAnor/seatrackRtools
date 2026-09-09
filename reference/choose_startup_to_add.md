# Choose the appropriate startup row to add to the master startup data frame

This function selects the most appropriate startup row from the provided
startup rows based on the logger's deployment date and other criteria.
It handles cases where multiple startup rows exist for a logger and
ensures that the selected row is suitable for adding to the master
startup data frame.

## Usage

``` r
choose_startup_to_add(
  logger_partner_logger_data,
  all_startups,
  master_import,
  can_dummy_models = NULL
)
```

## Arguments

- logger_partner_logger_data:

  A data frame containing logger information from the partner metadata,
  including deployment and retrieval dates.

- all_startups:

  A data frame containing all startup rows for the logger, filtered from
  the startup files.

- master_import:

  The loaded master import object.

- can_dummy_models:

  Optional data frame of models that can be used to create dummy start
  times. If NULL, the function will retrieve the list of models from the
  database.

## Value

A single startup row that is deemed appropriate for adding to the master
startup data frame, or NULL if no suitable row is found.
