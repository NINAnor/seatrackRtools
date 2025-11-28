# Create a formatted flextable for the appendix table

This function generates a flextable summarizing deployment and retrieval
data for specified logger types and age classes. It includes background
coloring based on success rates and organizes species into groups with
distinct colors.

## Usage

``` r
create_appendix_table(
  field_plan_clean,
  logger_type,
  age_target,
  target_species
)
```

## Arguments

- field_plan_clean:

  Data frame containing the cleaned field plan data

- logger_type:

  Type of logger to filter by (e.g., "GLS", "GPS", "GSM")

- age_target:

  Age class to filter by (e.g., "A" for adult, "C" for chick)

- target_species:

  Vector of species names to include in the table

## Value

A formatted flextable object

## Examples

``` r
if (FALSE) { # \dontrun{
create_appendix_table(field_plan_clean, "GLS", "A", target_species)
} # }
```
