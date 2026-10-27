# Function to generate a summary table of field plan data by phase

This function will generate a summary table of field plan data by phase,
including median deployed and retrieved devices, as well as the
proportion of successful deployments. The table will be formatted and
exported as an Excel file.

## Usage

``` r
phase_summary_table(
  all_clean_field_plan,
  export_dir,
  table_name,
  field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
  phases = list(phase_1 = c(2014:2018), phase_2 = c(2019:2022), phase_3 = c(2023:2026)),
  target_species = c("Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
    "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
    "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull",
    "Herring gull", "Lesser black-backed gull", "European shag", "Common eider"),
  target_age = "A",
  target_logger_type = NULL,
  target_colony = NULL,
  body_cell_width = 9
)
```

## Arguments

- all_clean_field_plan:

  Cleaned field plan data, a list of data frames for each year.

- export_dir:

  Directory where the summary table will be exported.

- table_name:

  Name of the summary table to be exported.

- phases:

  List of phases, each containing a vector of years.

- target_species:

  Vector of target species to include in the summary.

- target_age:

  Target age class to filter the data (default is "A" for adults).

- target_logger_type:

  Optional vector of target logger types to filter the data.

- target_colony:

  Optional vector of target colonies to filter the data.

## Value

None. The function will export the summary table as an Excel file in the
specified directory.
