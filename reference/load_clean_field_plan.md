# Load and clean field plan data

This function loads a field plan data dataframe from the specified path
using `load_field_plan_sheet`, and then cleans the data using
`get_clean_field_plan`. It returns a cleaned data frame containing the
field plan information for the specified year and target species.

## Usage

``` r
load_clean_field_plan(
  field_plan_path = file.path(the$sea_track_folder, "Admin/Fieldplanning"),
  field_year = c(as.numeric(format(Sys.Date(), "%Y"))),
  all_metadata = NULL,
  target_species = c("Atlantic puffin", "Brünnich's guillemot", "Common guillemot",
    "Little auk", "Razorbill", "Arctic tern", "Black-legged kittiwake", "Great skua",
    "Leach's Storm Petrel", "Northern fulmar", "Northern gannet", "Glaucous gull",
    "Herring gull", "Lesser black-backed gull", "European shag", "Common eider")
)
```

## Arguments

- field_plan_path:

  The file path to the field plan Excel sheet

- field_year:

  The year for which to load the field plan (default is the current
  year)

- all_metadata:

  Optional metadata to be used in cleaning the field plan. If not
  provided, the function will load master sheets from the SEATRACK
  folder.

- target_species:

  A vector of target species to filter the field plan data. Defaults to
  SEATRACK species list.

## Value

A cleaned data frame containing the field plan information
