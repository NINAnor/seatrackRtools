# Get SEATRACK data request

This function retrieves SEATRACK data for a specified request, including
position data, morphological and breeding data, and recordings of light,
temperature, and activity. If `export` is TRUE the `export_data_package`
function is used to create a zip file containing the data. This includes
writing each data type as a compressed parquet file and creating a
README file with metadata about the data request. If `export` is FALSE
the function returns a named list of data.frames containing the
requested data. This can be edited and passed to `export_data_package`
later if desired. Requires an active connection to the SEATRACK
database. Currently netCDF files are not included in the data request
package, so these have to be injected using `additional_data_files` if
needed.

## Usage

``` r
data_request(
  request_name,
  data_types = c("raw_data", "GLS_positional_data", "IRMA_positional_data",
    "individual_data", "light", "temperature", "activity", "population_maps",
    "logger_info", "immersion"),
  start_year = "2000",
  end_year = format(Sys.Date(), "%Y"),
  species = NULL,
  colony = NULL,
  age_deployment = "A",
  project = NULL,
  exclude_embargoed = TRUE,
  session_ID = NULL,
  export = TRUE,
  output_dir = NULL,
  additional_notes = "",
  additional_data_files = list(),
  additional_files = list(),
  additional_data = list()
)
```

## Arguments

- request_name:

  A string representing the name of the data request.

- data_types:

  A character vector specifying the types of data to include in the
  request. Possible values are "GLS_positional_data",
  "IRMA_positional_data", "individual_data", "light", "temperature",
  "activity", "population_maps" and "logger_info". Defaults to all
  types.

- start_year:

  An integer representing the start year for the data request.

- end_year:

  An integer representing the end year for the data request. Defaults to
  the current year.

- species:

  An optional string specifying the species to filter the data. If NULL,
  data for all species will be retrieved.

- colony:

  An optional string specifying the colony to filter the data. If NULL,
  data for all colonies will be retrieved.

- age_deployment:

  An optional string specifying the age class to filter the data.
  Possible values are "A" for adults and "C" for juveniles. Defaults to
  "A".

- project:

  An optional string specifying the project to filter the data. Defaults
  to "SEATRACK".

- exclude_embargoed:

  A boolean indicating whether to exclude data that is currently under
  embargo. Defaults to TRUE.

- session_ID:

  Optional list of strings specifying exact session IDs.

- export:

  A boolean indicating whether to export the data package as a zip file.
  If FALSE, the function will return the data as a list instead.

- output_dir:

  An optional string specifying the directory where the exported zip
  file will be saved.

- additional_notes:

  An optional string containing additional notes to be included in the
  README file in the export.

- additional_data_files:

  An optional list of additional data files to include in the data
  directory of the exported zip file. Each element of the list should
  contain the file path to the file to be included and a description.

- additional_files:

  An optional list of additional files to include in the exported zip
  file. Each element of the list should contain the file path to the
  file to be included and a description. If NULL, it will be saved in a
  default location based on the current year.

- additional_data:

  An optional named list of lists containing `data`: a data.frame to be
  included in the export and `description`: A string description. Each
  name corresponds to a data type. This can be used to include data that
  is not directly available through the SEATRACK database, but is
  relevant for the data request.

## Value

If `export` is `TRUE`: None. The function creates a zip file in the
specified output directory. If `export` is `FALSE`: A named list of
data.frames containing the requested data.

## Examples

``` r
if (FALSE) { # \dontrun{
data_request("Mosbech_120925", 2023, 2025, "Common eider", "Christiansø")
} # }
```
