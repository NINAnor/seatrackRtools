# Export a data request package

This function will create a zip file of seatrack data. This includes:

- Writing each data type as a compressed parquet file

- Creating a README file with metadata about the data request

## Usage

``` r
export_data_package(
  data_request_result = NULL,
  all_data = NULL,
  request_name = NULL,
  output_dir = NULL,
  species = NULL,
  colony = NULL,
  times = NULL,
  additional_notes = "",
  additional_data_files = list(),
  additional_files = list()
)
```

## Arguments

- data_request_result:

  List containing the result of a data request, which will contain all
  the arguments needed for the export. If this is provided, other
  arguments are ignored.

- all_data:

  A named list of lists containing `data`: a data.frame to be exported
  and `description`: A string description. Each name corresponds to a
  data type.

- request_name:

  A string representing the name of the data request.

- output_dir:

  An optional string specifying the directory where the zip file will be
  saved. Defaults to `requested_data_packages/<current_year>`.

- species:

  A character vector of species included in the data request. If NULL,
  it will be inferred from the data.

- colony:

  A character vector of colonies included in the data request. If NULL,
  it will be inferred from the data.

- times:

  A vector of two dates representing the start and end of the data
  request. If NULL, it will be inferred from the data.

- additional_notes:

  An optional string containing additional notes to be included in the
  README file.

- additional_data_files:

  An optional list of additional data files to include in the data
  directory of the exported zip file. Each element of the list should
  contain the file path to the file to be included and a description.

- additional_files:

  An optional list of additional files to include in the base directory
  of the exported zip file. Each element of the list should contain the
  file path to the file to be included and a description. If NULL, it
  will be saved in a default location based on the current year.

## Value

None. The function creates a zip file in the specified output directory.
